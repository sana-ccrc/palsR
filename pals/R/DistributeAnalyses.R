# Functions to distribute analyses, esp when multiple cores used
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)

DistributeSingleSiteAnalyses = function(Analysis,data,vars){
	
	# These will be metrics passed 
	metrics = list(nme = 0, rmse=0,correlation=1)
	
	# Create outfilename:
	outfile = setOutput('ModelAnalysis')
	
	# First deal with multiple-variable analyses:
	if(Analysis$type == 'Conserve'){
	# No single variable, so 'multiple' returned in javascript:
	varname = 'multiple'
	# Analysis identifier for javascript:
	outfiletype = Analysis$type
	
	}else if(Analysis$type == 'EvapFrac'){
	# No single variable, so 'multiple' returned in javascript:
	varname = 'multiple'
	# Analysis identifier for javascript:
	outfiletype = Analysis$type
	
	
	}else{ # Analysis will be for a single variable.
		# The name of this variable
		varname = vars[[Analysis$vindex]][['Name']][1]
		# Units expression:
		unitstxt = vars[[Analysis$vindex]][['UnitsText']]
		# Longer variable name for plots:
		longvarname = vars[[Analysis$vindex]][['PlotName']]
		# File name for graphics file:
		filestring = paste(getwd(),outfile,sep = "/")
		# Analysis identifier for javascript:
		outfiletype = paste(varname,tolower(Analysis$type))
		
		# Check for obs or model aren't missing variable data:
		errcheck = CanAnalysisProceed(data[[Analysis$vindex]]$obs$err,
			data[[Analysis$vindex]]$obs$errtext,data[[Analysis$vindex]]$model$err,
			data[[Analysis$vindex]]$model$errtext)
		# Don't proceed and report error if there's an issue:		
		if( ! errcheck$proceed){
			result = list(type=outfiletype,filename=filestring,mimetype="image/png",
				error=errcheck$errtext,bencherror=data[[Analysis$vindex]]$bench$errtext)
			return(result)
		}		
		# Check model, obs timing consistency
		tcheck = CheckTiming(data[[Analysis$vindex]]$model$timing,
			data[[Analysis$vindex]]$obs$timing)	
		# Don't proceed and report error if there's an issue:	
		if(tcheck$err){
			result = list(type=outfiletype,filename=filestring,mimetype="image/png",
				error=tcheck$errtext,bencherror=data[[Analysis$vindex]]$bench$errtext)
			return(result)
		}
		
		# Test benchmark timing compatibility:
		if(data[[Analysis$vindex]]$bench$exist){
			for(b in 1: (data[[Analysis$vindex]]$bench$howmany)){
				tcheck = CheckTiming(data[[Analysis$vindex]]$bench[[data[[Analysis$vindex]]$bench$index[b]]]$timing,
					data[[Analysis$vindex]]$obs$timing)
				if(tcheck$err){
					# Report error with benchmark
					data[[Analysis$vindex]]$bench$errtext = 
						paste(data[[Analysis$vindex]]$bench$errtext,tcheck$errtext)
					# Remove benchmark from benchmark list:
					data[[Analysis$vindex]]$bench$howmany = data[[Analysis$vindex]]$bench$howmany - 1
					if(data[[Analysis$vindex]]$bench$howmany == 0){
						# If that was the only benchmark, note there no longer any:
						data[[Analysis$vindex]]$bench$exist = FALSE
					}else{
						# Change index of appropriate benchmarks:
						oldlength = length(data[[Analysis$vindex]]$bench$index)
						if(b==1){
							data[[Analysis$vindex]]$bench$index = data[[Analysis$vindex]]$bench$index[2:oldlength]
						}else if(b==oldlength){	
							data[[Analysis$vindex]]$bench$index = data[[Analysis$vindex]]$bench$index[1:(oldlength-1)]
						}else{
							data[[Analysis$vindex]]$bench$index = 
								c(data[[Analysis$vindex]]$bench$index[1:(b-1)],
								data[[Analysis$vindex]]$bench$index[(b+1):oldlength])
						}
					}	
					
				}
			}
		}
		
		# Create data matrix to send to analysis function:
		adata=matrix(NA,length(data[[Analysis$vindex]]$obs$data),(2+data[[Analysis$vindex]]$bench$howmany))
		adata[,1] = data[[Analysis$vindex]]$obs$data
		adata[,2] = data[[Analysis$vindex]]$model$data
		# Add benchmark data, if any:
		if(data[[Analysis$vindex]]$bench$exist){
			for(b in 1: (data[[Analysis$vindex]]$bench$howmany) ){
				adata[,(b+2)] = data[[Analysis$vindex]]$bench[[ data[[Analysis$vindex]]$bench$index[b] ]]$data
			}
		}
		
		# Add obs quality control data, if present:		
		if(data[[Analysis$vindex]]$obs$qcexists){
			vqcdata = matrix(NA,length(data[[Analysis$vindex]]$obs$data),1)
			vqcdata[,1] = data[[Analysis$vindex]]$obs$qc
		}else{
			vqcdata = matrix(-1,nrow=1,ncol=1)
		}
		
		# For adding to plots:
		obsname = data[[Analysis$vindex]]$obs$name
		moname = data[[Analysis$vindex]]$model$name
		benchnames = c()
		if(data[[Analysis$vindex]]$bench$exist){
			for(b in 1: (data[[Analysis$vindex]]$bench$howmany) ){
				benchnames[b] = data[[Analysis$vindex]]$bench[[ data[[Analysis$vindex]]$bench$index[b] ]]$name
			}
		}
		legendtext = c('Observed',moname,benchnames)
		
		# Call analysis function:	
		if(Analysis$type == 'Timeseries'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			plotcex = 1.1 # plot text magnification factor
			winsize = 14
			ytext=bquote('Smoothed'~.(tolower(longvarname)) ~ ' (' ~ .(unitstxt) ~ ')')
			areturn = Timeseries(obsname,adata,varname,ytext,legendtext,plotcex,
				data[[Analysis$vindex]]$obs$timing,smoothed=TRUE,winsize,moname,vqcdata=vqcdata)				
		}else if(Analysis$type == 'AnnualCycle'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			ytext = bquote('Average'~.(tolower(longvarname)) ~ ' (' ~ .(unitstxt) ~ ')')
			areturn = AnnualCycle(obsname,adata,varname,ytext,legendtext,
				data[[Analysis$vindex]]$obs$timing$tstepsize,
				data[[Analysis$vindex]]$obs$timing$whole,moname)
		}else if(Analysis$type == 'DiurnalCycle'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			ytext=bquote('Average'~.(varname) ~ ' (' ~.(unitstxt) ~ ')')
			areturn = DiurnalCycle(obsname,adata,varname,ytext,legendtext,
				data[[Analysis$vindex]]$obs$timing$tstepsize,
				data[[Analysis$vindex]]$obs$timing$whole,moname,vqcdata=vqcdata)
		}else if(Analysis$type == 'PDF'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			nbins=500
			xtext=bquote(.(longvarname) ~ ' (' ~ .(unitstxt) ~ ')')
			areturn = PALSPdf(obsname,adata,varname,xtext,legendtext,
				data[[Analysis$vindex]]$obs$timing,nbins,moname,vqcdata=vqcdata)
		}else if(Analysis$type == 'Scatter'){
			# Not a benhcmark plot for the moment:
			bencherrtext = 'B4: Benchmark analysis not available for this analysis type'
			vtext = bquote(.(tolower(longvarname)) ~ ' (' ~.(unitstxt) ~ ')')
			xytext = c('Observed','Modelled')
			areturn = PALSScatter(obsname,data[[Analysis$vindex]]$model$data,
				data[[Analysis$vindex]]$obs$data,varname,vtext,
				xytext,data[[Analysis$vindex]]$obs$timing$tstepsize,
				data[[Analysis$vindex]]$obs$timing$whole,ebal=FALSE,
				modlabel=moname,vqcdata=vqcdata)
		}else if(Analysis$type == 'Taylor'){
			# Not a benhcmark plot for the moment:
			bencherrtext = 'B4: Benchmark analysis not available for this analysis type'
			xtext=bquote(.(longvarname) ~ ' (' ~ .(unitstxt) ~ ')')
			areturn = TaylorDiagram(obsname,data[[Analysis$vindex]]$model$data,
				data[[Analysis$vindex]]$obs$data,varname,xtext,
				data[[Analysis$vindex]]$obs$timing$tstepsize,
				data[[Analysis$vindex]]$obs$timing$whole,moname)
		}else if(Analysis$type == 'AvWindow'){
			# Not a benhcmark plot for the moment:
			bencherrtext = 'B4: Benchmark analysis not available for this analysis type'
			ytext=bquote('Average'~.(longvarname) ~ .(unitstxt))
			areturn = AveragingWindow(obsname,moname,data[[Analysis$vindex]]$model$data,
				data[[Analysis$vindex]]$obs$data,varname,ytext,
				data[[Analysis$vindex]]$obs$timing$tstepsize)
		}
		
	}
	print(outfiletype)
	result = list(type=outfiletype,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png",
		metrics = metrics,
		analysistype=Analysis$type, variablename=varname,
		error=areturn$errtext,bencherror=bencherrtext)
	return(result)
}

CanAnalysisProceed = function(obserr,obserrtext,moderr,moderrtext){
	errtext = 'ok'
	proceed = TRUE
	if(obserr){
		proceed = FALSE
		errtext = obserrtext
	}else if(moderr){
		proceed = FALSE
		errtext = moderrtext
	}	
	result = list(proceed = proceed,errtext = errtext)
	return(result)
}