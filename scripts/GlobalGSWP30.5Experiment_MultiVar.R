# Master script for Global 0.5 x 0.5 degree experiment based on GSWP3 forcing
# Gab Abramowitz, UNSW, 2015 (palshelp at gmail dot com)

library(pals)
library(parallel)
library(fields)

print(paste('ID:',input["_id"]))
files <- input[["files"]]

# Retrieve model output, forcing and evaluation data set and benchmark location and 
# meta data from javascript input list: 
ModelOutputFiles = list()
ForcingDataSetFiles = list()
EvalDataSetFiles = list()
BenchmarkFiles = list()
OutputList = list()
MOctr = FDSctr = EDSctr = Bctr = 0
for (i in 1:(length(files))  ) {
  file <- files[[i]]
  if( file[['type']] == "ModelOutput" ) {
    MOctr = MOctr + 1
    ModelOutputFiles[[MOctr]] = list(path=file[['path']],mimetype=file[['mimetype']],
                                     name=file[['name']],variable=file[['variable']])
  }else if( (file[['type']] == "DataSet")) {
    EDSctr = EDSctr + 1
    EvalDataSetFiles[[EDSctr]] = list(path=file[['path']],mimetype=file[['mimetype']],
                                      name=file[['name']],variable=file[['variable']])
  }else if( file[['type']] == "Benchmark") {
    Bctr = Bctr + 1
    BenchmarkFiles[[Bctr]] = list(path=file[['path']],mimetype=file[['mimetype']],
                                  name=file[['name']],variable=file[['variable']],number=file[['number']]) # number: user rank of benchmark
  }
}

region = 'Global'

# Analyses that can apply to any variable:
genAnalysis = c('TimeMean','TimeSD','TimeRMSE','TimeCor') #'PDFall','PDF2D','Taylor')

# Determine number of user-nominated benchmarks:
nBench = BenchmarkInfo(BenchmarkFiles,Bctr)

# Set up analysis data and analysis list so we can use lapply or parlapply:
AnalysisList = list()

# Initialize output list of analyses
OutInfo = list()

# Initialize analyses counter
actr = 0

# Call subfunction that separates EvalDataSetFiles according to name and variable
EvalProducts = SeparateEval(EvalDataSetFiles)

# Load all variables from obs and model output
for(v in 1:length(EvalProducts)){
  if((EvalProducts[[v]][[1]]$name == "GLEAM_GSWP3") & (EvalProducts[[v]][[1]]$variable == "Qle")){
    # Nominate variables to analyse here (use ALMA standard names) - fetches alternate names, units, units transformations etc:
    vars = GetVariableDetails(c('Qle'))
    obs = GetGLEAM_Global(vars[[1]],EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(Analysis=SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
    
  }else if((EvalProducts[[v]][[1]]$name == "GLEAM_v2A") & (EvalProducts[[v]][[1]]$variable == "Qle")){
    # Nominate variables to analyse here (use ALMA standard names) - fetches alternate names, units, units transformations etc:
    vars = GetVariableDetails(c('Qle'))
    obs = GetGLEAM_Global_v2A(variable=vars[[1]],filelist=EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)
    ## Change resolution of "obs" from 0.25° to 0.5°
    obsi=array(NA,dim=c(length(model$grid$lon),length(model$grid$lat),obs$timing$tsteps))
    grid.list<- list(x= model$grid$lon, y=model$grid$lat)
    for(t in 1:obs$timing$tsteps){
      obj<- list( x= obs$grid$lon[,1], y=obs$grid$lat[1,], z= obs$data[,,t]) 
      obs_interp=interp.surface.grid( obj, grid.list)
      obsi[,,t]=obs_interp$z
    }
    obs$data <- obsi
    obs$grid <- model$grid   
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
    
  }else if((EvalProducts[[v]][[1]]$name == "GLEAM_v2B") & (EvalProducts[[v]][[1]]$variable == "Qle")){
    # Nominate variables to analyse here (use ALMA standard names) - fetches alternate names, units, units transformations etc:
    vars = GetVariableDetails(c('Qle'))
    obs = GetGLEAM_Global_v2B(variable=vars[[1]],filelist=EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)
    # Change resolution of "obs" from 0.25° to 0.5°
    obsi=array(NA,dim=c(length(model$grid$lon),length(model$grid$lat[63:298]),obs$timing$tsteps))
    grid.list<- list(x= model$grid$lon, y=model$grid$lat[63:298])
    for(t in 1:obs$timing$tsteps){
      obj<- list( x= obs$grid$lon[,1], y=obs$grid$lat[1,], z= obs$data[,,t]) 
      obs_interp=interp.surface.grid( obj, grid.list)
      obsi[,,t]=obs_interp$z
    }
    obs$data <- obsi
    obs$grid$lat <- model$grid$lat[63:298] 
    obs$grid$lon <- model$grid$lon
    obs$grid$latlen <- length(obs$grid$lat)
    obs$grid$lonlen <- length(obs$grid$lon)
    # Choose correct latitude range (-59 to 59) for "model"
    model$data <- model$data[,63:298,]
    model$grid$lat <- model$grid$lat[63:298] 
    model$grid$latlen <- length(model$grid$lat)
    # Choose correct latitude range (-59 to 59) for "bench"
    for(i in 1:bench$howmany){
      bench[[bench$index[i]]]$data <- bench[[bench$index[i]]]$data[,63:298,]
      bench[[bench$index[i]]]$grid$lat <- bench[[bench$index[i]]]$grid$lat[63:298]
      bench[[bench$index[i]]]$grid$latlen <- length(bench[[bench$index[i]]]$grid$lat)
    }
          
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
    
  }else if((EvalProducts[[v]][[1]]$name == "MPI") & (EvalProducts[[v]][[1]]$variable == "Qle")){
    vars = GetVariableDetails(c('Qle'))
    obs = GetMPI_Global(vars[[1]],EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
    
  }else if((EvalProducts[[v]][[1]]$name == "MPI") & (EvalProducts[[v]][[1]]$variable == "NEE")){
    vars = GetVariableDetails(c('NEE'))
    obs = GetMPI_NEE_Global(vars[[1]],EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(Analysis=SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
    
  }else if((EvalProducts[[v]][[1]]$name == "MPI_uncertainty") & (EvalProducts[[v]][[1]]$variable == "Qle")){
    vars = GetVariableDetails(c('Qle'))
    obs = GetMPI_uncertainty_Global(vars[[1]],EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)
    genAnalysis = c('Timeseries')
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(Analysis=SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
    
  }else if((EvalProducts[[v]][[1]]$name == "MODIS") & (EvalProducts[[v]][[1]]$variable == "Qle")){
    vars = GetVariableDetails(c('Qle'))
    obs = GetMODIS_Global(vars[[1]],EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
    
  }else if((EvalProducts[[v]][[1]]$name == "MODIS") & (EvalProducts[[v]][[1]]$variable == "VISalbedo")){
    vars = GetVariableDetails(c('VISalbedo'))
    obs = GetMODIS_VISalbedo_Global(variable=vars[[1]],filelist=EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
    
  }else if((EvalProducts[[v]][[1]]$name == "MODIS") & (EvalProducts[[v]][[1]]$variable == "NIRalbedo")){
    vars = GetVariableDetails(c('NIRalbedo'))
    obs = GetMODIS_NIRalbedo_Global(variable=vars[[1]],filelist=EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
    
  }else if((EvalProducts[[v]][[1]]$name == "LandFlux") & (EvalProducts[[v]][[1]]$variable == "Qle")){
    vars = GetVariableDetails(c('Qle'))
    obs = GetLandFlux_Global(vars[[1]],EvalProducts[[v]],force_interval='monthly')
    model = GetModelOutput(vars[[1]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[1]],BenchmarkFiles,nBench)  
    ## Change resolution of "obs" from 1° to 0.5°
    #data_in=list(dat=obs$data,lat=obs$grid$lat,lon=obs$grid$lon,lon_bnds=matrix(c(obs$grid$lon-0.5,obs$grid$lon+0.5),ncol=360,byrow=T),lat_bnds=matrix(c(obs$grid$lat-0.5,obs$grid$lat+0.5),ncol=180,byrow=T))
    #obs_fine=interpolate_areaweight(data_in,model$grid$lat,model$grid$lon)
    #obs$data <- obs_fine  
    #obs$grid <- model$grid 
    ## Change resolution of "model" from 0.5° to 1°
    modeli=array(NA,dim=c(length(obs$grid$lon),length(obs$grid$lat),model$timing$tsteps))
    grid.list<- list(x= obs$grid$lon, y=obs$grid$lat)
    for(t in 1:model$timing$tsteps){
      obj<- list( x= model$grid$lon, y=model$grid$lat, z= model$data[,,t]) 
      model_interp=interp.surface.grid( obj, grid.list)
      modeli[,,t]=model_interp$z
    }
    model$data <- modeli
    model$grid <- obs$grid   
    ## Change resolution of "bench" from 0.5° to 1°    
    grid.list<- list(x= obs$grid$lon, y=obs$grid$lat)
    for(b in 1:bench$howmany){
      benchi=array(NA,dim=c(length(obs$grid$lon),length(obs$grid$lat),bench[[b]]$timing$tsteps))
      for(t in 1:bench[[bench$index[[b]]]]$timing$tsteps){
        obj<- list( x= bench[[b]]$grid$lon, y=bench[[b]]$grid$lat, z= bench[[b]]$data[,,t]) 
        bench_interp=interp.surface.grid( obj, grid.list)
        benchi[,,t]=bench_interp$z
      }
      bench[[b]]$data <- benchi
      bench[[b]]$grid <- obs$grid
    }   
    # Add those analyses that are equally applicable to any variable to analysis list:
    for(a in 1:length(genAnalysis)){
      SingleAnalysisList = list(vindex=1, type=genAnalysis[a])
      actr = actr + 1
      # Create cluster:
      cl = makeCluster(getOption('cl.cores', detectCores()))
      OutInfo[[actr]] = DistributeGriddedAnalyses(SingleAnalysisList,vars=vars,obs=obs,model=model,bench=bench,region=region,cl)
    }
    stopCluster(cl) # Stop cluster
  }
}

for(v in 1:actr){
  # Write outinfo to output list for javascript:
  OutputList[[actr]] = list(files=OutInfo[[actr]]);
  output = OutputList[[actr]]
  
  for(i in 1: length(output[["files"]])){
    cat('Output ',i,': \n')
    cat('  type:',output[["files"]]$type,'\n')
    if(!is.null(output[["files"]]$error)){
      cat('  ERROR: ',output[["files"]]$error,'\n')
    }else{
      cat('  filename:',output[["files"]]$filename,'\n')
      cat('  bench error:',output[["files"]]$bencherror,'\n')
      cat('  first metric for model - ',output[["files"]]$metrics[[1]]$name,':',
          output[["files"]]$metrics[[1]]$model_value,'\n')
    }
  }
}