# GetPRINCETON.R
#
# This script fetches PRINCETON SWdown and LWdown data
#
# Gab Abramowitz, UNSW, 2015, gabsun at gmail dot com
# Sanaa Hobeichi, UNSW, 2016
#
################################################################################################################

#==============================================================================================================
GetPRINCETON_SWdown_monthly_Global= function(variable,filelist,force_interval='no',dsetversion='default'){
# 'variable' - list from GetVariableDetails
# 'filedetails' - list containing path, mimetype, component etc

library(ncdf4) # load netcdf library	

errtext='ok'	
if((variable[['Name']][1] != 'SWdown')){
  errtext = 'Request for non-Surface shortwave down variable to GetPRINCETON_SWdown_Global read routine.'
  obs = list(err=TRUE,errtext=errtext)
  return(obs)
}
  nyears = length(filelist)
  year = c() 
  for(f in 1:nyears){ # For each file sent by js
    # Establish which year the file contains:
    year[f] = as.numeric(substr(filelist[[f]][['path']],(nchar(filelist[[f]][['path']])-6), (nchar(filelist[[f]][['path']])-3) ) )
  }
  # Define the order to read files:
  fileorder = order(year)	
  # Define number of months or timesetps in total:
  if((force_interval == 'no') | (force_interval == 'monthly')){
    interval = 'monthly'
    tsteps = nyears*12
  
    SWdown = array(NA,dim=c(720,360,tsteps))	# Initialise data array:
  }else{
    errtext = paste('GetPRINCETON_SWdown_monthly_Global requested to force to unknown interval:',force_interval)
    obs = list(err=TRUE,errtext=errtext)
    return(obs)	
  }
 # Get data:

for(f in 1:nyears){ # For each file sent by js		
  # Open file:
  fid = nc_open(filelist[[ fileorder[f] ]][['path']],write=FALSE,readunlim=FALSE)
 
  # Read SWdown data for this year and find monthly averages 

	if(!is.leap(f)){
	  
		jandata=ncvar_get(fid,"dswrf",start=c(1,1,1),count=c(720,360,248))
  		SWdown[,,((f-1)*12+1)]= apply(jandata,c(1,2),mean)

		febdata=ncvar_get(fid,"dswrf",start=c(1,1,249),count=c(720,360,224))
		SWdown[,,((f-1)*12+2)]= apply(febdata,c(1,2),mean)
	
		mardata=ncvar_get(fid,"dswrf",start=c(1,1,473),count=c(720,360,248))
		SWdown[,,((f-1)*12+3)]= apply(mardata,c(1,2),mean)

		aprdata=ncvar_get(fid,"dswrf",start=c(1,1,721),count=c(720,360,240))
		SWdown[,,((f-1)*12+4)]= apply(aprdata,c(1,2),mean)

		maydata=ncvar_get(fid,"dswrf",start=c(1,1,961),count=c(720,360,248))
		SWdown[,,((f-1)*12+5)]= apply(maydata,c(1,2),mean)

		jundata=ncvar_get(fid,"dswrf",start=c(1,1,1209),count=c(720,360,240))
		SWdown[,,((f-1)*12+6)]= apply(jundata,c(1,2),mean)

		juldata=ncvar_get(fid,"dswrf",start=c(1,1,1449),count=c(720,360,248))
		SWdown[,,((f-1)*12+7)]= apply(juldata,c(1,2),mean)

		augdata=ncvar_get(fid,"dswrf",start=c(1,1,1697),count=c(720,360,248))
		SWdown[,,((f-1)*12+8)]= apply(augdata,c(1,2),mean)

		sepdata=ncvar_get(fid,"dswrf",start=c(1,1,1945),count=c(720,360,240))
		SWdown[,,((f-1)*12+9)]= apply(sepdata,c(1,2),mean)

		octdata=ncvar_get(fid,"dswrf",start=c(1,1,2185),count=c(720,360,248))
		SWdown[,,((f-1)*12+10)]= apply(octdata,c(1,2),mean)

		novdata=ncvar_get(fid,"dswrf",start=c(1,1,2432),count=c(720,360,240))
		SWdown[,,((f-1)*12+11)]=apply(novdata,c(1,2),mean)

		decdata=ncvar_get(fid,"dswrf",start=c(1,1,2673),count=c(720,360,248))
		SWdown[,,((f-1)*12+12)]= apply(decdata,c(1,2),mean)
	}else{
		jandata=ncvar_get(fid,"dswrf",start=c(1,1,1),count=c(720,360,248))
  		SWdown[,,((f-1)*12+1)]= apply(jandata,c(1,2),mean)

		febdata=ncvar_get(fid,"dswrf",start=c(1,1,249),count=c(720,360,232))
		SWdown[,,((f-1)*12+2)]= apply(febdata,c(1,2),mean)
	
		mardata=ncvar_get(fid,"dswrf",start=c(1,1,481),count=c(720,360,248))
		SWdown[,,((f-1)*12+3)]= apply(mardata,c(1,2),mean)

		aprdata=ncvar_get(fid,"dswrf",start=c(1,1,729),count=c(720,360,240))
		SWdown[,,((f-1)*12+4)]= apply(aprdata,c(1,2),mean)

		maydata=ncvar_get(fid,"dswrf",start=c(1,1,969),count=c(720,360,248))
		SWdown[,,((f-1)*12+5)]= apply(maydata,c(1,2),mean)

		jundata=ncvar_get(fid,"dswrf",start=c(1,1,1217),count=c(720,360,240))
		SWdown[,,((f-1)*12+6)]= apply(jundata,c(1,2),mean)

		juldata=ncvar_get(fid,"dswrf",start=c(1,1,1457),count=c(720,360,248))
		SWdown[,,((f-1)*12+7)]= apply(juldata,c(1,2),mean)

		augdata=ncvar_get(fid,"dswrf",start=c(1,1,1705),count=c(720,360,248))
		SWdown[,,((f-1)*12+8)]= apply(augdata,c(1,2),mean)

		sepdata=ncvar_get(fid,"dswrf",start=c(1,1,1953),count=c(720,360,240))
		SWdown[,,((f-1)*12+9)]= apply(sepdata,c(1,2),mean)

		octdata=ncvar_get(fid,"dswrf",start=c(1,1,2193),count=c(720,360,248))
		SWdown[,,((f-1)*12+10)]= apply(octdata,c(1,2),mean)

		novdata=ncvar_get(fid,"dswrf",start=c(1,1,2440),count=c(720,360,240))
		SWdown[,,((f-1)*12+11)]=apply(novdata,c(1,2),mean)

		decdata=ncvar_get(fid,"dswrf",start=c(1,1,2681),count=c(720,360,248))
		SWdown[,,((f-1)*12+12)]= apply(decdata,c(1,2),mean)

	}

  # Close netcdf file for this year:
  nc_close(fid)
}
  # Reopen first file to fetch lat and lon:
  fid = nc_open(filelist[[ 1 ]][['path']],write=FALSE,readunlim=FALSE)
  # Then get spatial grid structure from first model output file:
  grid = GetGrid(fid)
  
  if(grid$err){	
    obs = list(err=TRUE,errtext=grid$errtext)
    nc_close(fid) # Close netcdf file
    return(obs)
  }
  

  nc_close(fid)
  
  timing = list(interval=interval,tsteps=tsteps,syear=year)
  
  errtext='ok'
  
  # Return result
  obs = list(err=FALSE,errtext=errtext,data=SWdown,grid=grid,timing=timing,name='PRINCETON SWdown')
  return(obs)	
}

#=====================================================================================================

GetPRINCETON_LWdown_monthly_Global= function(variable,filelist,force_interval='no',dsetversion='default'){
# 'variable' - list from GetVariableDetails
# 'filedetails' - list containing path, mimetype, component etc

library(ncdf4) # load netcdf library	

errtext='ok'	
if((variable[['Name']][1] != 'LWdown')){
  errtext = 'Request for non-Surface shortwave down variable to GetPRINCETON_LWdown_Global read routine.'
  obs = list(err=TRUE,errtext=errtext)
  return(obs)
}
  nyears = length(filelist)
  year = c() 
  for(f in 1:nyears){ # For each file sent by js
    # Establish which year the file contains:
    year[f] = as.numeric(substr(filelist[[f]][['path']],(nchar(filelist[[f]][['path']])-6), (nchar(filelist[[f]][['path']])-3) ) )
  }
  # Define the order to read files:
  fileorder = order(year)	
  # Define number of months or timesetps in total:
  if((force_interval == 'no') | (force_interval == 'monthly')){
    interval = 'monthly'
    tsteps = nyears*12
  
    SWdown = array(NA,dim=c(720,360,tsteps))	# Initialise data array:
  }else{
    errtext = paste('GetPRINCETON_LWdown_monthly_Global requested to force to unknown interval:',force_interval)
    obs = list(err=TRUE,errtext=errtext)
    return(obs)	
  }
 # Get data:

for(f in 1:nyears){ # For each file sent by js		
  # Open file:
  fid = nc_open(filelist[[ fileorder[f] ]][['path']],write=FALSE,readunlim=FALSE)
 
  # Read SWdown data for this year and find monthly averages 


	if(!is.leap(f)){
	  
		jandata=ncvar_get(fid,"dlwrf",start=c(1,1,1),count=c(720,360,248))
  		SWdown[,,((f-1)*12+1)]= apply(jandata,c(1,2),mean)

		febdata=ncvar_get(fid,"dlwrf",start=c(1,1,249),count=c(720,360,224))
		SWdown[,,((f-1)*12+2)]= apply(febdata,c(1,2),mean)
	
		mardata=ncvar_get(fid,"dlwrf",start=c(1,1,473),count=c(720,360,248))
		SWdown[,,((f-1)*12+3)]= apply(mardata,c(1,2),mean)

		aprdata=ncvar_get(fid,"dlwrf",start=c(1,1,721),count=c(720,360,240))
		SWdown[,,((f-1)*12+4)]= apply(aprdata,c(1,2),mean)

		maydata=ncvar_get(fid,"dlwrf",start=c(1,1,961),count=c(720,360,248))
		SWdown[,,((f-1)*12+5)]= apply(maydata,c(1,2),mean)

		jundata=ncvar_get(fid,"dlwrf",start=c(1,1,1209),count=c(720,360,240))
		SWdown[,,((f-1)*12+6)]= apply(jundata,c(1,2),mean)

		juldata=ncvar_get(fid,"dlwrf",start=c(1,1,1449),count=c(720,360,248))
		SWdown[,,((f-1)*12+7)]= apply(juldata,c(1,2),mean)

		augdata=ncvar_get(fid,"dlwrf",start=c(1,1,1697),count=c(720,360,248))
		SWdown[,,((f-1)*12+8)]= apply(augdata,c(1,2),mean)

		sepdata=ncvar_get(fid,"dlwrf",start=c(1,1,1945),count=c(720,360,240))
		SWdown[,,((f-1)*12+9)]= apply(sepdata,c(1,2),mean)

		octdata=ncvar_get(fid,"dlwrf",start=c(1,1,2185),count=c(720,360,248))
		SWdown[,,((f-1)*12+10)]= apply(octdata,c(1,2),mean)

		novdata=ncvar_get(fid,"dlwrf",start=c(1,1,2432),count=c(720,360,240))
		SWdown[,,((f-1)*12+11)]=apply(novdata,c(1,2),mean)

		decdata=ncvar_get(fid,"dlwrf",start=c(1,1,2673),count=c(720,360,248))
		SWdown[,,((f-1)*12+12)]= apply(decdata,c(1,2),mean)
	}else{
		jandata=ncvar_get(fid,"dlwrf",start=c(1,1,1),count=c(720,360,248))
  		SWdown[,,((f-1)*12+1)]= apply(jandata,c(1,2),mean)

		febdata=ncvar_get(fid,"dlwrf",start=c(1,1,249),count=c(720,360,232))
		SWdown[,,((f-1)*12+2)]= apply(febdata,c(1,2),mean)
	
		mardata=ncvar_get(fid,"dlwrf",start=c(1,1,481),count=c(720,360,248))
		SWdown[,,((f-1)*12+3)]= apply(mardata,c(1,2),mean)

		aprdata=ncvar_get(fid,"dlwrf",start=c(1,1,729),count=c(720,360,240))
		SWdown[,,((f-1)*12+4)]= apply(aprdata,c(1,2),mean)

		maydata=ncvar_get(fid,"dlwrf",start=c(1,1,969),count=c(720,360,248))
		SWdown[,,((f-1)*12+5)]= apply(maydata,c(1,2),mean)

		jundata=ncvar_get(fid,"dlwrf",start=c(1,1,1217),count=c(720,360,240))
		SWdown[,,((f-1)*12+6)]= apply(jundata,c(1,2),mean)

		juldata=ncvar_get(fid,"dlwrf",start=c(1,1,1457),count=c(720,360,248))
		SWdown[,,((f-1)*12+7)]= apply(juldata,c(1,2),mean)

		augdata=ncvar_get(fid,"dlwrf",start=c(1,1,1705),count=c(720,360,248))
		SWdown[,,((f-1)*12+8)]= apply(augdata,c(1,2),mean)

		sepdata=ncvar_get(fid,"dlwrf",start=c(1,1,1953),count=c(720,360,240))
		SWdown[,,((f-1)*12+9)]= apply(sepdata,c(1,2),mean)

		octdata=ncvar_get(fid,"dlwrf",start=c(1,1,2193),count=c(720,360,248))
		SWdown[,,((f-1)*12+10)]= apply(octdata,c(1,2),mean)

		novdata=ncvar_get(fid,"dlwrf",start=c(1,1,2440),count=c(720,360,240))
		SWdown[,,((f-1)*12+11)]=apply(novdata,c(1,2),mean)

		decdata=ncvar_get(fid,"dlwrf",start=c(1,1,2681),count=c(720,360,248))
		SWdown[,,((f-1)*12+12)]= apply(decdata,c(1,2),mean)

	}

  # Close netcdf file for this year:
  nc_close(fid)
}
  # Reopen first file to fetch lat and lon:
  fid = nc_open(filelist[[ 1 ]][['path']],write=FALSE,readunlim=FALSE)
  # Then get spatial grid structure from first model output file:
  grid = GetGrid(fid)
  
  if(grid$err){	
    obs = list(err=TRUE,errtext=grid$errtext)
    nc_close(fid) # Close netcdf file
    return(obs)
  }
  

  nc_close(fid)
  
  timing = list(interval=interval,tsteps=tsteps,syear=year)
  
  errtext='ok'
  
  # Return result
  obs = list(err=FALSE,errtext=errtext,data=SWdown,grid=grid,timing=timing,name='PRINCETON LWdown')
  return(obs)	
}



