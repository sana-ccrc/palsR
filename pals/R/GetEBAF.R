# GetEBAF.R
#
# This script fetches EBAF SWdown, LWdown and LWnet data
#
# Gab Abramowitz, UNSW, 2015, gabsun at gmail dot com
# Sanaa Hobeichi, UNSW, 2016
#
################################################################################################################
GetEBAF_SWdown_Global= function(variable,filelist,force_interval='no',dsetversion='default'){
# 'variable' - list from GetVariableDetails
# 'filedetails' - list containing path, mimetype, component etc

library(ncdf4) # load netcdf library	
errtext='ok'	
if((variable[['Name']][1] != 'SWdown')){
  errtext = 'Request for non-Surface shortwave down variable to GetEBAF_SWdonwn_Global read routine.'
  obs = list(err=TRUE,errtext=errtext)
  return(obs)
}
  nyears = length(filelist)
  year = c() 
  for(f in 1:nyears){ # For each file sent by js
    # Establish which year the file contains:
    year[f] = as.numeric(substr(filelist[[f]][['path']], 
                                (nchar(filelist[[f]][['path']])-6), (nchar(filelist[[f]][['path']])-3) ) )
  }
  # Define the order to read files:
  fileorder = order(year)	
  # Define number of months(or time steps) in total:
  if((force_interval == 'no') | (force_interval == 'monthly')){
    interval = 'monthly'
    tsteps = nyears*12
    SWdown = array(NA,dim=c(360,180,tsteps))	# Initialise data array:
  }else{
    errtext = paste('GetEBAF_SWdown_Global requested to force to unknown interval:',force_interval)
    obs = list(err=TRUE,errtext=errtext)
    return(obs)	
  }
  # Get data:
  for(f in 1:nyears){ # For each file sent by js		
    # Open file:
    fid = nc_open(filelist[[ fileorder[f] ]][['path']],write=FALSE,readunlim=FALSE)
    # Read EBAF data for this year:
    if(interval == 'monthly'){
	SWdown_tmp= ncvar_get(fid, 'sfc_sw_down_all_mon' ) # read model output data
	SWdown_tmp2=SWdown_tmp
	SWdown_tmp[1:180,,]=SWdown_tmp2[181:360,,]
  	SWdown_tmp[181:360,,]=SWdown_tmp2[1:180,,]
	SWdown[,, ((f-1)*12+1) : ((f-1)*12+12)] = SWdown_tmp
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
  

#convert the grid's longitude from -180, 180 to 0, 360
grid$lon=grid$lon + 180

  nc_close(fid)
  
  timing = list(interval=interval,tsteps=tsteps,syear=year)
  
  errtext='ok'
  
  # Return result
  obs = list(err=FALSE,errtext=errtext,data=SWdown,grid=grid,timing=timing,name='EBAF SWdown')
  return(obs)	
}
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
GetEBAF_LWdown_Global= function(variable,filelist,force_interval='no',dsetversion='default'){
# 'variable' - list from GetVariableDetails
# 'filedetails' - list containing path, mimetype, component etc

library(ncdf4) # load netcdf library	
errtext='ok'	
if((variable[['Name']][1] != 'LWdown')){
  errtext = 'Request for non-Surface longwave down variable to GetEBAF_LWdonwn_Global read routine.'
  obs = list(err=TRUE,errtext=errtext)
  return(obs)
}
  nyears = length(filelist)
  year = c() 
  for(f in 1:nyears){ # For each file sent by js
    # Establish which year the file contains:
    year[f] = as.numeric(substr(filelist[[f]][['path']], 
                                (nchar(filelist[[f]][['path']])-6), (nchar(filelist[[f]][['path']])-3) ) )
  }
  # Define the order to read files:
  fileorder = order(year)	
  # Define number of months(or time steps) in total:
  if((force_interval == 'no') | (force_interval == 'monthly')){
    interval = 'monthly'
    tsteps = nyears*12
    SWdown = array(NA,dim=c(360,180,tsteps))	# Initialise data array:
  }else{
    errtext = paste('GetEBAF_LWdown_Global requested to force to unknown interval:',force_interval)
    obs = list(err=TRUE,errtext=errtext)
    return(obs)	
  }
  # Get data:
  for(f in 1:nyears){ # For each file sent by js		
    # Open file:
    fid = nc_open(filelist[[ fileorder[f] ]][['path']],write=FALSE,readunlim=FALSE)
    # Read EBAF data for this year:
    if(interval == 'monthly'){
	SWdown_tmp= ncvar_get(fid, 'sfc_lw_down_all_mon' ) # read model output data
	SWdown_tmp2=SWdown_tmp
	SWdown_tmp[1:180,,]=SWdown_tmp2[181:360,,]
  	SWdown_tmp[181:360,,]=SWdown_tmp2[1:180,,]
	SWdown[,, ((f-1)*12+1) : ((f-1)*12+12)] = SWdown_tmp
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
  

#convert the grid's longitude from -180, 180 to 0, 360
grid$lon=grid$lon + 180

  nc_close(fid)
  
  timing = list(interval=interval,tsteps=tsteps,syear=year)
  
  errtext='ok'
  
  # Return result
  obs = list(err=FALSE,errtext=errtext,data=SWdown,grid=grid,timing=timing,name='EBAF SWdown')
  return(obs)	
}


#To Do
#GetEBAF_LWnet_Global

