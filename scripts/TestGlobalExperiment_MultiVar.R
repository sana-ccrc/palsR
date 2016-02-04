# setwd("/home/z5043321/PalsFork/palsR/scripts")
library("RJSONIO")
#inputFile <- "TestInput_Global_MPI.json"
#inputFile <- "TestInput_Global_NEE_MPI.json"
#inputFile <- "TestInput_Global_MPI_uncertainty.json"
#inputFile <- "TestInput_Global_MODIS.json"
#inputFile <- "TestInput_Global_MODIS_VISalbedo.json"
#inputFile <- "TestInput_Global_MODIS_NIRalbedo.json"
#inputFile <- "TestInput_Global.json"
#inputFile <- "TestInput_Global_GLEAM_v2A.json"
#inputFile <- "TestInput_Global_GLEAM_v2B.json"
#inputFile <- "TestInput_Global_LandFlux.json"
#inputFile <- "TestInput_Global_MsTMIP_ET.json"
#inputFile <- "TestInput_Global_EBAF_SWdown.json"
#inputFile <- "TestInput_Global_EBAF_LWdown.json"#shows  ploting errors
#inputFile <- "TestInput_Global_SWdown.json"
#inputFile <- "TestInput_Global_LWdown.json" #shows plotting errors
input <- fromJSON(paste(readLines(inputFile), collapse=""));
Rruntime = system.time(source("GlobalGSWP30.5Experiment_MultiVar.R"))
print(paste('Time to run:',Rruntime[3]))
output <- toJSON(output)
fileConn<-file("output_Global.json")
writeLines(output, fileConn)
close(fileConn)
