# setwd("/media/nadja/Documents/CCRC/palsR/scripts")
library("RJSONIO")
#inputFile <- "TestInput_Regional_05deg_short_MultiVar_MPI.json"
inputFile <- "TestInput_Regional_05deg_short_MPI_uncertainty.json"
#inputFile <- "TestInput_Regional_05deg_short_GLEAM.json"
#inputFile <- "TestInput_Regional_05deg_short_GLEAM_v2A.json"
#inputFile <- "TestInput_Regional_05deg_short_GLEAM_v2B.json"
#inputFile <- "TestInput_Regional_05deg_short_MODIS.json"
#inputFile <- "TestInput_Regional_05deg_short_MODIS_VISalbedo.json"
#inputFile <- "TestInput_Regional_05deg_short_MODIS_NIRalbedo.json"
#inputFile <- "TestInput_Regional_05deg_short_LandFlux.json"
input <- fromJSON(paste(readLines(inputFile), collapse=""));
Rruntime = system.time(source("RegionalAus0.5Experiment_MultiVar.R"))
print(paste('Time to run:',Rruntime[3]))
output <- toJSON(output)
fileConn<-file("output_Regional.json")
writeLines(output, fileConn)
close(fileConn)