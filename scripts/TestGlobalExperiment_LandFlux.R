# setwd("/media/nadja/Documents/CCRC/palsR/scripts")
library("RJSONIO")
inputFile <- "TestInput_Global_LandFlux.json"
input <- fromJSON(paste(readLines(inputFile), collapse=""));
Rruntime = system.time(source("GlobalGSWP30.5Experiment_LandFlux.R"))
print(paste('Time to run:',Rruntime[3]))
output <- toJSON(output)
fileConn<-file("output_Global.json")
writeLines(output, fileConn)
close(fileConn)