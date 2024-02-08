library(sp)
library(raster)
library(ENMeval)
# library(Maxent) # no
library(dismo) #new
library(tidyverse) #new

# these starting parameters were taken out of function build_sdm(), but could be re-implemented if necessary
# buffer_dist <- 1e6 
# corr_threshold <- 0.7
# regularization_values <- c(0.1, 0.5, 1, 2, 5)
# output_path <- "sdm_result.tif"

pres <- read.csv(file.path(system.file(package="dismo"), "/ex/bradypus.csv"))[,2:3] #new
env <- raster::stack(list.files(path=paste(system.file(package="dismo"), "/ex", sep=""), #new
                                 pattern="grd", full.names=TRUE)) #new

