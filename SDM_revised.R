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

output_path <- "sdm_result_gray.tif" #new
output_path1 <- "sdm_result.tif" #new
output_path2 <- "sdm_result_points.tif" #new

pres <- read.csv(file.path(system.file(package="dismo"), "/ex/bradypus.csv"))[,2:3] #new
env <- raster::stack(list.files(path=paste(system.file(package="dismo"), "/ex", sep=""), #new
                                 pattern="grd", full.names=TRUE)) #new

# Step 1: Convert presence data to spatial points
#  pres_sp <- SpatialPointsDataFrame(coords = pres[, c("longitude", "latitude")], data = pres)
pres_sp <- SpatialPointsDataFrame(coords = pres[, c("lon", "lat")], data = pres) #new
  
# Step 2: Remove duplicated pres in the cells of env
#  pres_sp <- unique(pres_sp)
  
# Step 3: Generate modeling domain by buffering occurrence points
# buffered_pres <- gBuffer(pres_sp, width = buffer_dist)
# buffered_pres <- buffer(pres_sp, width = buffer_dist)
# env_masked <- mask(env, buffered_pres)
  
# Step 4: Perform correlation analysis and remove layers
#  env_corr <- abs(cor(env_masked))
#  highly_correlated <- findCorrelation(env_corr, cutoff = corr_threshold)
#  env_filtered <- env_masked[[setdiff(1:nlayers(env_masked), highly_correlated)]]
  
# Step 5: Build model using ENMeval
#  enm_model <- maxent(x = env_filtered, p = pres_sp)
enm_model <- maxent(x = env, p = pres_sp) #new
  
# Step 6: Find the best model based on test AUC
#best_model <- enm_model$models[[which.max(enm_model$testAUC)]]
  
# Step 7: Project the result onto a map
# sdm_result <- predict(best_model, env, type = "response")
sdm_result <- predict(enm_model, env, type = "response") #new
  
# Step 8: Save the result to the specified path
writeRaster(sdm_result, filename = output_path, format = "GTiff", overwrite = TRUE)
  
# Step 9: Plot result and presence data on the same map
#plot(sdm_result, main = "Species Distribution Model", legend = FALSE)
#  points(pres_sp, col = "red", pch = 16)

# new plot outputs
png(output_path1,width=6,height=6,units="in",res=600)
plot(sdm_result, main = "Species Distribution Model")
dev.off()

png(output_path2,width=6,height=6,units="in",res=600)
plot(sdm_result, main = "Species Distribution Model")
points(pres_sp, col = "red", pch = 16)
dev.off()

