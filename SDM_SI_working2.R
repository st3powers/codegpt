library(sp)
library(raster)
library(ENMeval)
library(dismo)

library(tidyverse)
# library(rgeos) # deprecated pacakge, no longer on CRAN as of 2023 Oct
library(terra)
library(ecospat)

#install.packages("https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz")

occs <- read.csv(file.path(system.file(package="dismo"), "/ex/bradypus.csv"))[,2:3]
envs <- raster::stack(list.files(path=paste(system.file(package="dismo"), "/ex", sep=""), 
                                 pattern="grd", full.names=TRUE))
occs.z <- cbind(occs, raster::extract(envs, occs))
occs.z$biome <- factor(occs.z$biome)
bg <- as.data.frame(dismo::randomPoints(envs, 1000))
names(bg) <- names(occs)
bg.z <- cbind(bg, raster::extract(envs, bg))
bg.z$biome <- factor(bg.z$biome)

# set other.settings -- pred.type is only for Maxent models
os <- list(abs.auc.diff = FALSE, pred.type = "cloglog", validation.bg = "partition")
# set partition.settings -- here's an example for the block method
# see Details for the required settings for other partition methods
ps <- list(orientation = "lat_lat")


e.maxent.jar <- ENMevaluate(occs, envs, bg, 
                        tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5), 
                        partitions = "block", other.settings = os, partition.settings = ps,
                        algorithm = "maxent.jar", categoricals = "biome", overlap = TRUE)

eval.results(e.maxent.jar)
mods.maxent.jar <- eval.models(e.maxent.jar)
pred.L2 <- enm.maxent.jar@predict(mods.maxent.jar$fc.L_rm.2, envs, os)
raster::plot(pred.L2)



build_sdm <- function(pres, env, buffer_distance_m = 1e6, max_cor = 0.7,
                      regularization = c(0, 1, 5, 10, 100), output_path =  "result.tif") {   
  
  # Step 1: Convert presence data to spatial points
#  pres_sp <- SpatialPointsDataFrame(pres[, c("lon", "lat")], pres)    
  pres_sp <- SpatialPointsDataFrame(
    pres[, c("lon", "lat")],
    pres,
    proj4string = CRS("+init=epsg:4326")
  )
  
  # Step 2: Remove duplicated pres in the cells of env
### env_no_dup <- raster::raster::aggregate(env, fact = round(res(env) / res(pres_sp)))   
#  env_no_dup <- raster::aggregate(env, fact = round(res(env) / res(pres_sp)))   
#  env_no_dup <- raster::aggregate(env, fact = round(raster::res(env) / raster::res(pres_sp)))   
  env_no_dup <- env
  
  pres_no_dup <- raster::extract(env_no_dup, pres_sp)   
  pres_no_dup <- pres_no_dup[!duplicated(pres_no_dup), ]      
  
  # Step 3: Generate modeling domain
  pres_moll <- sp::spTransform(pres_sp, sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0"))   
  env_moll <- projectRaster(env_no_dup, crs = sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0"))   
#  pres_buff <- rgeos::gBuffer(pres_moll, width = buffer_distance_m, byid = TRUE)
  pres_buff <- buffer(pres_moll, width = buffer_distance_m)
  env2 <- raster::mask(env_moll, pres_buff)      
  
  # Step 4: Remove highly correlated layers
  cor_mat <- cor(as.data.frame(env2))   
#  to_remove <- findCorrelation(cor_mat, cutoff = max_cor)   
#  env3 <- env2[, -to_remove]      
  env3 <- env2
  
  # Step 5: Build model with ENMeval
  feature_type <- c("linear", "quadratic")   
#  enm <- ENMeval(pres_no_dup, env3, feature_type = feature_type,
#                 regularization.multiplier = regularization) 
  enm <- ENMevaluate(occs=pres_no_dup, envs=env3,
                     algorithm="bioclim",
                     partitions="jackknife")   
  
  # Step 6: Find best model based on test AUC
  best_model <- enm$results$models[[which.max(enm$results$test.AUC)]]      
  
  # Step 7: Project model onto map
  env_moll_ext <- extent(env_moll)   
  grid <- raster::raster(ext = env_moll_ext, resolution = raster::res(env_moll), crs = crs(env_moll))   
  pred <- predict(best_model$final.model, grid, na.rm = TRUE)      
  
  
  # Step 8: Save model to output_path
  writeRaster(pred, output_path, format = "GTiff", overwrite = TRUE)      
  
  # Step 9: Plot result and presence data
  plot(pred, main = "SDM", legend = FALSE)
  points(pres_sp, pch = 16, col = "red") 
} 

pres0 <- read_csv("Carnegiea-gigantea-GBIF.txt")
pres <- pres0
pres <- pres %>% rename(lat=latitude,lon=longitude)
which(is.na(pres))
pres <- na.omit(pres)


makeDomain <- function(rstack, occ_points, buffer_distance_m) {   
  # Set the projection to Mollweide   
  occ_points_moll <- sp::spTransform(occ_points, sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0")) # EDIT: CRS was incorrectly namespaced to rgdal    
  
  rstack_moll <- raster::projectRaster(rstack, crs=sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0"))# EDIT: CRS was incorrectly namespaced to rgdal    
  
  # Create a buffer around the occurrence points   
  occ_points_buff <- rgeos::gBuffer(occ_points_moll, width = buffer_distance_m, byid = TRUE)    
  
  # Mask the rasters with the buffered occurrence points   
  domain_mask <- raster::mask(rstack_moll, occ_points_buff)    
  
  return(domain_mask) 
}  

# Load WorldClim rasters for BIO1 and BIO12 # EDIT: the code ChatGPT provides downloads all 18 worldclim layers, not just the two its comment mentions 
wc_stack <- raster::getData("worldclim", var="bio", res=10)  
  
# Simulate 200 occurrence points in the US
set.seed(123) 
us_states <- raster::getData("GADM", country="USA", level=1) 

us_points <- sp::spsample(us_states, n=200, type="random")  
# Test the makeDomain function with a buffer distance of 100000 meters 
#domain_mask <- makeDomain(wc_stack, us_points, buffer_distance_m = 1000000)    

# Plot the result raster::plot(domain_mask[[1]]) 
# EDIT: added [[1]] for simplicity  # EDIT: added this plot to ensure sensibility of the result 
plot(wc_stack[[1]]); points(us_points) 

env <- wc_stack

build_sdm(pres,env)

