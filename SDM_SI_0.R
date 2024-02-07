library(sp)
library(raster)
library(ENMeval)
library(dismo)

build_sdm <- function(pres, env, buffer_distance_m = 1e6, max_cor = 0.7,
                      regularization = c(0, 1, 5, 10, 100), output_path =  "result.tif") {   
  
  # Step 1: Convert presence data to spatial points
  pres_sp <- SpatialPointsDataFrame(pres[, c("lon", "lat")], pres)    
  
  # Step 2: Remove duplicated pres in the cells of env
  env_no_dup <- raster::raster::aggregate(env, fact = round(res(env) / res(pres_sp)))   
  pres_no_dup <- raster::extract(env_no_dup, pres_sp)   
  pres_no_dup <- pres_no_dup[!duplicated(pres_no_dup), ]      
  
  # Step 3: Generate modeling domain
  pres_moll <- sp::spTransform(pres_sp, sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0"))   
  env_moll <- projectRaster(env_no_dup, crs = sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0"))   
  pres_buff <- rgeos::gBuffer(pres_moll, width = buffer_distance_m, byid = TRUE)   
  env2 <- raster::mask(env_moll, pres_buff)      
  
  # Step 4: Remove highly correlated layers
  cor_mat <- cor(as.data.frame(env2))   
  to_remove <- findCorrelation(cor_mat, cutoff = max_cor)   
  env3 <- env2[, -to_remove]      
  
  # Step 5: Build model with ENMeval
  feature_type <- c("linear", "quadratic")   
  enm <- ENMeval(pres_no_dup, env3, feature_type = feature_type,
                 regularization.multiplier = regularization)      
  
  # Step 6: Find best model based on test AUC
  best_model <- enm$results$models[[which.max(enm$results$test.AUC)]]      
  
  # Step 7: Project model onto map
  env_moll_ext <- extent(env_moll)   
  grid <- raster::raster(ext = env_moll_ext, resolution = res(env_moll), crs = crs(env_moll))   
  pred <- predict(best_model$final.model, grid, na.rm = TRUE)      
  
  
  # Step 8: Save model to output_path
  writeRaster(pred, output_path, format = "GTiff", overwrite = TRUE)      
  
  # Step 9: Plot result and presence data
  plot(pred, main = "SDM", legend = FALSE)
  points(pres_sp, pch = 16, col = "red") 
} 



  