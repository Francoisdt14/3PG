

fl = list.files("D:/BP_Layers/M_9S/climate/tmax", full.names = T, pattern = ".tif$")
fl2 <- list.files("D:/BP_Layers/M_9S/climate/precipitation", full.names = T, pattern = ".tif$")
fl3 <- list.files("D:/BP_Layers/M_9S/climate/radiation", full.names = T, pattern = ".tif$")


for (i in 1:length(fl)) {
  
  rast <- rast(fl[i]) %>% project("EPSG:32609")
  #rast_a <- align_raster(iraster = rast, rtemplate = mask_crop)
  rast_a <- resample(rast, mask_crop, method = "near")
  
  rast_a = focal(rast_a, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)
  
  rast_m <- terra::crop(rast_a, mask_crop)
  rast_m <- terra::mask(rast_m, mask_crop)
  
  file.name <- basename(fl[i])
  file.name <- gsub('.tif','',file.name)
  
  #writeRaster(rast_m, "D:/BP_Layers/outputs/dem_crop_M_9S_test.tif")
  writeRaster(rast_m, paste("D:/BP_Layers/outputs/climate_2/", file.name, ".tif", sep = ""), overwrite = T)
  
}



### re-sample DEM to another raster that we are sure are correct 

dostuff <- function(raster, mask = "D:/BP_Layers/outputs/tree_mask.tif", outdir = "D:/BP_Layers/outputs/climate_2/", 
                    overwrite = TRUE, focal.fun = "mean"){
  
  # basename of output
  file.name <- tools::file_path_sans_ext(basename(raster))
  
  # load mask
  mask <- terra::rast(mask)
  
  # load in
  r <- terra::rast(raster) %>% project("EPSG:32609")
  
  # align
  rast_a <- resample(rast, mask_crop, method = "near")
  #ra <- align_raster(iraster = r, rtemplate = mask)
  
  if(str_detect(raster, "species")){ra <-  focal(ra, w = 3, fun = "modal", na.policy = "only", na.rm = T, expand = T)}
  else(ra <-  focal(ra, w = 3, fun = focal.fun, na.policy = "only", na.rm = T, expand = T))
  
  #mask
  rm <- terra::crop(ra, mask)
  rm <- terra::mask(ra, mask)
  
  # write
  terra::writeRaster(rm, paste(outdir, file.name, ".tif", sep = ""), overwrite = overwrite)
  
}

library(furrr)

plan(multisession, workers = 2)

furrr::future_walk(.x = fl2, .f = dostuff)


global(mask_crop, "notNA")
global(rast_m, "notNA")
