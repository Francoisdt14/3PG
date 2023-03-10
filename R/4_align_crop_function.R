# Lots of rasters created.. need to make sure everything is aligned, masked, and cropped correctly

source("R/lib.R")

# Rasters that currently need to be aligned and re-cropped:
#   climate rasters
#   DEM (elevation)
#   Species raster (eventually used for different species inputs)
#   Disturbance (stand initiation)

# List of the input rasters that we are masking and aligning

fl = list.files("D:/BP_Layers/M_9S/climate/tmax", full.names = T, pattern = ".tif$")

### re-sample DEM to another raster that we are sure are correct, in this case we want everything aligned to our 'tree_mask' raster

align_crop <- function(raster, mask = "D:/BP_Layers/outputs/tree_mask.tif", outdir = "D:/BP_Layers/outputs/climate_2/",
                    overwrite = TRUE, focal.fun = "mean"){

  # basename of output
  file.name <- tools::file_path_sans_ext(basename(raster))

  # load mask
  mask <- terra::rast(mask)

  # load in
  r <- terra::rast(raster) %>% project("EPSG:32609")

  # align
  ra <- resample(r, mask_crop, method = "near")

  if(str_detect(raster, "species")){ra <-  focal(ra, w = 3, fun = "modal", na.policy = "only", na.rm = T, expand = T)}
  else(ra <-  focal(ra, w = 3, fun = focal.fun, na.policy = "only", na.rm = T, expand = T))

  #mask
  rm <- terra::crop(ra, mask)
  rm <- terra::mask(ra, mask)

  # write
  terra::writeRaster(rm, paste(outdir, file.name, ".tif", sep = ""), overwrite = overwrite)

}

# Use the furrr package to achieve this quickly:

plan(multisession, workers = 2)

furrr::future_walk(.x = fl, .f = align_crop)

# We can check whether or not the rasters line up by comparing the numbers below!
global(mask_crop, "notNA")
global(rast_m, "notNA")


####################################
# Original loop before writing as a function...
# Initially manually did DEM, Species, and Disturbance rasters using this loop:

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

######################################################################################################################
