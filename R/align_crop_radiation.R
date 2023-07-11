# Lots of rasters created.. need to make sure everything is aligned, masked, and cropped correctly

# THIS IS TAKEN DIRECTLY FROM 4_align_crop - slight changes

source("R/lib.R")

# Rasters to be aligned and re-cropped:
# Age
# Radiation - DONE
# REDO SPECIES

# Climate/other for remaining areas?

# List of the input rasters that we are masking and aligning
# List files here for radiation and disturbance - should be 12 rad, 1 disturbance
#fl = list.files("D:/BP_Layers/M_9S/climate/tmax", full.names = T, pattern = ".tif$")

fl = list.files("D:/Radiation/30m_crop_align/proj_test", full.names = T, pattern = ".tif$")

### re-sample DEM to another raster that we are sure are correct, in this case we want everything aligned to our 'tree_mask' raster
# MAKE SURE TO CHANGE OUTDIRECTORY!
align_crop <- function(raster, mask = "D:/BP_Layers/M_18S/tree_mask.tif", outdir = "D:/Radiation/30m_crop_align/M_eighteenS/",
                       overwrite = TRUE, focal.fun = "mean"){

  # basename of output
  file.name <- tools::file_path_sans_ext(basename(raster))

  # load mask
  mask <- terra::rast(mask)

  # load in - this is important to do!
  r <- terra::rast(raster)
  r2 <- terra::project(r, mask)

  # align ---- mask = mask_crop?
  ra <- resample(r, mask, method = "near")

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
global(mask, "notNA")
global(test, "notNA")

