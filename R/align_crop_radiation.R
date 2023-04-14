# Lots of rasters created.. need to make sure everything is aligned, masked, and cropped correctly

# THIS IS TAKEN DIRECTLY FROM 4_align_crop

source("R/lib.R")

# Rasters to be aligned and re-cropped:
# Age
# Radiation - DONE
# REDO SPECIES

# Climate/other for remaining areas?

# List of the input rasters that we are masking and aligning
# List files here for radiation and disturbance - should be 12 rad, 1 disturbance
#fl = list.files("D:/BP_Layers/M_9S/climate/tmax", full.names = T, pattern = ".tif$")

fl = list.files("D:/BP_Layers/M_9S/species", full.names = T, pattern = ".dat$")

test <- rast(fl[1])
#raster <- fl[1]
plot(test)

species <- rast("D:/BP_Layers/outputs/inputs/leading-species_2019.tif")
age <- rast("D:/BP_Layers/outputs/inputs/Forest_Age_2019.tif")
mask <- rast("D:/BP_Layers/outputs/tree_mask.tif")

global(species, "notNA")
global(age, "notNA")
global(mask, "notNA")



### re-sample DEM to another raster that we are sure are correct, in this case we want everything aligned to our 'tree_mask' raster
# MAKE SURE TO CHANGE OUTDIRECTORY!
align_crop <- function(raster, mask = "D:/BP_Layers/outputs/tree_mask.tif", outdir = "D:/BP_Layers/outputs/inputs/",
                    overwrite = TRUE, focal.fun = "mean"){

  # basename of output
  file.name <- tools::file_path_sans_ext(basename(raster))

  # load mask
  mask <- terra::rast(mask)

  # load in - this is important to do!
  r <- terra::rast(raster) %>% project("EPSG:32609")

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


####
# different number of NA values..
# Check if there are any NA values in each raster

mask_na <- is.na(mask)

n_true_NA <- sum(as.vector(mask_na))

age_na <- is.na(test)

n_true <- sum(as.vector(age_na))

# Create a new raster to identify locations where mask has non-NA cells and test has NA cells
na_diff <- age_na - mask_na

na_diff2 <- mask_na - age_na


plot(mask_not_na_and_test_na, col = c('grey', 'red'))

plot(age_na & !mask_na, col = "green")

####################################
# Original loop before writing as a function...
# Initially manually did DEM, Species, and Disturbance rasters using this loop:

fl <- "D:/BP_Layers/M_9S/age/Forest_Age_2019.dat"

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
