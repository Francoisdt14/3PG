library(terra)

# Single test to see if the project/crop works - it does in this order!

# Read the shapefile of study area
study_area <- vect("D:/Landcover/francois2/Shapefiles/Study_Area_M_nineS.shp")

# raster of interest - we have 12 months worth of radiation rasters
rast1 <- rast("D:/Radiation/1km_radiation/NACID_rswd_mon_norm_1971to2000_si_hist_v1_1.tif")

# project the study area shapefile - faster than projecting entire raster
projected_vector <- terra::project(study_area, "+proj=lcc +lat_0=40 +lon_0=-100 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

# crop the large raster using the projected study area
crop.rast <- terra::crop(rast1, projected_vector)

# Now we project back to the projection we want (the tree mask)
tic()
crop.proj <- terra::project(crop.rast, tree.mask, threads = T)
toc()

# Plot to check the extents etc.
plot(crop.proj)
plot(study_area, add = T, col = "red")

ext_mask <- ext(tree.mask)
ext_crop <- ext(crop.proj)


########

#this is what every single raster is aligned to
tree.mask <- rast("D:/BP_Layers/outputs/tree_mask.tif")

#####

# Loop for radiation: we need to project radiation to be able to work with it:
####################################
#
#shapefile of study area
study.area <- vect("D:/Landcover/francois2/Shapefiles/Study_Area_M_nineS.shp")
#project the study area shapefile
projected.vector <- terra::project(study.area, "+proj=lcc +lat_0=40 +lon_0=-100 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
#this is what every single raster is aligned to
tree.mask <- rast("D:/BP_Layers/outputs/tree_mask.tif")

#folder with data
fl <- ("D:/Radiation/1km_radiation/")
#list of files in folder
input.files <- list.files(fl, pattern = ".tif$", full.names = TRUE)
#output folder
out.dir <- ("D:/Radiation/test_crop_project/M_nineS/")

# Loop through each raster input
for (i in 1:length(input.files)) {

    # load in big raster
  r <- terra::rast(input.files[i])

  #crop the large raster using the projected study area [faster this way]
  crop.rast <- terra::crop(r, projected.vector)

  #project to the projection of tree.mask - what everything is matched to
  crop.proj <- terra::project(crop.rast, tree.mask, threads = T)

  ## IS THIS NECESSARY?? - NOT FOR RADIATION
    ###
  # align
  #ra <- resample(crop_proj, tree.mask, method = "near")
  #ra <- focal(ra, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

  #mask
  #rm <- terra::crop(ra, tree.mask)

  ###

  file.name <- basename(input.files[i])
  file.name <- gsub('.tif','',file.name)

  # write
  terra::writeRaster(crop.proj, paste(out.dir, file.name, ".tif", sep = ""), overwrite = T)

  }

########
# We want to make sure tree age is aligned for relevant study areas
tree.age <- rast("D:/BP_Layers/outputs/inputs/Forest_Age_2019.tif")
# Clamp the max age to 1869 (150 years old)
tree.age.clamp <- clamp(tree.age, lower = 1869, upper = Inf)
# fill in NA values to 2010 if we want to simulate trees being elsewhere
tree.age.clamp[is.na(tree.age.clamp)] <- 2025
# create an output directory
out.dir = "D:/BP_Layers/M_9S/3PG_flt/4_30m_inputs_all/"
file.name = "Forest_Age_2019_2025"
## check geometry
compareGeom(tree.age.clamp, tree.mask)
## since it is TRUE - we can write it directly
terra::writeRaster(tree.age.clamp, paste(out.dir, file.name, ".tif", sep = ""), overwrite = T)

############
# Align any layers to the tree.mask

#this is what every single raster is aligned to
tree.mask <- rast("D:/BP_Layers/outputs/tree_mask.tif")

scen  <- "Y5_S3"

#folder with data
fl <- paste0("D:/climate/Future/M_9S/tif_30m/", scen)
#list of files in folder
input.files <- list.files(fl, pattern = ".tif$", full.names = TRUE)
#output folder
out.dir <- paste0("D:/BP_Layers/M_9S/3PG_flt/2_future_climate_align/", scen, "/") # don't forget the forward /

# Loop through each raster input
for (i in 1:length(input.files)) {

  # load in big raster
  r <- terra::rast(input.files[i])

  ## IS THIS NECESSARY?? - NOT FOR RADIATION
  ###
  # align
  ra <- resample(r, tree.mask, method = "near", threads = T)
  ra <- focal(ra, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

  #mask
  rm <- terra::crop(ra, tree.mask)

  ###

  file.name <- basename(input.files[i])
  file.name <- gsub('.tif','',file.name)

  # write
  terra::writeRaster(rm, paste(out.dir, file.name, ".tif", sep = ""), overwrite = T)

}

###########
# frost free days

library(terra)

fp <- "D:/BP_Layers/M_9S/3PG_flt/2_future_climate_align/Y5_S3/"
tiffs <- list.files(fp, pattern = ".tif$", full.names = TRUE, recursive = TRUE)

# Create a new folder to save the processed rasters
output_folder <- "D:/BP_Layers/M_9S/3PG_flt/3_future_frost/Y5_S3"
dir.create(output_folder, showWarnings = FALSE)

# Loop through the list of files
for (file in tiffs) {
  # Check if the filename contains 'nffd'
  if (grepl("nffd", basename(file), ignore.case = TRUE)) {

    # Read the raster
    raster <- rast(file)

    # Extract the month number from the filename - 5th and 6th number
    month_number <- as.integer(substr(basename(file), 5, 6))

    # Set the correct number of days based on the month
    if (month_number %in% c(04, 06, 09, 11)) {
      # April, June, September, November have 30 days
      num_days <- 30
    } else if (month_number == 02) {
      # February has 28 days
      num_days <- 28
    } else {
      # All other months have 31 days
      num_days <- 31
    }

    # Perform the processing on the raster (31 - raster and round to nearest full day)
    processed_raster <- round(num_days - raster)

    # Get the filename without the full path
    filename <- basename(file)

    # Create the output path for the processed raster
    output_path <- file.path(output_folder, filename)

    # Save the processed raster to the output folder
    writeRaster(processed_raster, output_path)
  }
}

###########
# divide radiation by 10 - important step!!! - this is for the radiation provided by Dr. Hember

library(terra); library(tidyverse)

# only radiation needs this step
fp = "D:/BP_Layers/M_9S/3PG_flt/1_clipped_rad"

tiffs = list.files(fp, pattern = ".tif$", full.names = T, recursive = T)

for(t in tiffs){

  out.path = str_replace(t, "1_clipped_rad", "2_normalized_rad") #%>%
    #str_replace(".tif", ".flt")

  if(file.exists(out.path)){next}

  r = rast(t)
  r2 <- r / 10

  writeRaster(r2, out.path)
}

#####

# check alignment ...

#####
# aggregate
fp = "D:/BP_Layers/M_9S/3PG_flt/2_future_climate_align/Y5_S3"

tiffs = list.files(fp, pattern = ".tif$", full.names = T, recursive = T)

# Create a new folder to save the processed rasters
output_folder <- "D:/BP_Layers/M_9S/3PG_flt/5_90m_inputs_future/Y5_S3/"
dir.create(output_folder, showWarnings = FALSE)

for(t in tiffs){
  out.path = str_replace(t, "2_future_climate_align", "5_90m_inputs_future")

  if(file.exists(out.path)){
      cat("Skipping:", out.path, "\n")
      next
      }

  r = rast(t) %>% terra::aggregate(., 100/res(.)[1], cores = 12)

  writeRaster(r, out.path)
}

###
# Compare geom
check.rast <- rast("D:/BP_Layers/M_9S/3PG_flt/5_90m_inputs_future/Y2_S2/NFFD01.tif")
check.rast2 <- rast("D:/BP_Layers/M_9S/3PG_flt/5_90m_inputs_all/Forest_Age_2019.tif")
compareGeom(check.rast, check.rast2)

###
# create float files


# Loop to create float files

# Set the input and output directories
input_dir <- "D:/BP_Layers/M_9S/3PG_flt/5_90m_inputs_future/Y5_S3"

output_dir <- "D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_future/Y5_S3"

# Get the list of TIFF files in the input directory
tif_files <- list.files(input_dir, pattern = ".tif$", full.names = TRUE)
# Create the output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE)
# Loop over each TIFF file
for (tif_file in tif_files) {
  # Build the output file path by replacing the input directory with the output directory
  output_file <- gsub(input_dir, output_dir, tif_file)

  # Change the file extension to ".flt"
  output_file <- sub("\\.tif$", ".flt", output_file)

  if (file.exists(output_file)) {
    cat(" -- file already exists")
    next
  }

  # Read the TIFF file using terra
  raster <- rast(tif_file)

  # Write the raster in FLT4S format to the output file
  writeRaster(raster, output_file, datatype = "FLT4S", overwrite = TRUE)

  # Print the file name to track progress
  cat("Converted:", tif_file, "\n")
}

check.rast3 <- rast("D:/BP_Layers/M_9S/3PG_flt/4_30m_inputs_all/PPT11.tif")

###
# We want to find the lower left center of the X and Y dimension for the header files:
res_x <- res(check.rast3)[1]
res_y <- res(check.rast3)[2]

center_x <- xmin(raster) + res_x / 2
center_y <- ymin(raster) + res_y / 2
# Print the results
print(paste("Center X:", center_x))
print(paste("Center Y:", center_y))

# rewrite HDR files!

library(stringr)

# Set the directory where the .hdr and .flt files are located

#directory <- "Y:/Francois/flt_test_100_noNA"
directory <- "D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_future/Y5_S1"
# Get the list of .hdr files in the directory
hdr_files <- list.files(directory, pattern = "\\.hdr$", full.names = TRUE)

# very important to write this correctly depending on raster size!!

# Process each .hdr file
for (hdr_file in hdr_files) {
  # Define the new content for the .hdr file
  new_content <- c(
    "NROWS          3249", # 90m = 3249 , 30m = 9745
    "NCOLS          3249", # 90m 3249  , 30m 9746
    "xllcenter         403650.448601884", # 90m = 403650.448601884 , 30m = 403619.663820003
    "yllcenter         6312667.21413766", # 90m = 6312667.21413766" , 30m = 6312636.42935578"
    "cellsize           92.35435", # 90m = 92.35435 , 30m = 30.78478
    "nodata_value -9999.000000",
    "byteorder lsbfirst"
  )

  # Write the new content to the .hdr file, overwriting the existing contents
  writeLines(new_content, hdr_file)
}

check.rast4 <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_future/ssp245/Tmin12.flt")


###
# random final checks

check.raster5 <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/dem_90.flt")
check.raster6 <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt/Rad12.flt")

compareGeom(check.raster5, check.raster6)



## We need to amend the .hdr files that are produced as outputs from 3PG:


# Set the path to the folder containing .flt and .hdr files
folder_path <- "D:/3PG_Cplusplus/_delete"  # Replace with the path to your folder

# List .hdr files in the folder
hdr_files <- list.files(folder_path, pattern = "\\.hdr$", full.names = TRUE)

# Add projection information to each .hdr file
for (hdr_file in hdr_files) {
  hdr_content <- readLines(hdr_file)

  # Add the projection information
  projection_info <- c(
    "projection UTM",
    "zone 9",
    "datum WGS84",
    "units meters"
  )

  # Find the line number with "cellsize" and insert the projection info after it
  cellsize_index <- which(grepl("cellsize", hdr_content))
  hdr_content <- append(hdr_content, projection_info, after = cellsize_index)

  # Write the modified content back to the .hdr file
  writeLines(hdr_content, con = hdr_file)

  cat("Projection information added to", basename(hdr_file), "\n")
}




