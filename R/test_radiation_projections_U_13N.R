library(terra)
library(tictoc)
library(tidyverse)

# Single test to see if the project/crop works - it does in this order!

# Read the shapefile of study area
study_area <- vect("D:/Landcover/francois2/Shapefiles/Study_Area_M_eighteenS.shp")

# raster of interest - we have 12 months worth of radiation rasters
rast1 <- rast("D:/Radiation/1km_radiation/NACID_rswd_mon_norm_1971to2000_si_hist_v1_1.tif")

# project the study area shapefile - faster than projecting entire raster
projected_vector <- terra::project(study_area, "+proj=lcc +lat_0=40 +lon_0=-100 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

# crop the large raster using the projected study area
crop.rast <- terra::crop(rast1, projected_vector, mask = T)

# Now we project back to the projection we want (the tree mask)
tic()
crop.proj <- terra::project(crop.rast, "epsg:32618", threads = T, mask = T)
toc()

###############################

crop.resample <- resample(crop.proj, tree.mask, method = "near")

crop.resample <- focal(crop.resample, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

#mask
crop.mask <- terra::crop(crop.resample, tree.mask)

compareGeom(crop.mask, tree.mask)

############################################################################################################################

# Plot to check the extents etc.
plot(crop.proj)
plot(study_area, add = T, col = "red")

ext_mask <- ext(tree.mask)
ext_crop <- ext(crop.proj)

########

#this is what every single raster is aligned to
tree.mask <- rast("D:/BP_Layers/U_13N/tree_mask.tif")
# need to fill the NA's so that we can make sure we have full climate layers!
tree.mask[is.na(tree.mask)] <- 1

#####

# Loop for radiation: we need to project radiation to be able to work with it:
####################################
#
#shapefile of study area
study.area <- vect("D:/Landcover/francois2/Shapefiles/Study_Area_M_eighteenS.shp")
#project the study area shapefile
projected.vector <- terra::project(study.area, "+proj=lcc +lat_0=40 +lon_0=-100 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#folder with data
fl <- ("D:/Radiation/30m_resample/U_thirteenN_buff_cropped/")
#list of files in folder
input.files <- list.files(fl, pattern = ".tif$", full.names = TRUE)
#output folder
out.dir <- ("D:/BP_Layers/U_13N/3PG_flt/1_clipped_rad/")

# Loop through each raster input
for (i in 1:length(input.files)) {

  # load in big raster
  r <- terra::rast(input.files[i])

  #crop the large raster using the projected study area [faster this way]
  #crop.rast <- terra::crop(r, projected.vector)
  #crop.proj <- terra::project(crop.rast, "epsg:32618", threads = T, mask = T)

  crop.proj <- terra::project(r, tree.mask, threads = T, gdal = TRUE, by_util = TRUE)

  # align
  #crop.resample <- resample(crop.proj, tree.mask, method = "near")
  #crop.resample <- focal(crop.resample, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

  #mask
  #crop.mask <- terra::crop(crop.resample, tree.mask)

  file.base <- basename(input.files[i])
  #file.name <- gsub('.tif','',file.name)
  # Extract the number from the file name
  file_number <- as.numeric(gsub(".*_v1_(\\d+)\\.tif", "\\1", file.base))

  # Format the number with leading zeros
  file_name <- sprintf("Rad%02d", file_number)

  # write
  terra::writeRaster(crop.proj, paste(out.dir, file_name, ".tif", sep = ""), overwrite = T)

}
########
# This seems to work much faster for radiation so it is used above:

#test <- rast("D:/Radiation/30m_resample/U_thirteenN_buff_cropped/NACID_rswd_mon_norm_1971to2000_si_hist_v1_12.tif")
#test.project <- terra::project(test, tree.mask, threads = T, gdal = TRUE, by_util = TRUE)

test1 <- rast("D:/BP_Layers/U_13N/3PG_flt/1_clipped_rad/Rad12.tif")

compareGeom(tree.mask, test1)
###

########
tree.age.dat <- rast("D:/Landcover/francois5/Study_Area_U_thirteenN/mosaiced/age/Forest_Age_2019.dat")
plot(tree.age.dat)

tree.age.proj <- terra::project(tree.age.dat, tree.mask, threads = T, gdal = TRUE, by_util = TRUE)
terra::writeRaster(tree.age.proj, "D:/BP_Layers/U_13N/inputs/Forest_Age_2019_U_13N.tif", overwrite = T)

# We want to make sure tree age is aligned for relevant study areas
tree.age <- rast("D:/BP_Layers/U_13N/inputs/Forest_Age_2019_U_13N.tif")
# Clamp the max age to 1869 (150 years old)
tree.age.clamp <- clamp(tree.age, lower = 1869, upper = Inf)
# fill in NA values to 2010 if we want to simulate trees being elsewhere
tree.age.clamp[is.na(tree.age.clamp)] <- 2010
# create an output directory
out.dir = "D:/BP_Layers/U_13N/3PG_flt/4_30m_inputs_all/"
file.name = "Forest_Age_2019"
## check geometry
compareGeom(tree.age.clamp, tree.mask)
## since it is TRUE - we can write it directly
terra::writeRaster(tree.age.clamp, paste(out.dir, file.name, ".tif", sep = ""), overwrite = T)


############
# Align any layers to the tree.mask - in the case of M_18S it looks like the files are already aligned!

#this is what every single raster is aligned to
#tree.mask <- rast("D:/BP_Layers/M_18S/tree_mask.tif")

#this is what every single raster is aligned to
#tree.mask <- rast("D:/BP_Layers/M_18S/tree_mask.tif")
# need to fill the NA's so that we can make sure we have full climate layers!
tree.mask[is.na(tree.mask)] <- 1

scen  <- "Y2_S1"

#folder with data
fl <- paste0("D:/climate/Future/M_18S/tif_30m/", scen)
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

scen  <- ""

#folder with data
fp <- paste0("D:/climate/U_13N/output_tif_30m/", scen)
#list of files in folder
tiffs <- list.files(fp, pattern = ".tif$", full.names = TRUE, recursive = TRUE)

#output folder
output_folder <- paste0("D:/BP_Layers/U_13N/3PG_flt/3_frost/", scen) # check we have a /
dir.create(output_folder, showWarnings = FALSE)

# Loop through the list of files
for (file in tiffs) {
  # Check if the filename contains 'nffd'
  if (grepl("nffd", basename(file), ignore.case = TRUE)) {

    # Read the raster
    raster <- rast(file)

    # Extract the month number from the filename - 16th and 17th number (5th and 6th number for future scen)
    month_number <- as.integer(substr(basename(file), 16, 17))

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

# only radiation needs this step
fp = "D:/BP_Layers/U_13N/3PG_flt/1_clipped_rad"

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
check.rast1 <- rast("D:/BP_Layers/U_13N/3PG_flt/2_normalized_rad/Rad02.tif")
#check.rast1 <- rast("D:/BP_Layers/M_18S/3PG_flt/2_climate_align/Tmax02.tif")
plot(check.rast1)

compareGeom(check.rast1, tree.mask)

#####
# aggregate
#fp = "D:/BP_Layers/M_18S/3PG_flt/3_future_frost/Y5_S3"

fp = "D:/BP_Layers/U_13N/3PG_flt/3_frost"

tiffs = list.files(fp, pattern = ".tif$", full.names = T, recursive = T)

# Create a new folder to save the processed rasters
output_folder <- "D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all/"
dir.create(output_folder, showWarnings = FALSE)

for(t in tiffs){
  out.path = str_replace(t, "3_frost", "5_90m_inputs_all")

  if(file.exists(out.path)){next}

  r = rast(t) %>% terra::aggregate(., 100/res(.)[1], cores = 12)

  writeRaster(r, out.path)
}

###
# Compare geom
#check.rast <- rast("D:/BP_Layers/M_18S/3PG_flt/5_90m_inputs_all/Tmin03.tif")
#check.rast2 <- rast("D:/BP_Layers/M_18S/3PG_flt/5_90m_inputs_all/Forest_Age_2019.tif")
#compareGeom(check.rast, check.rast2)

###
# create float files

# Set the input and output directories
input_dir <- "D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all"

output_dir <- "D:/BP_Layers/U_13N/3PG_flt/6_90m_flt"

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

check.rast3 <- rast("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all/PPT09.tif")

###
# We want to find the lower left center of the X and Y dimension for the header files:
res_x <- res(check.rast3)[1]
res_y <- res(check.rast3)[2]

center_x <- xmin(check.rast3) + res_x / 2
center_y <- ymin(check.rast3) + res_y / 2
# Print the results
print(paste("Center X:", center_x))
print(paste("Center Y:", center_y))

# rewrite HDR files!

library(stringr)

# Set the directory where the .hdr and .flt files are located
# This will be different for every study site

#directory <- "Y:/Francois/flt_test_100_noNA"
#directory <- "D:/BP_Layers/M_18S/3PG_flt/6_90m_flt"
directory <- "D:/BP_Layers/U_13N/biomass_3PG/fert/S3/Y4_Output"
directory <- "I:/data_2024_05_02_deciduous/U_13N/S2_dec/Y4_output_test"
directory <- "D:/BP_Layers/U_13N/biomass_3PG/S2/Y4_output_test"




# Get the list of .hdr files in the directory
hdr_files <- list.files(directory, pattern = "\\.hdr$", full.names = TRUE)

# very important to write this correctly depending on raster size!!
# Process each .hdr file
for (hdr_file in hdr_files) {
    # Define the new content for the .hdr file
    new_content <- c(
        "NROWS          3334", # 90m = 3334 , 30m =
        "NCOLS          3334", # 90m 3334  , 30m
        "xllcenter         335542.427", # 90m =  325620 , 30m =
        "yllcenter         6657053.047", # 90m = 5236920 , 30m =
        "cellsize           90", # 90m = 90 , 30m =
        "nodata_value -9999.000000",
        "byteorder lsbfirst"
    )

  # Write the new content to the .hdr file, overwriting the existing contents
  writeLines(new_content, hdr_file)
}

check.rast4 <- rast("D:/BP_Layers/M_18S/3PG_flt/2_climate_align/NFFD05.tif")


###
# random final checks

check.raster5 <- rast("D:/BP_Layers/M_18S/3PG_flt/6_90m_flt_future/Y5_S1/Tmin09.flt")
check.raster6 <- rast("D:/BP_Layers/M_18S/3PG_flt/6_90m_flt/Forest_Age_2019.flt")

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



