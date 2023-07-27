# Read the shapefile

#shapefile of study area
study_area <- vect("D:/Landcover/francois2/Shapefiles/Study_Area_M_nineS.shp")

#raster of interest
rast1 <- rast("D:/Radiation/1km_radiation/NACID_rswd_mon_norm_1971to2000_si_hist_v1_1.tif")

#radiation data of study area with NAs
#rad <- rast("D:/Radiation/30m_crop_align/M_nineS/NACID_rswd_mon_norm_1971to2000_si_hist_v1_1.tif")

#project the study area shapefile 
projected_vector <- terra::project(study_area, "+proj=lcc +lat_0=40 +lon_0=-100 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#crop the large raster using the projected study area 
crop.rast <- terra::crop(rast1, projected_vector)

#this is what every single raster is aligned to 
tree.mask <- rast("D:/BP_Layers/outputs/tree_mask.tif")

tic()
crop.proj <- terra::project(crop.rast, tree.mask, threads = T)
toc()

plot(crop.proj)
plot(study_area, add = T, col = "red")

ext_mask <- ext(tree.mask)
ext_crop <- ext(crop.proj)


########


plot(ext_mask, col = 'red')
plot(ext_crop, col = 'blue', add = T)

plot(ext_mask, col = 'red')
plot(crop.proj, add = T)


#####

# Loop for radiation ...
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

# Align climate to tree.mask

#this is what every single raster is aligned to 
tree.mask <- rast("D:/BP_Layers/outputs/tree_mask.tif")

#folder with data
fl <- ("D:/Radiation/test_crop_project/climate")
#list of files in folder
input.files <- list.files(fl, pattern = ".tif$", full.names = TRUE)
#output folder
out.dir <- ("D:/Radiation/test_crop_project/climate_align/")

# Loop through each raster input
for (i in 1:length(input.files)) {
  
  # load in big raster
  r <- terra::rast(input.files[i])
  
  ## IS THIS NECESSARY?? - NOT FOR RADIATION
  ###
  # align
  ra <- resample(r, tree.mask, method = "near")
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
# tree age
tree.age <- rast("D:/BP_Layers/outputs/inputs/Forest_Age_2019.tif")
tree.age.clamp <- clamp(tree.age, lower = 1869, upper = Inf)
tree.age.clamp[is.na(tree.age.clamp)] <- 2010

out.dir = "D:/Radiation/test_crop_project/4_30m_inputs_all/"
file.name = "Forest_Age_2019"
## check geometry
compareGeom(tree.age.clamp, tree.mask)
## since it is TRUE - we can write it directly 
terra::writeRaster(tree.age.clamp, paste(out.dir, file.name, ".tif", sep = ""), overwrite = T)

#ra <- resample(tree.age.clamp, tree.mask, method = "near")
#ra <- focal(ra, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

#mask
#rm <- terra::crop(ra, tree.mask)
#terra::writeRaster(rm, paste(out.dir, file.name, ".tif", sep = ""), overwrite = T)

# divide radiation files by 10

# frost free days

library(terra)

fp <- "D:/Radiation/test_crop_project/2_climate_align"
tiffs <- list.files(fp, pattern = ".tif$", full.names = TRUE, recursive = TRUE)

# Create a new folder to save the processed rasters
output_folder <- "D:/Radiation/test_crop_project/3_frost"
dir.create(output_folder, showWarnings = FALSE)

# Loop through the list of files
for (file in tiffs) {
  # Check if the filename contains 'nffd'
  if (grepl("nffd", basename(file), ignore.case = TRUE)) {
    # Read the raster
    raster <- rast(file)
    
    # Perform the processing on the raster (31 - raster and round to nearest full day)
    processed_raster <- round(31 - raster)
    
    # Get the filename without the full path
    filename <- basename(file)
    
    # Create the output path for the processed raster
    output_path <- file.path(output_folder, filename)
    
    # Save the processed raster to the output folder
    writeRaster(processed_raster, output_path)
  }
}

###########
# divide radiation by 10 

library(terra); library(tidyverse)

fp = "D:/Radiation/test_crop_project/4_30m_inputs_all"

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

library(terra); library(tidyverse)

fp = "D:/Radiation/test_crop_project/4_30m_inputs_all"

tiffs = list.files(fp, pattern = ".tif$", full.names = T, recursive = T)

for(t in tiffs){
  out.path = str_replace(t, "4_30m_inputs_all", "5_90m_inputs_all")
  
  if(file.exists(out.path)){next}
  
  r = rast(t) %>% terra::aggregate(., 100/res(.)[1])
  
  writeRaster(r, out.path)
}

###
# Compare geom
check.rast <- rast("D:/Radiation/test_crop_project/5_90m_inputs_all/Rad06.tif")
check.rast2 <- rast("D:/Radiation/test_crop_project/5_90m_inputs_all/Forest_Age_2019.tif")
compareGeom(check.rast, check.rast2)

###
# create float files


# Loop to create float files

library(terra)

# Set the input and output directories
input_dir <- "D:/Radiation/test_crop_project/4_30m_inputs_all"

output_dir <- "D:/Radiation/test_crop_project/6_30m_flt"

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
  
  # Read the TIFF file using terra
  raster <- rast(tif_file)
  
  # Write the raster in FLT4S format to the output file
  writeRaster(raster, output_file, datatype = "FLT4S", overwrite = TRUE)
  
  # Print the file name to track progress
  cat("Converted:", tif_file, "\n")
}

check.rast3 <- rast("D:/Radiation/test_crop_project/6_30m_flt/Rad06.flt")

###

# Assuming you have already loaded or read your raster data
raster <- check.rast3

# Get the resolution of the raster (assuming x and y resolutions are the same)
res_x <- res(raster)[1]
res_y <- res(raster)[2]

# Get the dimensions of the raster
ncols <- ncol(raster)
nrows <- nrow(raster)

# Calculate the center x and y values of the lower pixel
center_x <- xmin(raster) + res_x * (ncols - 0.5)
center_y <- ymin(raster) + res_y * (nrows - 0.5)

# Print the results
print(paste("Center X:", center_x))
print(paste("Center Y:", center_y))

###
# rewrite HDR files!

library(stringr)
library(tools)

# Set the directory where the .hdr and .flt files are located

#directory <- "Y:/Francois/flt_test_100_noNA"
directory <- "D:/Radiation/test_crop_project/6_30m_flt/"
# Get the list of .hdr files in the directory
hdr_files <- list.files(directory, pattern = "\\.hdr$", full.names = TRUE)

# very important to write this correctly depending on raster size!!

# Process each .hdr file
for (hdr_file in hdr_files) {
  # Define the new content for the .hdr file
  new_content <- c(
    "NROWS          9745",
    "NCOLS          9746",
    "xllcenter         703617.363255707",
    "yllcenter         6612664.91357337",
    "cellsize           30.78478",
    "nodata_value -9999.000000",
    "byteorder lsbfirst"
  )
  
  # Write the new content to the .hdr file, overwriting the existing contents
  writeLines(new_content, hdr_file)
}

check.rast4 <- rast("D:/Radiation/test_crop_project/6_30m_flt/Tmin12.flt")


#####
# Calculate the center point
r_extent <- ext(check.rast)
center_x <- (r_extent[1] + r_extent[2]) / 2
center_y <- (r_extent[3] + r_extent[4]) / 2
center_point <- cbind(center_x, center_y)

###
# random final checks

check.raster5 <- rast("D:/Radiation/test_crop_project/6_30m_flt/Tmin02.flt")
check.raster6 <- rast("D:/Radiation/test_crop_project/6_30m_flt/Rad12.flt")

compareGeom(check.raster5, check.raster6)
