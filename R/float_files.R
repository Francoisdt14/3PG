
# test writing float files

test_rast <- rast("D:/BP_Layers/outputs/inputs/Forest_Age_2019.tif")

raster_clamped <- clamp(test_rast, lower = 1869, upper = Inf)

raster_clamped[is.na(raster_clamped)] <- 1850

writeRaster(raster_clamped, "D:/BP_Layers/flt_test/age.flt", datatype = "FLT4S", overwrite = TRUE)

#######################

library(terra)

# simple way of creating frost days from frost free days

# Set the input and output directories
input_dir <- "Y:/Francois/flt_test_100_noNA/nffd_flt/"
output_dir <- "Y:/Francois/flt_test_100_noNA/frost_flt"

# Get the list of TIFF files in the input directory
tif_files <- list.files(input_dir, pattern = ".tif$", full.names = TRUE)

# Create the output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE)

# Loop over each TIFF file
for (tif_file in tif_files) {
  # Read the TIFF file using terra
  raster <- rast(tif_file)

  # Subtract 31 from each pixel value
  raster_frost <- 31 - raster

  # Build the output file path by replacing the input directory with the output directory
  output_file <- gsub(input_dir, output_dir, tif_file)

  # Create the output directory if it doesn't exist
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # Write the modified raster to the output file
  writeRaster(raster_frost, output_file, filetype = "GTiff", overwrite = TRUE)

  # Print the file name to track progress
  cat("Processed:", tif_file, "\n")
}

##############################

# Loop to create float files

library(terra)

# Set the input and output directories
#input_dir <- "D:/BP_Layers/flt_test/climate"
input_dir <- "D:/BP_Layers/TWI_Outputs"

output_dir <- "D:/BP_Layers/TWI_Outputs/FLT"

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

###########################

###########################
#Cropping layers to small areas if needed
library(terra)

# Set the input and output directories
input_dir <- "D:/BP_Layers/flt_test/frost_flt"
output_dir <- "D:/BP_Layers/flt_test/tile/frost_flt"

# Reference raster for cropping
reference_raster <- rast("D:/BP_Layers/outputs/crops/889_test/varied/alpha/both_vary/biom_full889.tif")

# Get the list of float files in the input directory
float_files <- list.files(input_dir, pattern = ".flt$", full.names = TRUE)

# Create the output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE)

# Loop over each float file
for (float_file in float_files) {
  # Read the float file using terra
  raster_float <- rast(float_file)

  # Crop the float raster to the reference raster's extent
  cropped_raster <- crop(raster_float, reference_raster)

  # Build the output file path by replacing the input directory with the output directory
  output_file <- gsub(input_dir, output_dir, float_file)

  # Create the output directory if it doesn't exist
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # Write the cropped raster to the output file
  writeRaster(cropped_raster, output_file, datatype = "FLT4S", overwrite = TRUE)

  # Print the file name to track progress
  cat("Cropped:", float_file, "\n")
}


########

test_1 <- rast("D:/BP_Layers/flt_test/tile/climate_flt/PPT01.flt")
test_2 <- rast("D:/BP_Layers/flt_test/tile/age_flt/age.flt")

global(test_1, "notNA")

###
# need a tif of the relevant raster to get our information for rewriting header files..
check.rast <- rast()


# We want to find the lower left center of the X and Y dimension for the header files:
res_x <- res(check.rast)[1]
res_y <- res(check.rast)[2]

center_x <- xmin(check.rast) + res_x / 2
center_y <- ymin(check.rast) + res_y / 2
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

####################################################
# Aggregation

library(terra); library(tidyverse)

fp = "Y:/Francois/flt_test_100_noNA/output_tif_30m"

tiffs = list.files(fp, pattern = ".tif$", full.names = T, recursive = T) %>%
  str_subset("nffd", negate = T)

for(t in tiffs){
  which(t %in% tiffs)
  out.path = str_replace(t, "output_tif_30m", "climate_flt") %>%
    str_replace(".tif", ".flt")

  if(file.exists(out.path)){next}

  r = rast(t) %>% aggregate(., 100/res(.)[1])

  writeRaster(r, out.path, datatype = "FLT4S")
}


#####################################################
# Looking at float file outputs using spatial 3PG runs
dbh_flt <- rast("D:/BP_Layers/flt_test/tile/Output2/dbh.flt")
lai_flt <- rast("D:/BP_Layers/flt_test/tile/Output2/LAI.flt")
vol_flt <- rast("D:/BP_Layers/flt_test/tile/Output2/s_vol.flt")
biom_flt <- rast("D:/BP_Layers/flt_test/tile/Output2/totalw.flt")

biom_r <- rast("D:/BP_Layers/outputs/crops/889_test/varied/biom_full889.tif")
dbh_r <- rast("D:/BP_Layers/outputs/crops/889_test/varied/dbh889.tif")
lai_r <- rast("D:/BP_Layers/outputs/crops/889_test/varied/lai889.tif")
vol_r <- rast("D:/BP_Layers/outputs/crops/889_test/varied/volume889.tif")

biom_r_alpha <- rast("D:/BP_Layers/outputs/crops/889_test/varied/alpha/asw_150/biom_full889.tif")

vol_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/volume_ntems_full.tif")
vol_ntems_889 <- crop(vol_ntems, vol_r)
vol_ntems_889 <- vol_ntems_889 / 10

global(vol_flt, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
global(vol_r, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
global(vol_ntems_889, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)

global(vol_flt, 'notNA')
global(vol_r, 'notNA')

lat <- read.csv("D:/BP_Layers/outputs/crops/lat/889.csv")
