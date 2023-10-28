
##############################

library(terra)
library(dplyr)

# Load your vector polygons and mask raster
boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")
mask_crop <- rast("D:/BP_Layers/outputs/tree_mask.tif")

# Define the folder path for the input rasters
folder_path <- "D:/BP_Layers/M_9S/3PG_flt/4_30m_inputs_all"

# Get a list of file names in the folder
raster_files <- list.files(folder_path, pattern = "*.tif$", full.names = TRUE)

# Define which boxes you want to process
vals <- c(732:734, 763:765, 794:796)

# Loop through the vector polygons and crop the rasters
for (i in vals) {

    polygon <- boxes.v[boxes.v$name == i,]
    target_folder <- paste0("Y:/Francois/_Vaughan/", i)

    # Create the target folder if it doesn't exist
    if (!dir.exists(target_folder)) {
        dir.create(target_folder, recursive = TRUE)
    }

    # Loop through the list of rasters and extract values
    for (j in 1:length(raster_files)) {
        file_path <- raster_files[j]
        raster <- rast(file_path)

        # Crop the masked layer
        sub.mask <- crop(mask_crop, polygon)

        # Crop the raster to the current polygon
        cropped_raster <- crop(raster, polygon) %>% mask(sub.mask)

        # Get the original raster file name
        original_name <- basename(file_path)

        # Create the output file path by combining the target folder and original name
        output_path <- file.path(target_folder, original_name)

        # Write the cropped raster to the target folder with its original name
        writeRaster(cropped_raster, filename = output_path)
    }
}



##############################


# Load your vector polygons (boxes.shp)
boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")

# Define the values that represent the names of the smaller boxes you want to dissolve
vals <- c(732:734, 763:765, 794:796)

# Extract the smaller boxes identified by vals
smaller_boxes <- boxes.v[boxes.v$name %in% vals,]

smaller_box_extents <- ext(smaller_boxes)

combined_extent <- ext(smaller_box_extents)

# Extract the extent of the merged polygon
combined_extent <- ext(merged_polygon)

# Create a polygon representing the larger square using the combined extent
larger_square <- vect(combined_extent)

crs(larger_square) <- crs(boxes.v)

##########################################################

# CROP TO LARGER SQUARE

# Define the folder path for the input rasters
folder_path <- "D:/BP_Layers/M_9S/3PG_flt/4_30m_inputs_all"

# Get a list of file names in the folder
raster_files <- list.files(folder_path, pattern = "*.tif$", full.names = TRUE)

polygon <- larger_square
target_folder <- "Y:/Francois/_Vaughan/combined_areas"

    # Loop through the list of rasters and extract values
    for (j in 1:length(raster_files)) {
        file_path <- raster_files[j]
        raster <- rast(file_path)

        # Crop the masked layer
        sub.mask <- crop(mask_crop, polygon)

        # Crop the raster to the current polygon
        cropped_raster <- crop(raster, polygon) %>% mask(sub.mask)

        # Get the original raster file name
        original_name <- basename(file_path)

        # Create the output file path by combining the target folder and original name
        output_path <- file.path(target_folder, original_name)

        # Write the cropped raster to the target folder with its original name
        writeRaster(cropped_raster, filename = output_path)
    }









########################################################

# Compare geometries




