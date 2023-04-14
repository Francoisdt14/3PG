# Simple Raster Cropping

library(terra)

# Set paths to the folders containing the shapefiles and rasters
shp_folder <- "D:/Radiation/study_areas"
rast_folder <- "D:/Radiation/30m_resample/U_thirteenN_buff_cropped"
rast_crop_folder <- "D:/Radiation/30m_cropped/U_thirteenN"

# Get a list of all the shapefiles in the shapefile folder
shp_files <- list.files(shp_folder, pattern = ".shp$", full.names = TRUE)

shp_file <- shp_files[6]

#shp_file <- "D:/Radiation/study_areas/Study_Area_M_eighteenS.shp"
# Loop through each shapefile and crop all rasters to that shapefile

    # Get the name of the shapefile without the extension
#shp_name <- tools::file_path_sans_ext(basename(shp_file))

# Read in the shapefile
shp <- vect(shp_file)

# Loop through each raster and crop it to the shapefile
rast_files <- list.files(rast_folder, pattern = ".tif$", full.names = TRUE)

for (rast_file in rast_files) {

        # Get the name of the raster without the extension
        rast_name <- tools::file_path_sans_ext(basename(rast_file))

        # Read in the raster
        rast <- rast(rast_file)

        shapefile_reproj <- project(shp, rast)

        # Crop the raster to the shapefile
        rast_crop <- crop(rast, shapefile_reproj)

        # Write the cropped raster to the new folder
        rast_path_cropped <- paste0(rast_crop_folder, "/", rast_name, ".tif")
        writeRaster(rast_crop, rast_path_cropped, overwrite = TRUE)
    }
test_rast
