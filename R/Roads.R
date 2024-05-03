# Clipping Road Buffers
library(terra)
# areas

#area = "M_18S"
#area = "M_11S"
#area = "U_18S"
area = "U_15S"
#area = "M_9S"
#area = "U_13N"

## test for study area M 18
# load in roads - only once

#roads <- vect("D:/Roads/lrnf000r23a_e.shp")

# load in study area
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_eighteenS.shp')
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_elevenS.shp')
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_U_eighteenS.shp')
vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_U_fifteenS.shp')
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_nineS.shp')
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_U_thirteenN.shp')

#vect.study.buff <- terra::buffer(vect.study, 50000)

# crop roads to study area
#roads.study <- crop(roads, vect.study)

road.ON <- vect("D:/Roads/NRN_RRN_ON_SHAPE/NRN_ON_17_0_SHAPE_en/NRN_ON_17_0_ROADSEG.shp")
road.MB <- vect("D:/Roads/NRN_MB_6_0_SHAPE_en/NRN_MB_6_0_ROADSEG.shp")

road.QC <- vect("")
road.BC <- vect("")
road.NT <- vect("")
road.AB <- vect("")


vect.study.1 <- terra::project(vect.study,"EPSG:4617")
vect.study.2 <- terra::project(vect.study, road.2)


roads.study.1 <- crop(road.1, vect.study.1)
roads.study.2 <- crop(road.2, vect.study.2)

roads.study.1.proj <- terra::project(roads.study.1, study.mask)
roads.study.2.proj <- terra::project(roads.study.2, study.mask)

roads.study <- terra::union(roads.study.1.proj, roads.study.2.proj)

#roads.study.test <- crop(roads, vect.study)

# Check if clipped roads have any geometry
if (nrow(roads.study) == 0) {
    # No roads clipped, skip to next area
    cat("No roads clipped for ", area)
}

# create a roads buffer
roads.study.buffer <- terra::buffer(roads.study, 1000)
#roads.study.buffer <- terra::buffer(roads.study, 50000)

# read in study area mask
study.mask <- rast(paste("D:/BP_Layers/", area, "/landcover/Sh_90m.tif", sep = ""))

# project
#roads.study.buffer.proj <- terra::project(roads.study.buffer, study.mask)
# aggregate
#roads.study.buffer.proj.agg <- terra::aggregate(roads.study.buffer.proj, dissolve = TRUE)

### change order and check time

#aggregate
roads.study.buffer.agg <- terra::aggregate(roads.study.buffer, dissolve = TRUE)
#project
roads.study.buffer.proj.agg <- terra::project(roads.study.buffer.agg, study.mask)


# crop back to extent of mask if necessary
#roads.study.buffer.proj.agg.crop <- terra::crop(roads.study.buffer.proj.agg, study.mask)

# mask to the study area mask
study.mask.road <- terra::mask(study.mask, roads.study.buffer.proj.agg)

plot(roads.study.buffer.proj.agg)
plot(study.mask, add = T)
plot(study.mask.road, add = T, col = "red")

#study.mask.road


writeRaster(study.mask.road, paste("D:/BP_Layers/", area, "/landcover/Sh_90m_road_50km.tif", sep = ""))

####################################################################################################################################

# In a loop:

# Define study areas and their corresponding vector data paths
study_areas <- c(
    "M_18S" = "D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_eighteenS.shp",
    "M_11S" = "D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_elevenS.shp",
    "U_18S" = "D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_U_eighteenS.shp",
    "U_15S" = "D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_U_fifteenS.shp",
    "M_9S" = "D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_nineS.shp",
    "U_13N" = "D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_U_thirteenN.shp"
)

# Define road data path (replace with your actual path)
roads_path <- "D:/Roads/lrnf000r23a_e.shp"
# Load roads data
#roads <- vect(roads_path)

# Define output folder for raster files (replace with your desired path)
output_folder <- "D:/BP_Layers/"

# Loop through study areas and buffer distances
for (study_area in names(study_areas)) {
    # Get study area vector data path from lookup table
    study_vector_path <- study_areas[study_area]
    vect.study <- vect(study_vector_path)

    # Buffer the study area (50,000 meters)
    #vect.study.buff <- terra::buffer(vect.study, 50000)

    # Clip roads to study area
    roads.study <- crop(roads, vect.study)

    # Check if clipped roads have any geometry
    if (nrow(roads.study) == 0) {
        # No roads clipped, skip to next area
        cat("No roads clipped for ", study_area, ". Skipping...\n")
        next
    }

    # Read study area mask
    study_mask_path <- paste0(output_folder, study_area, "/landcover/Sh_90m.tif")
    study.mask <- rast(study_mask_path)

    # Define buffer distances (1 km and 50 km)
    buffer_distances <- c(1000, 50000)

    for (buffer_distance in buffer_distances) {
        # Create a buffer around roads in the study area
        roads.study.buffer <- terra::buffer(roads.study, buffer_distance)

        # Aggregate the road buffer (dissolve into one polygon)
        roads.study.buffer.agg <- terra::aggregate(roads.study.buffer, dissolve = TRUE)

        # Project the road buffer to the mask projection (if necessary)
        roads.study.buffer.proj <- terra::project(roads.study.buffer.agg, study.mask)

        # Mask the study area with the road buffer
        study.mask.road <- terra::mask(study.mask, roads.study.buffer.proj)

        # Define output raster filename
        output_filename <- paste0(output_folder, study_area, "/landcover/Sh_90m_road_", buffer_distance/1000, "km.tif")

        # Write the masked study area as a raster
        writeRaster(study.mask.road, output_filename)

        # Print a message for clarity
        cat("Created raster:", output_filename, "\n")
    }
}

#################

# Define the study areas
study_areas <- c("M_18S", "M_11S", "U_18S", "U_15S", "M_9S")

# Create an empty dataframe to store the results
results_df <- data.frame(
    study_area = character(),
    remaining_1km = numeric(),
    remaining_1km_perc = numeric(),
    remaining_50km = numeric(),
    remaining_50km_perc = numeric(),
    stringsAsFactors = FALSE
)

# Loop through the study areas and calculate the metrics
for (study_area in study_areas) {
    # Read in study area mask
    study_mask <- rast(paste("D:/BP_Layers/", study_area, "/landcover/Sh_90m.tif", sep = ""))

    # Read in road masks
    road_mask_1km <- rast(paste("D:/BP_Layers/", study_area, "/landcover/Sh_90m_road_1km.tif", sep = ""))
    road_mask_50km <- rast(paste("D:/BP_Layers/", study_area, "/landcover/Sh_90m_road_50km.tif", sep = ""))

    # Calculate percent of cells compared to the study mask that exist in road
    study_pixels <- global(study_mask, "notNA")
    road_pixels_1km <- global(road_mask_1km, "notNA")
    road_pixels_50km <- global(road_mask_50km, "notNA")

    remaining_1km <- road_pixels_1km / study_pixels
    remaining_1km_perc <- remaining_1km * 100

    remaining_50km <- road_pixels_50km / study_pixels
    remaining_50km_perc <- remaining_50km * 100

    # Add the results to the dataframe using bind_rows
    results_df <- bind_rows(results_df, data.frame(
        study_area = study_area,
        remaining_1km = remaining_1km,
        remaining_1km_perc = remaining_1km_perc,
        remaining_50km = remaining_50km,
        remaining_50km_perc = remaining_50km_perc
    ))
}

# Print the results dataframe
print(results_df)

#####################

# Define the study areas
study_areas <- c("M_18S", "M_11S", "U_18S", "U_15S", "M_9S")

# Create an empty dataframe to store the results
results_df <- data.frame(
    study_area = character(),
    remaining_1km = numeric(),
    remaining_1km_perc = numeric(),
    remaining_50km = numeric(),
    remaining_50km_perc = numeric(),
    stringsAsFactors = FALSE
)

# Loop through the study areas and calculate the metrics
for (study_area in study_areas) {
    # Read in study area mask
    study_mask <- rast(paste("D:/BP_Layers/", study_area, "/landcover/Sh_90m.tif", sep = ""))

    # Read in road masks
    road_mask_1km <- rast(paste("D:/BP_Layers/", study_area, "/landcover/Sh_90m_road_1km.tif", sep = ""))
    road_mask_50km <- rast(paste("D:/BP_Layers/", study_area, "/landcover/Sh_90m_road_50km.tif", sep = ""))

    # Calculate percent of cells compared to the study mask that exist in road
    study_pixels <- global(study_mask, "notNA")
    road_pixels_1km <- global(road_mask_1km, "notNA")
    road_pixels_50km <- global(road_mask_50km, "notNA")

    # Calculate remaining values and percentages within the loop
    remaining_1km <- sum(road_pixels_1km) / sum(study_pixels)
    remaining_1km_perc <- remaining_1km * 100

    remaining_50km <- sum(road_pixels_50km) / sum(study_pixels)
    remaining_50km_perc <- remaining_50km * 100

    # Create a data frame with results for this study area
    temp_df <- data.frame(
        study_area = study_area,
        remaining_1km = remaining_1km,
        remaining_1km_perc = remaining_1km_perc,
        remaining_50km = remaining_50km,
        remaining_50km_perc = remaining_50km_perc
    )

    # Append the temporary data frame to the results dataframe
    results_df <- rbind(results_df, temp_df)
}

print(results_df)

