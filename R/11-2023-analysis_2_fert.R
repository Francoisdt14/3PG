# Load required packages
library(terra)
library(tidyverse)
library(viridis)
library(ggrepel)
library(ggplot2)
library(gridExtra)  # For grid.arrange

# Overall Comparison


##################################################################################
# Define study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N") # , "U_13N"
#study_areas <- c("U_18S")

# Loop through study areas
for (area in study_areas) {

    # Set the output directory based on the current study area
    output_dir <- file.path("D:/BP_Layers", area, "analysis/fert")
    # Check if the folder exists, and create it if it doesn't
    if (!file.exists(output_dir)) {
        dir.create(output_dir)
    }

    # Load in the stem and foliage values at 2080 for each scenario
    s1.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/fert/S1/Y3_Output/ws205007.flt"))
    s1.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/fert/S1/Y3_Output/wf205007.flt"))

    s2.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/fert/S2/Y3_Output/ws205007.flt"))
    s2.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/fert/S2/Y3_Output/wf205007.flt"))

    s3.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/fert/S3/Y3_Output/ws205007.flt"))
    s3.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/fert/S3/Y3_Output/wf205007.flt"))

    # calculate agb for each scenario in 2080
    s1 <- s1.ws + s1.wf
    s2 <- s2.ws + s2.wf
    s3 <- s3.ws + s3.wf

    # Load the masks
    fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/fao_forest_90m.tif"))
    Sh_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_90m.tif"))
    ShBy_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_Bry_90m.tif"))
    Sh_fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_fao_90m.tif"))
    ShBy_fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_Bry_fao_90m.tif"))

    # Get the EPSG code from fao.mask
    epsg_code <- crs(fao_mask, describe = T)$code
    # Set the CRS of s1 using the extracted EPSG code
    crs(s1) <- paste0("EPSG:", epsg_code)
    crs(s2) <- paste0("EPSG:", epsg_code)
    crs(s3) <- paste0("EPSG:", epsg_code)

    # Create a list of scenarios and masks
    scenarios <- list(s1, s2, s3)
    mask_names <- c("fao_mask", "Sh_mask", "ShBy_mask", "Sh_fao_mask", "ShBy_fao_mask")

    # Create and save masked rasters
    masked_rasters <- list()

    for (i in 1:length(scenarios)) {
        scenario <- scenarios[[i]]
        scenario_name <- paste("s", i, sep = "")

        for (mask in mask_names) {
            # Apply the mask
            masked_scenario <- mask(scenario, get(mask))

            # Create the file path and name for the output raster
            output_path <- file.path(output_dir, paste(scenario_name, mask, sep = "_"))

            # Save the masked raster
            writeRaster(masked_scenario, filename = paste(output_path, ".tif", sep = ""), overwrite = TRUE)

            masked_rasters[[output_path]] <- masked_scenario
        }
    }
}


# Specify study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")

# Initialize an empty dataframe to store the final summary statistics
final_summary_df_fert <- data.frame(Study_Area = character(), Raster = character(), Mean = numeric(), Sum = numeric(),
                               Min = numeric(), Max = numeric(),
                               SD = numeric(), p25 = numeric(), p75 = numeric(),
                               stringsAsFactors = FALSE)

# Loop through study areas
for (area in study_areas) {
    # Specify the path to the folder containing rasters
    folder_path <- file.path("D:/BP_Layers", area, "analysis/fert")

    # List all the raster files in the folder
    raster_files <- list.files(path = folder_path, pattern = "\\.tif$", full.names = TRUE)

    # Loop through each raster file, load it, calculate summary statistics, and add to the data frame
    for (file in raster_files) {
        # Extract the name you initially assigned to each raster
        raster_name <- sub(".*/(s[0-9]+_.+)_mask\\.tif", "\\1", file)

        # Load the raster
        raster <- rast(file)

        # Calculate summary statistics
        summary_stats <- global(raster, c("mean", "sum", "min", "max", "sd"), na.rm = TRUE)
        p25_p75 <- global(raster, fun = function(x) quantile(x, c(0.25, 0.75), na.rm = TRUE))

        # Add the results to the final summary dataframe
        #final_summary_df <- bind_rows(final_summary_df, data.frame(Study_Area = area, Raster = raster_name,
        #Mean = summary_stats[1, 1], Sum = summary_stats[1, 2],
        #Min = summary_stats[1, 3], Max = summary_stats[1, 4]))
        # Add the results to the final summary dataframe
        final_summary_df_fert <- bind_rows(final_summary_df_fert, data.frame(
            Study_Area = area,
            Raster = raster_name,
            Mean = summary_stats[1, 1],
            Sum = summary_stats[1, 2],
            Min = summary_stats[1, 3],
            Max = summary_stats[1, 4],
            SD = summary_stats[1,5],
            p25 = p25_p75[1,1],
            p75 = p25_p75[1,2]
        ))
    }
    cat("Processed Study Area:", area, "\n")
}



# Print the final summary data frame
print(final_summary_df_fert)


##################################################################################
# Add new columns to final_summary_df
final_summary_df2_fert <- final_summary_df_fert %>%
    mutate(
        Scenario = substr(Raster, 1, 2),  # Extract first two characters
        Raster = substring(Raster, 4)  # Remove the first three characters from 'Raster'
    )

final_summary_df2_fert <- final_summary_df2_fert %>%
    mutate(
        Managed = substr(Study_Area, 1,1),  # Extract first two characters
    )

final_summary_df2_fert <- final_summary_df2_fert %>%
    mutate(Fert = "Y")  # Extract first two characters

# Print the modified final_summary_df
print(final_summary_df2_fert)


write.csv(final_summary_df2_fert, file = "D:/BP_Layers/analysis/study_area_summary_fert.csv", row.names = FALSE)
