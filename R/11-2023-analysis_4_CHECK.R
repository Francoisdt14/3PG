# Define study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N") # , "U_13N"
#study_areas <- c("U_13N")

# Loop through study areas
for (area in study_areas) {

    # Set the output directory based on the current study area
    output_dir <- file.path("D:/BP_Layers", area, "analysis")
    # Check if the folder exists, and create it if it doesn't
    if (!file.exists(output_dir)) {
        dir.create(output_dir)
    }

    # Load in the stem and foliage values at 2080 for each scenario
    s1.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/present/ws200007.flt"))
    s1.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/present/wf200007.flt"))

    # calculate agb for each scenario in 2080
    s1 <- s1.ws + s1.wf

    # Load the masks
    fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/fao_forest_90m.tif"))
    #Sh_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_90m.tif"))
    #ShBy_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_Bry_90m.tif"))
    #Sh_fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_fao_90m.tif"))
    #ShBy_fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_Bry_fao_90m.tif"))

    # Get the EPSG code from fao.mask
    epsg_code <- crs(fao_mask, describe = T)$code
    # Set the CRS of s1 using the extracted EPSG code
    crs(s1) <- paste0("EPSG:", epsg_code)

    masked_rast <- terra::mask(s1, fao_mask)

    # Create the file path and name for the output raster
    output_path <- file.path(output_dir, paste(area, "fao_2020", sep = "_"))
    # Save the masked raster
    writeRaster(masked_rast, filename = paste(output_path, ".tif", sep = ""), overwrite = TRUE)

    cat("Study area", area, "completed\n")
}

###################################################################################
# RE-SAMPLE THE M_9S Areas HERE! - they need to be at exactly 90 meters

test.rast <- rast("D:/BP_Layers/M_9S/analysis/old/M_9S_fao_2020_old.tif")
# Assuming 'raster_data' is the name of your raster object
# Replace 'raster_data' with the actual name of your raster object

# Define the target resolution and extent
target_res <- 90  # Target resolution in x and y directions
target_ex <- ext(test.rast)  # Use the extent of the original raster
# Define the target CRS
target_crs <- "EPSG:32609"  # Replace with your desired CRS, e.g., "EPSG:32630" for UTM zone 30N
# Create a new raster with the target extent, resolution, and CRS
target_raster <- rast(target_ex, res = target_res, crs = target_crs)

# Resample the original raster to the target resolution
resampled_raster <- resample(test.rast, target_raster, method = "near", threads = T)

#writeRaster(resampled_raster, "D:/BP_Layers/M_9S/analysis/M_9S_fao_2020.tif")

#test.rast2 <- rast("D:/BP_Layers/M_9S/analysis/s1_Sh_mask.tif")

###################################################################################
# WE ARE HERE
# - remove the old tifs in analysis
# - need to re-run the fertilized areas

###################################################################################
# Specify study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")

# Initialize an empty dataframe to store the final summary statistics
final_summary_df_2020 <- data.frame(Study_Area = character(), Mean = numeric(), Sum = numeric(),
                               Min = numeric(), Max = numeric(),
                               SD = numeric(), p25 = numeric(), p75 = numeric(),
                               stringsAsFactors = FALSE)

# Loop through study areas
for (area in study_areas) {
    # Specify the path to the folder containing rasters
    folder_path <- file.path("D:/BP_Layers", area, "analysis")

    # List all the raster files in the folder
    raster_files <- list.files(path = folder_path, pattern = "fao_2020\\.tif$", full.names = TRUE)

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
        #final_summary_df_2020 <- bind_rows(final_summary_df_2020, data.frame(Study_Area = area, Raster = raster_name,
        #Mean = summary_stats[1, 1], Sum = summary_stats[1, 2],
        #Min = summary_stats[1, 3], Max = summary_stats[1, 4]))
        # Add the results to the final summary dataframe
        final_summary_df_2020 <- bind_rows(final_summary_df_2020, data.frame(
            Study_Area = area,
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
print(final_summary_df_2020)

fao_2020 <- final_summary_df_2020 %>%
    select(Study_Area, Mean)
##################################################################################

########################################################################################
# PLOTS


#write.csv(final_summary_df_2020, file = "D:/BP_Layers/analysis/study_area_summary_fao_2020.csv", row.names = FALSE)
fao_2050 <- final_summary_df3 %>%
    filter(!grepl("ShBy|Sh", Raster))

fao_2050_2 <- fao_2050 %>%
    select(Study_Area, Mean, Scenario, Managed, Fert)

# Merge data frames
fao_result_df <- merge(fao_2050_2, fao_2020, by = "Study_Area", all.x = TRUE)

# Calculate the difference
fao_result_df$difference <- fao_result_df$Mean.x - fao_result_df$Mean.y


# Assuming your result_df data frame is called fao_result_df
fao_result_df_sum <- fao_result_df %>%
    group_by(Study_Area, Fert) %>%
    summarise(
        Mean_Difference = mean(difference)
    ) %>% as.data.frame()

# Display the summary
print(fao_result_df_sum)


final_summary_df3_2 <- final_summary_df3 %>%
    filter(grepl("Sh", Raster) & !grepl("ShBy", Raster) & !grepl("_fao", Raster)) %>%
    select(Study_Area, Mean, Scenario, Managed, Fert)

final_summary_df3_2_sum <- final_summary_df3_2 %>%
    group_by(Study_Area, Fert) %>%
    summarise(
        average = mean(Mean)
    ) %>% as.data.frame(.)


# Merge the two dataframes based on 'Study_Area' and 'Fert'
fao_growth_merge_df <- merge(fao_result_df_sum, final_summary_df3_2_sum, by = c("Study_Area", "Fert"))



shp <- vect("Z:/_CanadaLayers/Vectors/txomin/Forested_ecozones.shp")
shp.proj <- terra::project(shp, "epsg:3347")

writeVector(shp.proj, "D:/PostDoc_Maps_Original/Projected_Layers/canada_provinces_noDissolve.shp")




library(tidyterra)

test.rast.2 <- rast("Y:\\Francois\\_dem\\soil_carbon_aligned_scaled_noNA.tif")

yr = 2000

ggplot() +
    geom_spatraster(data = test.rast.2) +
    scale_fill_viridis_c(limits = c(0.2, 1), option = "H") +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          panel.background = element_rect(color = "black", fill = NA),
          axis.text.x.top = element_text()) +
    labs(x = yr)






asia <- rast(system.file("extdata/asia.tif", package = "tidyterra"))

terra::plot(asia)

ggplot() +
  geom_spatraster(data = asia) +
  scale_fill_hypso_tint_c(
    palette = "gmt_globe",
    labels = scales::label_number(),
    # Further refinements
    breaks = c(-10000, -5000, 0, 2000, 5000, 8000),
    guide = guide_colorbar(reverse = TRUE)
  ) +
  labs(
    fill = "elevation (m)",
    title = "Hypsometric map of Asia"
  )




# Load libraries
library(ggplot2)

library(ggplot2)
library(dplyr)

# Create a dataframe with the provided data
df <- read.csv("G:/Sync/PostDoc/Figures/Paper_1_CO2e_stats.csv")

# Convert Fert to a factor with appropriate levels
df$Fert <- factor(df$Fert, levels = c("no", "yes"))

# Create the plot =- NOT WORK
ggplot(df, aes(x = Area, y = TgCO2e_ha, fill = Scenario, shape = Fert)) +
    geom_point(size = 4) +
    scale_fill_manual(values = c("s1" = "red", "s2" = "blue", "s3" = "green", "Drever" = "orange")) +
    scale_shape_manual(values = c("no" = 15, "yes" = 17)) +
    labs(x = "Study Area", y = "Tg CO2e / ha", title = "CO2 Emission by Scenario and Fertilization") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Define colors and shapes for scenarios and fertilization
scenario_colors <- c("s1" = "blue", "s2" = "green", "s3" = "red")
fert_shapes <- c("no" = "+", "yes" = "o")

# Plot - yuck



ggplot(df, aes(x = Area, y = TgCO2e_ha, color = Scenario, shape = Fert)) +
    geom_point(size = 4) +
    scale_color_manual(values = c("s1" = "red", "s2" = "blue", "s3" = "green", "Drever" = "black")) +
    scale_shape_manual(values = c("no" = 1, "yes" = 2)) +
    labs(x = "Study Area", y = "Tg CO2e / ha", title = "CO2 Emission by Scenario and Fertilization") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)

summary_df <- df %>%
    group_by(Area) %>%
    summarise(Max = max(TgCO2e_ha),
              Min = min(TgCO2e_ha),
              Mean = mean(TgCO2e_ha))

# Almost
ggplot(summary_df, aes(x = Area, ymin = Min, lower = Mean, middle = Mean, upper = Mean, ymax = Max)) +
    geom_errorbar(width = 0.1, color = "black", size = 0.8) +  # Represent the range with error bars
    geom_point(aes(y = Mean), color = "red", size = 3) +  # Overlay mean values with points
    labs(title = "Summary of TgCO2e_Canada by Study Area",
         x = "Study Area",
         y = "TgCO2e_ha") +
    theme_minimal()


# Define custom order for "Area" column
custom_order <- c("Drever", "all", unique(summary_df$Area[!summary_df$Area %in% c("Drever", "all")]))

# Convert "Area" column to factor with custom order
summary_df$Area <- factor(summary_df$Area, levels = custom_order)

# Plot with ordered "Area" column
ggplot(summary_df, aes(x = Area, ymin = Min, lower = Mean, middle = Mean, upper = Mean, ymax = Max)) +
    geom_errorbar(width = 0.1, color = "black", size = 0.8) +  # Represent the range with error bars
    geom_point(aes(y = Mean), color = "firebrick", size = 3) +  # Overlay mean values with points
    labs(title = "Summary of TgCO2e_Canada by Study Area",
         x = "Study Area",
         y = "TgCO2e_ha") +
    theme_bw() + theme(aspect.ratio = 1.5)

# Filter out the "Drever" row from the summary dataframe
summary_filtered <- summary_df %>% filter(Area != "Drever")
# Filter the "Drever" row from the dataframe for the dotted line
drever_df <- summary_df %>% filter(Area == "Drever")

ggplot(summary_filtered, aes(x = Area, ymin = Min, lower = Mean, middle = Mean, upper = Mean, ymax = Max)) +
    geom_errorbar(aes(color = "Range"), width = 0.2, size = 0.8) +  # Assign color for range
    geom_point(aes(y = Mean, color = "Mean"), size = 3) +  # Assign color for mean
    geom_hline(data = drever_df, aes(yintercept = Mean, linetype = "Drever"), color = "blue", size = 0.8) +  # Assign linetype for Drever
    labs(x = "Study Area",
         y = "TgCO2e_ha") +
    scale_color_manual(values = c("red", "black"), labels = c("Mean", "Range")) +  # Define colors and labels for legend
    scale_linetype_manual(values = "dotted", labels = "Drever") +  # Define linetype and label for Drever in legend
    theme_bw() + theme(aspect.ratio = 1.5) +
    guides(color = guide_legend(title = NULL), linetype = guide_legend(title = NULL))  # Customize legend titles



# best example
ggplot(summary_filtered, aes(x = Area, y = Mean)) +
    geom_point(aes(y = Max), color = "black", size = 2, shape = 1) +  # Represent max as black points
    geom_point(aes(y = Min), color = "black", size = 2, shape = 1) +  # Represent min as black points
    geom_segment(aes(xend = Area, yend = Max), linetype = "dotted", color = "black") +  # Connect max values with dotted line
    geom_segment(aes(xend = Area, yend = Min), linetype = "dotted", color = "black") +  # Connect min values with dotted line
    geom_point(aes(color = "Mean"), size = 3) +  # Represent mean as red points
    geom_hline(data = drever_df, aes(yintercept = Mean), linetype = "longdash", color = "dodgerblue3", size = 0.7) +
    labs(x = "Study Area",
         y = "TgCO2e_ha") +
    scale_color_manual(values = c("firebrick2"), labels = c("Mean")) +  # Define color and label for mean
    theme_bw() + theme(aspect.ratio = 1.5) +
    guides(color = guide_legend(title = NULL))  # Customize legend titles




