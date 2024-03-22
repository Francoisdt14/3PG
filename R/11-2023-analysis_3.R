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
    s1.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S1/Y3_Output/ws205007.flt"))
    s1.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S1/Y3_Output/wf205007.flt"))

    s2.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S2/Y3_Output/ws205007.flt"))
    s2.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S2/Y3_Output/wf205007.flt"))

    s3.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S3/Y3_Output/ws205007.flt"))
    s3.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S3/Y3_Output/wf205007.flt"))

    # calculate agb for each scenario in 2080
    s1 <- s1.ws + s1.wf
    s2 <- s2.ws + s2.wf
    s3 <- s3.ws + s3.wf

    # Load the masks
    fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/fao_forest_90m.tif"))
    Sh_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_90m.tif"))
    #ShBy_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_Bry_90m.tif"))
    #Sh_fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_fao_90m.tif"))
    #ShBy_fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_Bry_fao_90m.tif"))

    # Get the EPSG code from fao.mask
    epsg_code <- crs(fao_mask, describe = T)$code
    # Set the CRS of s1 using the extracted EPSG code
    crs(s1) <- paste0("EPSG:", epsg_code)
    crs(s2) <- paste0("EPSG:", epsg_code)
    crs(s3) <- paste0("EPSG:", epsg_code)

    # Create a list of scenarios and masks
    scenarios <- list(s1, s2, s3)
    #mask_names <- c("fao_mask", "Sh_mask", "ShBy_mask", "Sh_fao_mask", "ShBy_fao_mask")
    mask_names <- c("Sh_mask")
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

###################################################################################
# RE-SAMPLE THE M_9S Areas HERE! - they need to be at exactly 90 meters

test.rast <- rast("D:/BP_Layers/M_9S/analysis/s1_Sh_mask_old.tif")
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

#writeRaster(resampled_raster, "D:/BP_Layers/M_9S/analysis/s1_Sh_mask.tif")

#test.rast2 <- rast("D:/BP_Layers/M_9S/analysis/s1_Sh_mask.tif")

###################################################################################
# WE ARE HERE
 # - remove the old tifs in analysis
 # - need to re-run the fertilized areas

###################################################################################
# Specify study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")

# Initialize an empty dataframe to store the final summary statistics
final_summary_df <- data.frame(Study_Area = character(), Raster = character(), Mean = numeric(), Sum = numeric(),
                               Min = numeric(), Max = numeric(),
                               SD = numeric(), p25 = numeric(), p75 = numeric(),
                               stringsAsFactors = FALSE)

# Loop through study areas
for (area in study_areas) {
    # Specify the path to the folder containing rasters
    folder_path <- file.path("D:/BP_Layers", area, "analysis")

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
        final_summary_df <- bind_rows(final_summary_df, data.frame(
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
print(final_summary_df)


##################################################################################
# Add new columns to final_summary_df
final_summary_df2 <- final_summary_df %>%
    mutate(
        Scenario = substr(Raster, 1, 2),  # Extract first two characters
        Raster = substring(Raster, 4)  # Remove the first three characters from 'Raster'
    )

final_summary_df2 <- final_summary_df2 %>%
    mutate(
        Managed = substr(Study_Area, 1,1),  # Extract first two characters
        )

# Print the modified final_summary_df
print(final_summary_df2)

########################################################################################
# PLOTS

final_summary_df2 <- final_summary_df2 %>%
    mutate(Fert = "N")  # Extract first two characters

#write.csv(final_summary_df2, file = "D:/BP_Layers/analysis/study_area_summary_no_fert.csv", row.names = FALSE)

##################################################################################

# Read in datasets created above

final_summary_df2 <- read.csv("D:/BP_Layers/analysis/study_area_summary_no_fert.csv")
final_summary_df_fert2 <- read.csv("D:/BP_Layers/analysis/study_area_summary_fert.csv")

final_df_summary_df3 <- rbind(final_summary_df2, final_summary_df_fert2)

# Assuming 'final_summary_df' is your dataframe
final_summary_df4 <- final_df_summary_df3 %>%
    mutate(total_biomass = Sum* (90 * 90) / 10000)

#write.csv(final_summary_df4, file = "D:/BP_Layers/analysis/study_area_summary_all.csv", row.names = FALSE)

final_summary_df3 <- read.csv("D:/BP_Layers/analysis/study_area_summary_all.csv")

# Only keep the Shrub layer
Sh_df <- final_summary_df3 %>%
    filter(grepl("Sh", Raster) & !grepl("ShBy", Raster) & !grepl("_fao", Raster) & !grepl("N", Fert))

# Only keep the Shrub layer
Sh_df_all <- final_summary_df3 %>%
    filter(grepl("Sh", Raster) & !grepl("ShBy", Raster) & !grepl("_fao", Raster))

# FAO
# Filter the dataframe to include only fao rows
fao_df <- final_summary_df3 %>%
    filter(!grepl("ShBy|Sh", Raster) & !grepl("Y", Fert))


# Create a bar graph with facets for Sum values
ggplot(fao_df, aes(x = Study_Area, y = total_biomass, fill = Study_Area)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~Scenario, scales = "free") +
    labs(title = "Total Biomass in the FAO Forest Layer by Study Area and Scenario",
         x = "Study Area", y = "Total Biomass (Tons)") +
    theme_bw()


# Create a box plot faceted by Study Area and Scenario FAO
ggplot(fao_df, aes(x = Study_Area, y = Mean, ymin = p25, lower = p25, middle = Mean, upper = p75, ymax = p75, fill = Study_Area)) +
    geom_boxplot(stat = "identity", position = position_dodge(width = 0.9)) +
    facet_wrap(~ Scenario, scales = "free") +
    labs(title = "Mean Aboveground Biomass (ABG, tons/ha) by Study Area and Scenario - Forested Areas", x = "Study Area", y = "Mean ABG (tons/ha) ") +
    theme_bw()

#######################################################################################
# Create a box plot faceted by Study Area and Scenario JUST SHRUBS
ggplot(Sh_df, aes(x = Study_Area, y = Mean, ymin = p25, lower = p25, middle = Mean, upper = p75, ymax = p75, fill = Study_Area)) +
    geom_boxplot(stat = "identity", position = position_dodge(width = 0.9)) +
    facet_wrap(~ Scenario, scales = "free") +
    labs(title = "Mean Values by Study Area and Scenario", x = "Study Area", y = "Mean Value") +
    theme_bw()


############################################################################################
# Create a bar graph with facets for Sum values
ggplot(Sh_df, aes(x = Study_Area, y = total_biomass, fill = Scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass in the FAO Forest Layer by Study Area and Scenario",
         x = "Study Area", y = "Total Biomass (Tons)") +
    theme_bw()


##########

# Find the minimum total biomass value for each study area
min_values <- aggregate(total_biomass ~ Study_Area, Sh_df, min)
max_values <- aggregate(total_biomass ~ Study_Area, Sh_df, max)

# Subtract a specific amount (e.g., 500,000) from each minimum total biomass
#min_values$start_value <- min_values$total_biomass - 1000000
min_values$start_value <- min_values$total_biomass - 500000
max_values$end_value <- max_values$total_biomass + 500000

# Merge the minimum values back to the original dataframe
Sh_df_2 <- merge(Sh_df, min_values[c("Study_Area", "start_value")], by = "Study_Area")
Sh_df_3 <- merge(Sh_df_2, max_values[c("Study_Area", "end_value")], by = "Study_Area") %>%
    mutate(total_biomass = round(total_biomass, 0)) %>%
    mutate(start_value = round(start_value, 0)) %>%
    mutate(end_value = round(end_value, 0))
##########################################################################################################################
library(gridExtra)

area_plots <- list()

# Loop through unique Study_Area in the dataframe
for (selected_area in unique(Sh_df_3$Study_Area)) {
    # Subset the dataframe for the current Study_Area
    subset_df <- Sh_df_3[Sh_df_3$Study_Area == selected_area, ]

    # Plotting total_biomass against Scenario for the current Study_Area
    area_plot <- ggplot(subset_df, aes(x = Scenario, y = total_biomass)) +
        geom_bar(stat = "identity", aes(fill = Scenario), width = 0.7, color = "black") +
        geom_text(aes(label = total_biomass), vjust = -0.2) +
        scale_fill_manual(values = brewer.pal(8, "Set2")) +
        theme_classic() +
        theme(legend.position = "none",
              axis.title.y = element_text(hjust = 1)) +
        ggtitle(paste("Total Biomass vs Scenario for", selected_area)) +
        coord_cartesian(ylim = c(min(subset_df$start_value), max(subset_df$end_value)))

    # Add the plot to the list
    area_plots[[selected_area]] <- area_plot
}

# Arrange and print the plots
grid.arrange(grobs = area_plots, ncol = 2)  # Adjust ncol as needed


#########################################################

mean_plots <- list()

# Loop through unique Study_Area in the dataframe
for (selected_area in unique(Sh_df_3$Study_Area)) {
    # Subset the dataframe for the current Study_Area
    subset_df <- Sh_df_3[Sh_df_3$Study_Area == selected_area, ]

    # Plotting total_biomass against Scenario for the current Study_Area
    mean_plot <- ggplot(subset_df, aes(x = Scenario, y = Mean)) +
        geom_bar(stat = "identity", aes(fill = Scenario), width = 0.7, color = "black") +
        geom_text(aes(label = Mean), vjust = -0.2) +
        scale_fill_manual(values = brewer.pal(8, "Set2")) +
        theme_classic() +
        theme(legend.position = "none",
              axis.title.y = element_text(hjust = 1)) +
        ggtitle(paste("Total Biomass vs Scenario for", selected_area)) #+
        #coord_cartesian(ylim = c(min(subset_df$start_value), max(subset_df$end_value)))

    # Add the plot to the list
    mean_plots[[selected_area]] <- mean_plot
}

# Arrange and print the plots
grid.arrange(grobs = mean_plots, ncol = 2)  # Adjust ncol as needed


#############################################################################################################################

# Create a bar graph with facets for total biomass values
ggplot(Sh_df_2, aes(x = Scenario, y = total_biomass - start_value, fill = Scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~Study_Area) +
    labs(title = "Total Biomass in the FAO Forest Layer by Study Area and Scenario",
         x = "Scenario", y = "Total Biomass (Tons)",
         subtitle = "Start value reflected on Y-axis") +
    theme_bw()

#############################################



#################### Data over time - THIS IS TO PRESENT TIME #########################################
# Data over time - THIS IS TO PRESENT TIME - redo this biomass from 2020 - 2050 as a bar?


# Create an empty data frame to store the results
test_df <- data.frame(area = character(), variable = character(), year = character(), mean = numeric(), sum = numeric(), min = numeric(), max = numeric(), sd = numeric(), stringsAsFactors = FALSE)

# study area for folders
area <- "U_13N"

# Set the folder path where raster files are located
#folder_path <- "D:/BP_Layers/M_9S/biomass_3PG/S1/Y2_Output"
#folder_path <- "I:/data_2024_01_29/M_9S/output_M_9S_lp_new"

#folder_path <- paste("I:/data_2024_01_29/", area, "/output_", area,"_bs", sep = "")
folder_path <- paste("D:/BP_Layers/", area, "/biomass_3PG/present", sep = "")

# Load the mask
fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/fao_forest_90m.tif"))

# Define the pattern to match filenames
pattern <- "ws\\d{6}\\.flt"  # Adjust the pattern based on your actual filenames

# List raster files matching the pattern in the folder
raster_files <- list.files(folder_path, pattern = pattern, full.names = TRUE)

# Loop through each raster file
for (file in raster_files) {
    # Extract relevant information from the filename
    file_info <- unlist(strsplit(basename(file), "\\."))

    # Read in the raster using terra
    raster_data <- rast(file)

    # Get the EPSG code from fao.mask
    epsg_code <- crs(fao_mask, describe = T)$code
    # Set the CRS of s1 using the extracted EPSG code
    crs(raster_data) <- paste0("EPSG:", epsg_code)

    # Apply the mask
    masked_raster <- mask(raster_data, fao_mask)

    # Calculate global statistics
    summary_stats <- global(masked_raster, c("mean", "sum", "min", "max", "sd"), na.rm = TRUE)

    # Extract information from the filename
    variable <- substr(file_info[1], 1, 2)  # Extract first two characters (e.g., 'ws')
    year <- substr(file_info[1], 3, 6)       # Extract year (e.g., '2010')

    # Create a new row in the data frame
    new_row <- data.frame(area = area, variable = variable, year = year, mean = summary_stats[1], sum = summary_stats[2], min = summary_stats[3], max = summary_stats[4], sd = summary_stats[5])

    # Add the new row to the result data frame
    test_df <- rbind(test_df, new_row)
}

rownames(test_df) <- NULL
# Print the resulting data frame
print(test_df)

#write.csv(test_df, file = "D:/BP_Layers/analysis/study_areas_over_time_1870-2020.csv", row.names = FALSE)

test_df_2 <- test_df %>%
    mutate(
        managed = substr(area, 1,1),  # Extract first two characters
    )


test_df_2 <- test_df %>%
    mutate(total_biomass = sum * (90 * 90) / 10000)

# plots

# Assuming your dataframe is named test_df
# If it's named differently, replace test_df with your actual dataframe name

# Plotting mean vs year, colored by area
comb_mean_plot <- ggplot(test_df_2, aes(x = year, y = mean, color = area)) +
    geom_point() +
    labs(x = "Year",
         y = "Mean Biomass (tons/ha)") +
    theme_bw()

# Print the plot
print(comb_mean_plot)

# Assuming your dataframe is named test_df
# If it's named differently, replace test_df with your actual dataframe name

# Create a list to store plots
mean_plots <- list()

# Loop through unique areas in the dataframe
for (selected_area in unique(test_df$area)) {
    # Subset the dataframe for the current area
    subset_df <- test_df_2[test_df_2$area == selected_area, ]

    # Plotting the year against the mean for the current area
    mean_plot <- ggplot(subset_df, aes(x = year, y = mean)) +
        geom_point() +
        labs(title = paste("Year vs Mean for", selected_area),
             x = "Year",
             y = "Mean")

    # Add the plot to the list
    mean_plots[[selected_area]] <- mean_plot
}

# Arrange and print the plots
grid.arrange(grobs = mean_plots, ncol = 2)  # Adjust ncol as needed

# Create a list to store plots
sum_plots <- list()

# Loop through unique areas in the dataframe
for (selected_area in unique(test_df$area)) {
    # Subset the dataframe for the current area
    subset_df <- test_df_2[test_df_2$area == selected_area, ]

    # Plotting the year against the mean for the current area
    sum_plot <- ggplot(subset_df, aes(x = year, y = total_biomass)) +
        geom_point() +
        labs(title = paste("Year Sum for", selected_area),
             x = "Year",
             y = "Mean")

    # Add the plot to the list
    sum_plots[[selected_area]] <- sum_plot
}

# Arrange and print the plots
grid.arrange(grobs = sum_plots, ncol = 2)  # Adjust ncol as needed

################# Data over time - 2020-2050 ##########################################################

# looking at 2020 to 2050 under different scenarios...
# Create an empty data frame to store the results
test_df <- data.frame(area = character(), variable = character(), year = character(), mean = numeric(), sum = numeric(), min = numeric(), max = numeric(), sd = numeric(), stringsAsFactors = FALSE)
# study areas
areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")

# Loop through each study area
for (area in areas) {
    # Set the folder paths where raster files are located
    folder_path_1 <- file.path("D:/BP_Layers", area, "biomass_3PG", "S1", "Y2_Output")
    folder_path_2 <- file.path("D:/BP_Layers", area, "biomass_3PG", "S1", "Y3_Output")

    # Define the patterns to match filenames
    pattern_ws <- "ws\\d{6}\\.flt"  # Pattern for 'ws' rasters
    pattern_wf <- "wf\\d{6}\\.flt"  # Pattern for 'wf' rasters

    # List raster files matching the patterns in the folders
    raster_files_ws_1 <- list.files(folder_path_1, pattern = pattern_ws, full.names = TRUE)
    raster_files_wf_1 <- list.files(folder_path_1, pattern = pattern_wf, full.names = TRUE)
    raster_files_ws_2 <- list.files(folder_path_2, pattern = pattern_ws, full.names = TRUE)
    raster_files_wf_2 <- list.files(folder_path_2, pattern = pattern_wf, full.names = TRUE)

    # Combine the raster files
    raster_files_combined_ws <- c(raster_files_ws_1, raster_files_ws_2)
    raster_files_combined_wf <- c(raster_files_wf_1, raster_files_wf_2)

    # Read in and combine ws rasters
    ws_rasters <- lapply(raster_files_combined_ws, rast)

    # Read in and combine wf rasters
    wf_rasters <- lapply(raster_files_combined_wf, rast)

    # Load the mask
    sh_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_90m.tif"))

    # Get the EPSG code from fao.mask
    epsg_code <- crs(sh_mask, describe = T)$code

    # Set the CRS of combined ws and wf rasters using the extracted EPSG code
    ws_rasters <- lapply(ws_rasters, function(x) {crs(x) <- paste0("EPSG:", epsg_code); return(x)})
    wf_rasters <- lapply(wf_rasters, function(x) {crs(x) <- paste0("EPSG:", epsg_code); return(x)})

    # Combine ws and wf rasters based on their date (e.g., '202007')
    #combined_rasters <- ws_rasters + wf_rasters

    # Combine ws and wf rasters based on their date (e.g., '202007')
    combined_rasters <- Map(function(x, y) x + y, ws_rasters, wf_rasters)


    masked_rasters <- lapply(combined_rasters, function(r) terra::mask(r, sh_mask))

    # Calculate global statistics for each masked raster
    stats_list <- lapply(masked_rasters, function(r) global(r, c("mean", "sum", "min", "max", "sd"), na.rm = TRUE))

    # Extract information from the filename and create a new row for each study area
    for (i in seq_along(masked_rasters)) {
        year <- substr(basename(raster_files_combined_ws[i]), 3, 6)
        new_row <- data.frame(area = area, variable = "combined", year = year,
                              mean = stats_list[[i]][1], sum = stats_list[[i]][2],
                              min = stats_list[[i]][3], max = stats_list[[i]][4],
                              sd = stats_list[[i]][5])
        test_df <- rbind(test_df, new_row)
    }

    # Print the completed study area
    cat("Study area", area, "completed\n")
}

rownames(test_df) <- NULL

# Print the resulting data frame
print(test_df)

# Write the resulting data frame to a CSV file
#write.csv(test_df, file = "D:/BP_Layers/analysis/study_areas_over_time_1870-2020.csv", row.names = FALSE)

# Create a new column 'managed' by extracting first two characters from 'area'
test_df$managed <- substr(test_df$area, 1,1)

# Create a new column 'total_biomass' by calculating total biomass per hectare
test_df$total_biomass <- test_df$sum * (90 * 90) / 10000

# Plotting mean vs year, colored by area
comb_mean_plot <- ggplot(test_df, aes(x = year, y = mean, color = area)) +
    geom_point() +
    labs(x = "Year",
         y = "Mean Biomass (tons/ha)") +
    theme_bw()

# Print the plot
print(comb_mean_plot)

##########################################################################

# Create an empty data frame to store the results
test_df <- data.frame(area = character(), scenario = character(), variable = character(), year = character(), mean = numeric(), sum = numeric(), min = numeric(), max = numeric(), sd = numeric(), stringsAsFactors = FALSE)

# study areas
areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S") #, "U_13N")
scenarios <- c("S1", "S2", "S3")

# Loop through each study area and scenario
for (area in areas) {
    for (scenario in scenarios) {

        # Set the folder paths where raster files are located
        folder_path_1 <- file.path("D:/BP_Layers", area, "biomass_3PG", scenario, "Y2_Output")
        folder_path_2 <- file.path("D:/BP_Layers", area, "biomass_3PG", scenario, "Y3_Output")

        # Define the patterns to match filenames
        pattern_ws <- "ws\\d{6}\\.flt"  # Pattern for 'ws' rasters
        pattern_wf <- "wf\\d{6}\\.flt"  # Pattern for 'wf' rasters

        # List raster files matching the patterns in the folders
        raster_files_ws_1 <- list.files(folder_path_1, pattern = pattern_ws, full.names = TRUE)
        raster_files_wf_1 <- list.files(folder_path_1, pattern = pattern_wf, full.names = TRUE)
        raster_files_ws_2 <- list.files(folder_path_2, pattern = pattern_ws, full.names = TRUE)
        raster_files_wf_2 <- list.files(folder_path_2, pattern = pattern_wf, full.names = TRUE)

        # Combine the raster files
        raster_files_combined_ws <- c(raster_files_ws_1, raster_files_ws_2)
        raster_files_combined_wf <- c(raster_files_wf_1, raster_files_wf_2)

        # Read in and combine ws rasters
        ws_rasters <- lapply(raster_files_combined_ws, rast)

        # Read in and combine wf rasters
        wf_rasters <- lapply(raster_files_combined_wf, rast)

        # Load the mask
        sh_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_90m.tif"))

        # Get the EPSG code from fao.mask
        epsg_code <- crs(sh_mask, describe = T)$code

        # Set the CRS of combined ws and wf rasters using the extracted EPSG code
        ws_rasters <- lapply(ws_rasters, function(x) {crs(x) <- paste0("EPSG:", epsg_code); return(x)})
        wf_rasters <- lapply(wf_rasters, function(x) {crs(x) <- paste0("EPSG:", epsg_code); return(x)})

        # Combine ws and wf rasters based on their date (e.g., '202007')
        #combined_rasters <- ws_rasters + wf_rasters

        # Combine ws and wf rasters based on their date (e.g., '202007')
        combined_rasters <- Map(function(x, y) x + y, ws_rasters, wf_rasters)


        masked_rasters <- lapply(combined_rasters, function(r) terra::mask(r, sh_mask))

        # Calculate global statistics for each masked raster
        stats_list <- lapply(masked_rasters, function(r) global(r, c("mean", "sum", "min", "max", "sd"), na.rm = TRUE))


        # Extract information from the filename and create a new row for each study area and scenario
        for (i in seq_along(masked_rasters)) {
            year <- substr(basename(raster_files_combined_ws[i]), 3, 6)
            new_row <- data.frame(area = area, scenario = scenario, variable = "combined", year = year,
                                  mean = stats_list[[i]][1], sum = stats_list[[i]][2],
                                  min = stats_list[[i]][3], max = stats_list[[i]][4],
                                  sd = stats_list[[i]][5])
            test_df <- rbind(test_df, new_row)
        }

        # Print the completed study area and scenario
        cat("Study area", area, "under scenario", scenario, "completed\n")
    }
}

# Print the resulting data frame
print(test_df)

# Write the resulting data frame to a CSV file
#write.csv(test_df, file = "D:/BP_Layers/analysis/study_areas_over_time_1870-2020.csv", row.names = FALSE)

# Create a new column 'managed' by extracting first two characters from 'area'
test_df$managed <- substr(test_df$area, 1,1)

# Create a new column 'total_biomass' by calculating total biomass per hectare
test_df$total_biomass <- test_df$sum * (90 * 90) / 10000

# Plot the mean biomass of each study area and scenario, faceted by study area
library(ggplot2)
library(scales)

mean_biomass_plot <- ggplot(test_df, aes(x = year, y = mean, fill = scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~area) +
    labs(x = "Year", y = "Mean Biomass (tons/ha)") +
    theme_bw()

# Print the plot
print(mean_biomass_plot)

# Create a bar plot with different y-axes for each facet
total_biomass_plot <- ggplot(test_df, aes(x = year, y = total_biomass, fill = scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~area, scales = "free_y") +  # Add scales = "free_y" to allow different y-axes for each facet
    labs(x = "Year", y = "Total Biomass (tDM)") +
    theme_bw()

# Print the plot
print(total_biomass_plot)


# Convert 'year' to factor for better visualization
test_df_fut <- test_df
test_df_fut$year <- factor(test_df_fut$year)
# Convert 'scenario' to factor to control the order
test_df_fut$scenario <- factor(test_df_fut$scenario, levels = c("S1", "S2", "S3"))
test_df_fut$area <- factor(test_df_fut$area)

# Define custom colors for the study areas
custom_colors <- c("M_9S" = "aquamarine", "M_11S" = "cadetblue", "M_18S" = "hotpink2", "U_18S" = "lightsalmon", "U_15S" = "brown")

#"brown"
# Update the plot to customize the colors for the study areas and differentiate between managed and unmanaged plots
mean_biomass_point_plot <- ggplot(test_df_fut, aes(x = year, y = mean, color = area, shape = scenario, fill = managed)) +
    geom_jitter(position = position_dodge(width = 0.5), size = 3) +
    labs(x = "Year", y = "Mean Biomass (tons/ha)") +
    scale_shape(solid = TRUE) +
    scale_color_manual(values = custom_colors) +  # Customize colors for the study areas
    scale_fill_manual(values = c("managed" = "lightgray", "unmanaged" = "white")) +  # Differentiate between managed and unmanaged plots
    theme_bw() +
    theme(aspect.ratio = 1)


# Create the ggplot
mean_biomass_point_plot <- ggplot(test_df_fut, aes(x = year, y = mean, color = area, shape = scenario, fill = scenario)) +
    geom_jitter(position = position_dodge(width = 0.5), size = 3) +
    labs(x = "Year", y = "Mean Biomass (tons/ha)") +
    scale_shape(solid = TRUE) +
    theme_bw()


# Print the plot
print(mean_biomass_point_plot)

#########################################################################
######################################################################
############################## LOOK AT Each individual raster ########

# masked fao example in 2050

w1 <- rast("D:/BP_Layers/M_9S/analysis/s3_fao_mask.tif")
w2 <- rast("D:/BP_Layers/M_11S/analysis/s3_fao_mask.tif")
w3 <- rast("D:/BP_Layers/M_18S/analysis/s3_fao_mask.tif")
w4 <- rast("D:/BP_Layers/U_18S/analysis/s3_fao_mask.tif")
w5 <- rast("D:/BP_Layers/U_15S/analysis/s3_fao_mask.tif")
w6 <- rast("D:/BP_Layers/U_13N/analysis/s3_fao_mask.tif")


# Convert raster to data frame
df1 <- as.data.frame(w1, xy = TRUE)
colnames(df1)[3] <- "M_9S"
df2 <- as.data.frame(w2, xy = TRUE)
colnames(df2)[3] <- "M_11S"

df3 <- as.data.frame(w3, xy = TRUE)
colnames(df3)[3] <- "M_18S"

df4 <- as.data.frame(w4, xy = TRUE)
colnames(df4)[3] <- "U_18S"

df5 <- as.data.frame(w5, xy = TRUE)
colnames(df5)[3] <- "U_15S"

df6 <- as.data.frame(w6, xy = TRUE)
colnames(df6)[3] <- "U_13N"

# Set the seed for reproducibility
set.seed(123)
# Randomly sample 50,000 rows from df1
df1_s <- df1 %>% sample_n(50000, replace = FALSE)
df2_s <- df2 %>% sample_n(50000, replace = FALSE)
df3_s <- df3 %>% sample_n(50000, replace = FALSE)
df4_s <- df4 %>% sample_n(50000, replace = FALSE)
df5_s <- df5 %>% sample_n(50000, replace = FALSE)
df6_s <- df6 %>% sample_n(50000, replace = FALSE)

df_all_fao <- cbind(df1_s$M_9S, df2_s$M_11S, df3_s$M_18S, df4_s$U_18S, df5_s$U_15S, df6_s$U_13N) %>% as.data.frame()
#, df4$U_18S) %>% as.data.frame()

colnames(df_all_fao)[1] <- "M_9S"
colnames(df_all_fao)[2] <- "M_11S"
colnames(df_all_fao)[3] <- "M_18S"
colnames(df_all_fao)[4] <- "U_18S"
colnames(df_all_fao)[5] <- "U_15S"
colnames(df_all_fao)[6] <- "U_13N"


# Summarize the raster
summary1 <- df_all %>%
    summarize(
        mean = mean(M_9S),
        min = min(M_9S),
        max = max(M_9S),
        sd = sd(M_9S),
        median = median(M_9S),
        quartile_25 = quantile(M_9S, 0.25),
        quartile_75 = quantile(M_9S, 0.75)
    )




# Create a violin plot
ggplot(df_all_fao, aes(x = "", y = M_9S)) +
    geom_violin(fill = "lightgreen", color = "black", width = 0.5) +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    stat_summary(fun=mean, geom="point", shape=18, size=3, color="darkgreen") +
    labs(title = "Violin Plot of M_9S", x = "", y = "M_9S") +
    theme_minimal()


# Reshape the data into long format
df_all_fao_long <- gather(df_all_fao, key = "variable", value = "value")

# Create a violin plot for each variable with box plots and mean overlaid
ggplot(df_all_fao_long, aes(x = variable, y = value)) +
    geom_violin(fill = "lightgreen", color = "black") +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkgreen") +
    labs(title = "Violin Plots of M_9S, M_11S, and M_18S", x = "Variable", y = "Value") +
    theme_minimal() +
    facet_wrap(~variable, scales = "free")

###############

# Create a violin plot for each variable with box plots and mean overlaid
ggplot(df_all_fao_long, aes(x = factor(1), y = value)) +
    geom_violin(aes(fill = variable), color = "black") +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkgreen") +
    labs(title = "Violin Plots of M_9S, M_11S, and M_18S", x = "", y = "Value") +
    scale_fill_manual(values = c("aquamarine", "cadetblue", "hotpink2", "lightsalmon", "tan2", "brown")) +  # Set your own color scheme
    theme_minimal() +
    facet_grid(. ~ variable, scales = "fixed", space = "free")+
    theme(axis.text.x = element_blank())  # Remove x-axis text


##################################


# masked shrub example in 2050

w1 <- rast("D:/BP_Layers/M_9S/analysis/s3_Sh_mask.tif")
w2 <- rast("D:/BP_Layers/M_11S/analysis/s3_Sh_mask.tif")
w3 <- rast("D:/BP_Layers/M_18S/analysis/s3_Sh_mask.tif")
w4 <- rast("D:/BP_Layers/U_18S/analysis/s3_Sh_mask.tif")
w5 <- rast("D:/BP_Layers/U_15S/analysis/s3_Sh_mask.tif")
w6 <- rast("D:/BP_Layers/U_13N/analysis/s3_Sh_mask.tif")


# Convert raster to data frame
df1 <- as.data.frame(w1, xy = TRUE)
colnames(df1)[3] <- "M_9S"
df2 <- as.data.frame(w2, xy = TRUE)
colnames(df2)[3] <- "M_11S"

df3 <- as.data.frame(w3, xy = TRUE)
colnames(df3)[3] <- "M_18S"

df4 <- as.data.frame(w4, xy = TRUE)
colnames(df4)[3] <- "U_18S"

df5 <- as.data.frame(w5, xy = TRUE)
colnames(df5)[3] <- "U_15S"

df6 <- as.data.frame(w6, xy = TRUE)
colnames(df6)[3] <- "U_13N"

# List of your dataframes
df_list <- list(df1, df2, df3, df4, df5, df6)

# Find the minimum number of rows
min_rows <- min(sapply(df_list, nrow))

# Randomly sample each dataframe to have the same number of rows as the smallest dataframe
# sampled_dfs <- lapply(df_list, function(df) {
#     df %>% slice_sample(n = min_rows, replace = FALSE)
# })

# Randomly sample 50,000 rows from df1
df1_s <- df1 %>% sample_n(min_rows, replace = FALSE)
df2_s <- df2 %>% sample_n(min_rows, replace = FALSE)
df3_s <- df3 %>% sample_n(min_rows, replace = FALSE)
df4_s <- df4 %>% sample_n(min_rows, replace = FALSE)
df5_s <- df5 %>% sample_n(min_rows, replace = FALSE)
df6_s <- df6 %>% sample_n(min_rows, replace = FALSE)

df_all_ShBy <- cbind(df1_s$M_9S, df2_s$M_11S, df3_s$M_18S, df4_s$U_18S, df5_s$U_15S, df6_s$U_13N) %>% as.data.frame()
#, df4$U_18S) %>% as.data.frame()

colnames(df_all_ShBy)[1] <- "M_9S"
colnames(df_all_ShBy)[2] <- "M_11S"
colnames(df_all_ShBy)[3] <- "M_18S"
colnames(df_all_ShBy)[4] <- "U_18S"
colnames(df_all_ShBy)[5] <- "U_15S"
colnames(df_all_ShBy)[6] <- "U_13N"

# Reshape the data into long format
df_all_ShBy_long <- gather(df_all_ShBy, key = "variable", value = "value")


# Create a violin plot for each variable with box plots and mean overlaid
ggplot(df_all_ShBy_long, aes(x = factor(1), y = value)) +
    geom_violin(aes(fill = variable), color = "black") +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkgreen") +
    labs(title = "Distribution of biomass values under climate scenario 3, shrubs, herbs, and bryoids mask", x = "", y = "Biomass (tons/ha)") +
    scale_fill_manual(values = c("aquamarine", "cadetblue", "hotpink2", "lightsalmon", "tan2", "brown")) +  # Set your own color scheme
    theme_minimal() +
    facet_grid(. ~ variable, scales = "fixed", space = "free")+
    theme(axis.text.x = element_blank()) +  # Remove x-axis text
    coord_cartesian(ylim = c(20, 150))  + # Set y-axis limits
    labs(fill = "Study Area")




##########################################################################################################
# Create a function to read and process the rasters
process_raster <- function(scenario, study_area, min_rows) {
    # Read the raster
    raster_path <- paste0("D:/BP_Layers/", study_area, "/analysis/", scenario, "_Sh_mask.tif")
    raster_data <- rast(raster_path)

    # Extract the values from the raster
    values <- as.data.frame(raster_data)
    colnames(values) <- study_area

    # Sample the dataframe to have the same number of rows as the smallest dataframe
    values_sampled <- values %>% sample_n(min_rows, replace = FALSE)

    return(values_sampled)
}

# List of scenarios and study areas
scenarios <- c("s1", "s2", "s3")
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")

# Find the minimum number of rows
#min_rows <- min(sapply(all_dfs, function(df_list) min(sapply(df_list, nrow))))

# Process all rasters for each scenario and study area
all_dfs <- lapply(scenarios, function(scenario) {
    lapply(study_areas, function(area) {
        process_raster(scenario, area, min_rows)
    })
})

# Combine the data frames
combined_dfs <- lapply(seq_along(scenarios), function(i) {
    scenario <- scenarios[i]
    dfs <- all_dfs[[i]]
    df_combined <- do.call(cbind, dfs)
    colnames(df_combined) <- study_areas
    df_combined$scenario <- scenario
    return(df_combined)
})
# Combine the data frames for all scenarios
final_df <- do.call(rbind, combined_dfs)


# Reshape the data into long format
final_df_long <- gather(final_df, key = "study_area", value = "value", -scenario)

# Create a violin plot with an overlaid box plot for each study area, faceted by the scenario
violin_plot <- ggplot(final_df_long, aes(x = study_area, y = value)) +
    geom_violin(aes(fill = study_area), color = "black") +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkgreen") +
    labs(title = "Distribution of biomass values under different scenarios", x = "Study Area", y = "Biomass (tons/ha)") +
    scale_fill_manual(values = c("aquamarine", "cadetblue", "hotpink2", "lightsalmon", "tan2", "brown")) +
    theme_minimal() +
    facet_grid(. ~ scenario, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(20, 150))

# Plot the violin plots side by side using cowplot
plot_grid(violin_plot + theme(legend.position="none"),
          align = 'h', labels=c("A", "B"), hjust=0)


# Create a violin plot with an overlaid box plot for each study area, faceted by the study area
violin_plot <- ggplot(final_df_long, aes(x = scenario, y = value, fill = scenario)) +
    geom_violin(position = position_dodge(width = 0.9), color = "black") +
    geom_boxplot(position = position_dodge(width = 0.9), width = 0.2, fill = "white", color = "black",outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.9), shape = 18, size = 3, color = "darkgreen") +
    labs(title = "Distribution of biomass values under different scenarios", x = "Scenario", y = "Biomass (tons/ha)") +
    scale_fill_manual(values = c("aquamarine", "cadetblue", "hotpink2")) +
    theme_minimal() +
    facet_wrap(~ study_area, ncol=3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(20, 120)) +
    theme(aspect.ratio = 3/2)

# Plot the violin plots side by side using cowplot
print(violin_plot)

##############################################################################################################

# masked fao example in 2050

w1 <- rast("D:/BP_Layers/M_11S/analysis/s1_ShBy_mask.tif")
w2 <- rast("D:/BP_Layers/M_11S/analysis/s2_ShBy_mask.tif")
w3 <- rast("D:/BP_Layers/M_11S/analysis/s3_ShBy_mask.tif")


# Convert raster to data frame
df1 <- as.data.frame(w1, xy = TRUE)
colnames(df1)[3] <- "S1"
df2 <- as.data.frame(w2, xy = TRUE)
colnames(df2)[3] <- "S2"
df3 <- as.data.frame(w3, xy = TRUE)
colnames(df3)[3] <- "S3"

# List of your dataframes
df_list <- list(df1, df2, df3)

# Find the minimum number of rows
min_rows <- min(sapply(df_list, nrow))

# Randomly sample each dataframe to have the same number of rows as the smallest dataframe
# sampled_dfs <- lapply(df_list, function(df) {
#     df %>% slice_sample(n = min_rows, replace = FALSE)
# })

# Randomly sample 50,000 rows from df1
df1_s <- df1 %>% sample_n(50000, replace = FALSE)
df2_s <- df2 %>% sample_n(50000, replace = FALSE)
df3_s <- df3 %>% sample_n(50000, replace = FALSE)


df_s_ShBy <- cbind(df1_s$S1, df2_s$S2, df3_s$S3) %>% as.data.frame()
#, df4$U_18S) %>% as.data.frame()

colnames(df_s_ShBy)[1] <- "S1"
colnames(df_s_ShBy)[2] <- "S2"
colnames(df_s_ShBy)[3] <- "S3"


# Reshape the data into long format
df_s_ShBy_long <- gather(df_s_ShBy, key = "variable", value = "value")

# Create a violin plot for each variable with box plots and mean overlaid
ggplot(df_s_ShBy_long, aes(x = variable, y = value)) +
    geom_violin(aes(fill = variable), color = "black") +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkgreen") +
    labs(title = "Violin Plots of S1, S2, S3", x = "Scenario", y = "Value") +
    scale_fill_manual(values = c("aquamarine", "cadetblue", "hotpink2")) +  # Set your own color scheme
    theme_minimal() +
    facet_grid(. ~ variable, scales = "fixed", space = "free") +
    theme(axis.text.x = element_blank()) +  # Remove x-axis text
    coord_cartesian(ylim = c(20, 100))  # Set y-axis limits






# Filter the dataframe to include only 'Sh' and 'ShBy' rows
ShBy_df_2 <- final_summary_df3 %>%
    filter(grepl("Sh", Raster) & !grepl("_fao", Raster) & !grepl("ShBy", Raster))

# Plot biomass by climate scenario and fertilization status, facet by study area
ggplot(ShBy_df_2, aes(x = Scenario, y = total_biomass, fill = Fert)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass Comparison by Climate Scenario and Fertilization",
         x = "Climate Scenario", y = "Total Biomass (Tons)") +
    scale_fill_manual(values = c("N" = "cadetblue", "Y" = "lightgreen")) +  # Set your own colors
    theme_bw()


# Only keep the Shrub layers
Sh_ShBy_df_2 <- final_summary_df3 %>%
    filter(grepl("Sh", Raster) & !grepl("_fao", Raster) & !grepl("ShBy", Raster))

names(Sh_ShBy_df_2)[names(Sh_ShBy_df_2) == "Raster"] <- "Mask"

# Plot biomass by climate scenario, fertilization status, and mask, facet by study area
ggplot(Sh_ShBy_df_2, aes(x = Scenario, y = total_biomass, fill = Fert)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass Comparison by Climate Scenario, Fertilization, and Mask",
         x = "Climate Scenario", y = "Total Biomass (Tons)") +
    scale_fill_manual(values = c("N" = "cadetblue", "Y" = "lightgreen")) +  # Set your own colors for Fert
    theme_bw()



# Sum total biomass for each combination of Scenario, Fert, and Study_Area
summarized_df <- Sh_ShBy_df_2 %>%
    group_by(Scenario, Fert, Study_Area) %>%
    summarize(total_biomass = sum(total_biomass))

# Sum total biomass for each combination of Scenario and Fert
final_summarized_df <- summarized_df %>%
    group_by(Scenario, Fert) %>%
    summarize(total_biomass = sum(total_biomass))

# Plot grouped bar chart by climate scenario and facet by fertilization status
ggplot(final_summarized_df, aes(x = Scenario, y = total_biomass, fill = Fert)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "Grouped Biomass Comparison by Climate Scenario and Fertilization",
         x = "Scenario", y = "Total Biomass") +
    scale_fill_manual(values = c("N" = "blue", "Y" = "red")) +  # Set your own colors
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



############################

# Calculate the difference between 'Fert' = Y and 'Fert' = N for each study area and scenario
df_diff <- Sh_df_all %>%
    group_by(Study_Area, Scenario) %>%
    summarise(Difference = mean(Mean[Fert == 'Y']) - mean(Mean[Fert == 'N']))

# Plot the differences by study area
ggplot(df_diff, aes(x = Study_Area, y = Difference, fill = Scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Mean Difference between Fert Y and Fert N by Study Area",
         x = "Study Area", y = "Mean Difference") +
    theme_bw() +
    theme(aspect.ratio= 1)
