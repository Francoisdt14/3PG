library(terra)
library(tidyverse)
library(cowplot)

# First we want to sum up our rasters by year:

study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")
#study_areas <- c("M_11S")

scenarios <- c("s1", "s2", "s3")

for (area in study_areas) {
    for (scenario in scenarios) {
        # Define paths
        folder_path <- paste0("D:/BP_Layers/NPP/", area, "/", scenario)
        output_path <- paste0("D:/BP_Layers/NPP/", area, "/", scenario, "/", "npp_2050_", scenario, "_", area, ".tif")

        # List .flt files for the year 2050
        flt_files <- list.files(folder_path, pattern = "npp2050[0-9]{2}\\.flt$", full.names = TRUE)

        # Check if there are .flt files for the year 2050
        if (length(flt_files) > 0) {
            # Read and stack the rasters
            raster_stack <- rast(flt_files)

            # Sum the layers
            summed_raster <- sum(raster_stack)

            # Write the final summed raster
            writeRaster(summed_raster, filename = output_path, overwrite = TRUE)

            cat("Summed raster for", area, scenario, "and saved to", output_path, "\n")
        } else {
            cat("Error: No .flt files for the year 2050 in", area, scenario, "\n")
        }
    }
    cat("Finished processing study area", area, "\n")
}

############################################################################################################
# Apply as mask to each study area and scenario - shrub mask

# Define study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")
#study_areas <- c("U_13N")

# Loop through study areas
for (area in study_areas) {

    # Set the output directory based on the current study area
    output_dir <- file.path("D:/BP_Layers", area, "analysis/NPP")
    # Check if the folder exists, and create it if it doesn't
    if (!file.exists(output_dir)) {
        dir.create(output_dir)
    }

    # Loop through scenarios
    scenarios <- c("s1", "s2", "s3")
    for (scenario in scenarios) {

        # Load in the raster for the current scenario
        raster <- rast(file.path("D:/BP_Layers/NPP", area, scenario, paste0("npp_2050_", scenario, "_", area, ".tif")))

        # Load the mask
        mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_90m.tif"))

        # Get the EPSG code from the mask
        epsg_code <- crs(mask, describe = TRUE)$code
        # Set the CRS of the raster using the extracted EPSG code
        crs(raster) <- paste0("EPSG:", epsg_code)

        # Apply the mask
        masked_raster <- mask(raster, mask)

        # Create the file path and name for the output raster
        output_path <- file.path(output_dir, paste("npp_sh_", scenario, ".tif", sep = ""))

        # Save the masked raster
        writeRaster(masked_raster, filename = output_path, overwrite = TRUE)

        cat("Masked raster for", area, scenario, "saved to", output_path, "\n")
    }
    cat("Finished processing study area", area, "\n")
}

##
# DONE THIS!
##
#########################################################
# RE-SAMPLE THE M_9S Areas HERE! - they need to be at exactly 90 meters

test.rast <- rast("D:/BP_Layers/M_9S/analysis/NPP/npp_sh_s3_old.tif")
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

writeRaster(resampled_raster, "D:/BP_Layers/M_9S/analysis/NPP/npp_sh_s3.tif")

#test.rast2 <- rast("D:/BP_Layers/M_9S/landcover/Sh_30m.tif")

#########################################################
# load in the masked rasters and perform some analysis on them
# Define study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")

# Initialize an empty list to store data frames
df_list <- list()

# Loop through study areas
for (area in study_areas) {

    # Set the input directory based on the current study area
    input_dir <- file.path("D:/BP_Layers", area, "analysis/NPP")

    # List the masked raster files
    raster_files <- list.files(input_dir, pattern = "npp_.*\\.tif$", full.names = TRUE)

    # Check if there are raster files
    if (length(raster_files) > 0) {

        # Read in the raster files
        raster_list <- lapply(raster_files, terra::rast)

        # Combine the rasters into a single stack
        raster_stack <- terra::rast(raster_list)

        # Extract values from the raster stack
        # Create a data frame from the raster stack
        df <- terra::as.data.frame(raster_stack)

        # Extract study area name from the folder path
        study_area_name <- area

        # Extract last characters from the file names
        raster_names <- gsub(".*/npp_sh_(s[0-9]+)\\.tif", paste0(study_area_name, "_\\1"), raster_files)

        # Set column names
        colnames(df) <- raster_names

        # Write the dataframe to a CSV in the same place as the rasters
        csv_path <- file.path(input_dir, paste0(study_area_name, ".csv"))
        write.csv(df, file = csv_path, row.names = FALSE)

        # Add the dataframe to the list
        df_list[[study_area_name]] <- df

        # Print statement indicating that the study area is finished
        cat("Finished writing CSV for study area:", study_area_name, "\n")

    } else {
        cat("Error: No raster files found in", input_dir, "\n")
    }
}

# Combine all data frames into a single data frame
#final_df <- do.call(cbind, df_list)

##########################################################################
###########################################################################

# Define study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")
#study_areas <- c("M_9S", "M_11S")
# Initialize lists to store plots
violin_plots <- list()
bar_plots <- list()
# Initialize an empty dataframe to store the total NPP values
total_npp_df <- data.frame()
mean_npp_df <- data.frame()
# Loop through study areas
for (area in study_areas) {

    # Set the input directory based on the current study area
    input_dir <- file.path("D:/BP_Layers", area, "analysis/NPP")

    # List the CSV file
    csv_file <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

    # Check if there is a CSV file
    if (length(csv_file) == 1) {

        # Read in the CSV
        df <- read.csv(csv_file[[1]])

        # Add a Study_Area column
        df$Study_Area <- area

        # Sample to 10000 rows if necessary
        df_s <- if (nrow(df) >= 10000) {
            df[sample(nrow(df), 10000, replace = FALSE),]
        } else {
         df
        }


        # Reshape the dataframe to long format
        long_df <- tidyr::gather(df_s, key = "Scenario", value = "NPP", -Study_Area)

        # Rename the Scenario column
        long_df$Scenario <- gsub(paste0("^", area, "_"), "", long_df$Scenario)

        # Create violin plot without legend
        # Create violin plot without legend and with no x-axis label
        violin_plot <- ggplot2::ggplot(long_df, ggplot2::aes(x = Study_Area, y = NPP, fill = Scenario)) +
            ggplot2::geom_violin(scale = "width", width = 0.8, alpha = 0.7) +
            ggplot2::geom_boxplot(width = 0.2, position = position_dodge(0.8), color = "black", alpha = 0.5) +
            ggplot2::labs(title = paste(area), y = "Mean NPP (tDM/ha)") +
            ggplot2::theme_bw() +
            theme(legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())

        # Create a separate long dataframe for the entire study area without sampling
        long_df_total <- tidyr::gather(df, key = "Scenario", value = "NPP", -Study_Area)
        long_df_total$Scenario <- gsub(paste0("^", area, "_"), "", long_df_total$Scenario)

        # Calculate total NPP for each scenario
        total_npp_total <- long_df_total %>%
            group_by(Scenario) %>%
            summarise(Sum_NPP = sum(NPP)) %>%
            mutate(Total_NPP = Sum_NPP * (90 * 90) / 10000)

        mean_npp_total <- long_df_total %>%
            group_by(Scenario) %>%
            summarise(Mean_NPP = mean(NPP))

        # Add the Study Area column to the total_npp dataframe
        total_npp_total$Study_Area <- area
        mean_npp_total$Study_Area <- area

        # Combine the total NPP values for each study area and scenario into the initialized dataframe
        total_npp_df <- dplyr::bind_rows(total_npp_df, total_npp_total)
        mean_npp_df <- dplyr::bind_rows(mean_npp_df, mean_npp_total)

        # Create bar plot with total NPP
        bar_plot <- ggplot2::ggplot(total_npp_total, ggplot2::aes(x = Scenario, y = Total_NPP, fill = Scenario)) +
            ggplot2::geom_bar(stat = "identity", position = "dodge") +
            ggplot2::labs(title = paste(area) , y = "Total NPP (tDM)") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
            ggplot2::theme_bw() +
            ggplot2::geom_text(aes(label=round(Total_NPP)), position=position_dodge(width=0.9), vjust=-0.25) +
            theme(legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())

             # Add the plots to the lists
        violin_plots[[area]] <- violin_plot
        bar_plots[[area]] <- bar_plot

        # Print statement indicating that the study area is finished
        cat("Finished processing study area:", area, "\n")

    } else {
        cat("Error: No or multiple CSV files found in", input_dir, "\n")
    }
}

# Print the resulting dataframe with Study Area, Scenario, and total NPP
print(total_npp_df)
print(mean_npp_df)
# Assuming the dataframe is named 'total_npp_df'
total_npp_summary <- total_npp_df %>%
    group_by(Scenario) %>%
    summarise(Total_NPP_Sum = sum(Total_NPP))

# Print the resulting summary
print(total_npp_summary)
#write.csv(total_npp_summary, file = "D:/BP_Layers/analysis/total_NPP_no_fert.csv", row.names = FALSE)

# Combine the modified plots
combined_violin_plot <- cowplot::plot_grid(plotlist = violin_plots, nrow = 2, align = "hv")
# extract legend from plot1
legend <- cowplot::get_legend(
    violin_plots[[1]] +
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
)
# Combine combined plot and legend using plot_grid()
plot_grid(combined_violin_plot, legend, ncol=1, rel_heights = c(1, .1))


combined_bar_plot <- cowplot::plot_grid(plotlist = bar_plots, nrow = 2, align = "hv")
# extract legend from plot1
legend2 <- cowplot::get_legend(
    bar_plots[[1]] +
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
)
# Combine combined plot and legend using plot_grid()
plot_grid(combined_bar_plot, legend2, ncol=1, rel_heights = c(1, .1))



#grid.arrange(grobs = violin_plots, ncol = 2)

# Display the combined plots
print("Combined Violin Plots:")
print(combined_violin_plot)

print("Combined Bar Plots:")
print(combined_bar_plot)

############################################################################################################
############################################################################################################


# SAME THING FOR FERTILIZED NPP
# First we want to sum up our rasters by year:

study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")
study_areas <- c("U_13N")

scenarios <- c("s1", "s2", "s3")

for (area in study_areas) {
    for (scenario in scenarios) {
        # Define paths
        folder_path <- paste0("D:/BP_Layers/NPP_Fert/", area, "/", scenario)
        output_path <- paste0("D:/BP_Layers/NPP_Fert/", area, "/", scenario, "/", "npp_2050_", scenario, "_", area, ".tif")

        # List .flt files for the year 2050
        flt_files <- list.files(folder_path, pattern = "npp2050[0-9]{2}\\.flt$", full.names = TRUE)

        # Check if there are .flt files for the year 2050
        if (length(flt_files) > 0) {
            # Read and stack the rasters
            raster_stack <- rast(flt_files)

            # Sum the layers
            summed_raster <- sum(raster_stack)

            # Write the final summed raster
            writeRaster(summed_raster, filename = output_path, overwrite = TRUE)

            cat("Summed raster for", area, scenario, "and saved to", output_path, "\n")
        } else {
            cat("Error: No .flt files for the year 2050 in", area, scenario, "\n")
        }
    }
    cat("Finished processing study area", area, "\n")
}

############################################################################################################
# Apply as mask to each study area and scenario - shrub mask

# Define study areas
#study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")
study_areas <- c("U_13N")

# Loop through study areas
for (area in study_areas) {

    # Set the output directory based on the current study area
    output_dir <- file.path("D:/BP_Layers", area, "analysis/NPP_Fert")
    # Check if the folder exists, and create it if it doesn't
    if (!file.exists(output_dir)) {
        dir.create(output_dir)
    }

    # Loop through scenarios
    scenarios <- c("s1", "s2", "s3")
    for (scenario in scenarios) {

        # Load in the raster for the current scenario
        raster <- rast(file.path("D:/BP_Layers/NPP_Fert", area, scenario, paste0("npp_2050_", scenario, "_", area, ".tif")))

        # Load the mask
        mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_90m.tif"))

        # Get the EPSG code from the mask
        epsg_code <- crs(mask, describe = TRUE)$code
        # Set the CRS of the raster using the extracted EPSG code
        crs(raster) <- paste0("EPSG:", epsg_code)

        # Apply the mask
        masked_raster <- mask(raster, mask)

        # Create the file path and name for the output raster
        output_path <- file.path(output_dir, paste("npp_sh_", scenario, ".tif", sep = ""))

        # Save the masked raster
        writeRaster(masked_raster, filename = output_path, overwrite = TRUE)

        cat("Masked raster for", area, scenario, "saved to", output_path, "\n")
    }
    cat("Finished processing study area", area, "\n")
}

#########################################################
# RE-SAMPLE THE M_9S Areas HERE! - they need to be at exactly 90 meters

test.rast <- rast("D:/BP_Layers/M_9S/analysis/NPP_Fert/npp_sh_s3_old.tif")
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

writeRaster(resampled_raster, "D:/BP_Layers/M_9S/analysis/NPP_Fert/npp_sh_s3.tif")

#test.rast2 <- rast("D:/BP_Layers/M_9S/landcover/Sh_30m.tif")



#########################################################
# load in the masked rasters and perform some analysis on them
# Define study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")

# Initialize an empty list to store data frames
df_list <- list()

# Loop through study areas
for (area in study_areas) {

    # Set the input directory based on the current study area
    input_dir <- file.path("D:/BP_Layers", area, "analysis/NPP_Fert")

    # List the masked raster files
    raster_files <- list.files(input_dir, pattern = "npp_.*\\.tif$", full.names = TRUE)

    # Check if there are raster files
    if (length(raster_files) > 0) {

        # Read in the raster files
        raster_list <- lapply(raster_files, terra::rast)

        # Combine the rasters into a single stack
        raster_stack <- terra::rast(raster_list)

        # Extract values from the raster stack
        # Create a data frame from the raster stack
        df <- terra::as.data.frame(raster_stack)

        # Extract study area name from the folder path
        study_area_name <- area

        # Extract last characters from the file names
        raster_names <- gsub(".*/npp_sh_(s[0-9]+)\\.tif", paste0(study_area_name, "_\\1"), raster_files)

        # Set column names
        colnames(df) <- raster_names

        # Write the dataframe to a CSV in the same place as the rasters
        csv_path <- file.path(input_dir, paste0(study_area_name, ".csv"))
        write.csv(df, file = csv_path, row.names = FALSE)

        # Add the dataframe to the list
        df_list[[study_area_name]] <- df

        # Print statement indicating that the study area is finished
        cat("Finished writing CSV for study area:", study_area_name, "\n")

    } else {
        cat("Error: No raster files found in", input_dir, "\n")
    }
}

# Combine all data frames into a single data frame
#final_df <- do.call(cbind, df_list)

# Define study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")
#study_areas <- c("M_9S", "M_11S")
# Initialize lists to store plots
violin_plots_fert <- list()
bar_plots_fert <- list()

# Initialize an empty dataframe to store the total NPP values
total_npp_df_fert <- data.frame()

# Loop through study areas
for (area in study_areas) {

    # Set the input directory based on the current study area
    input_dir <- file.path("D:/BP_Layers", area, "analysis/NPP_Fert")

    # List the CSV file
    csv_file <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

    # Check if there is a CSV file
    if (length(csv_file) == 1) {

        # Read in the CSV
        df <- read.csv(csv_file[[1]])

        # Add a Study_Area column
        df$Study_Area <- area

        # Sample to 10000 rows if necessary
        df_s <- if (nrow(df) >= 10000) {
            df[sample(nrow(df), 10000, replace = FALSE),]
        } else {
            df
        }


        # Reshape the dataframe to long format
        long_df <- tidyr::gather(df_s, key = "Scenario", value = "NPP", -Study_Area)

        # Rename the Scenario column
        long_df$Scenario <- gsub(paste0("^", area, "_"), "", long_df$Scenario)

        # Create violin plot without legend
        # Create violin plot without legend and with no x-axis label
        violin_plot <- ggplot2::ggplot(long_df, ggplot2::aes(x = Study_Area, y = NPP, fill = Scenario)) +
            ggplot2::geom_violin(scale = "width", width = 0.8, alpha = 0.7) +
            ggplot2::geom_boxplot(width = 0.2, position = position_dodge(0.8), color = "black", alpha = 0.5) +
            ggplot2::labs(title = paste(area), y = "Mean NPP (tDM/ha)") +
            ggplot2::theme_bw() +
            theme(legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())

        # Create a separate long dataframe for the entire study area without sampling
        long_df_total <- tidyr::gather(df, key = "Scenario", value = "NPP", -Study_Area)
        long_df_total$Scenario <- gsub(paste0("^", area, "_"), "", long_df_total$Scenario)

        # Calculate total NPP for each scenario
        total_npp_total <- long_df_total %>%
            group_by(Scenario) %>%
            summarise(Sum_NPP = sum(NPP)) %>%
            mutate(Total_NPP = Sum_NPP * (90 * 90) / 10000)

        # Add the Study Area column to the total_npp dataframe
        total_npp_total$Study_Area <- area

        # Combine the total NPP values for each study area and scenario into the initialized dataframe
        total_npp_df_fert <- dplyr::bind_rows(total_npp_df_fert, total_npp_total)

        # Create bar plot with total NPP
        bar_plot <- ggplot2::ggplot(total_npp_total, ggplot2::aes(x = Scenario, y = Total_NPP, fill = Scenario)) +
            ggplot2::geom_bar(stat = "identity", position = "dodge") +
            ggplot2::labs(title = paste(area) , y = "Total NPP (tDM)") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
            ggplot2::theme_bw() +
            ggplot2::geom_text(aes(label=round(Total_NPP)), position=position_dodge(width=0.9), vjust=-0.25) +
            theme(legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())

        # Add the plots to the lists
        violin_plots_fert[[area]] <- violin_plot
        bar_plots_fert[[area]] <- bar_plot

        # Print statement indicating that the study area is finished
        cat("Finished processing study area:", area, "\n")

    } else {
        cat("Error: No or multiple CSV files found in", input_dir, "\n")
    }
}

# Print the resulting dataframe with Study Area, Scenario, and total NPP
print(total_npp_df_fert)
# Assuming the dataframe is named 'total_npp_df_fert'
total_npp_summary_fert <- total_npp_df_fert %>%
    group_by(Scenario) %>%
    summarise(Total_NPP_Sum = sum(Total_NPP))

# Print the resulting summary
print(total_npp_summary_fert)
#write.csv(total_npp_summary_fert, file = "D:/BP_Layers/analysis/total_NPP_with_fert.csv", row.names = FALSE)

######################################################################


total_npp_df$fert <- "no"

total_npp_df_fert$fert <- "yes"
total_npp <- rbind(total_npp_df, total_npp_df_fert)
total_npp <- total_npp %>%
    select(-Sum_NPP)

total_npp_summary$fert <- "no"
total_npp_summary_fert$fert <- "yes"

total_npp_comb <- rbind(as.data.frame(total_npp_summary), as.data.frame(total_npp_summary_fert))
total_npp_comb$Study_Area <- "all"
total_npp_comb <- total_npp_comb %>%
    rename(Total_NPP = Total_NPP_Sum)


total_npp_all <- rbind(total_npp, total_npp_comb)

# CREATE A DATAFRAME CALLED area_df


