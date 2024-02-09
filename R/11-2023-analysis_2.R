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

final_df_summary_df3 <- rbind(final_summary_df2, final_summary_df2_fert)

# Assuming 'final_summary_df' is your dataframe
final_summary_df4 <- final_df_summary_df3 %>%
    mutate(total_biomass = Sum* (90 * 90) / 10000)


#write.csv(final_summary_df4, file = "D:/BP_Layers/analysis/study_area_summary_all.csv", row.names = FALSE)

final_summary_df3 <- read.csv("D:/BP_Layers/analysis/study_area_summary_all.csv")

# Filter the dataframe to include only 'Sh' and 'ShBy' rows
ShBy_df <- final_summary_df3 %>%
    filter(grepl("ShBy", Raster) & !grepl("_fao", Raster) & !grepl("Y", Fert))

# Only keep the Shrub layer
Sh_df <- final_summary_df3 %>%
    filter(grepl("Sh", Raster) & !grepl("ShBy", Raster) & !grepl("_fao", Raster) & !grepl("Y", Fert))

# FAO
# Filter the dataframe to include only fao rows
fao_df <- final_summary_df3 %>%
    filter(!grepl("ShBy|Sh", Raster) & !grepl("Y", Fert))

#
#filtered_df4 <- final_summary_df3 %>%
# filter(!grepl("_", Raster))

# Only keep the Shrub layers
Sh_ShBy_df <- final_summary_df3 %>%
    filter(grepl("Sh", Raster) & !grepl("_fao", Raster) & !grepl("Y", Fert))


# Create a bar graph with facets for Sum values
ggplot(fao_df, aes(x = Study_Area, y = total_biomass, fill = Study_Area)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~Scenario, scales = "free") +
    labs(title = "Total Biomass in the FAO Forest Layer by Study Area and Scenario",
         x = "Study Area", y = "Total Biomass (Tons)") +
    theme_bw()


# Create a bar graph with facets for Sum values
ggplot(ShBy_df, aes(x = Study_Area, y = total_biomass, fill = Scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass in the FAO Forest Layer by Study Area and Scenario",
         x = "Study Area", y = "Total Biomass (Tons)") +
    theme_bw()


# Create a box plot faceted by Study Area and Scenario FAO
ggplot(fao_df, aes(x = Study_Area, y = Mean, ymin = p25, lower = p25, middle = Mean, upper = p75, ymax = p75, fill = Study_Area)) +
    geom_boxplot(stat = "identity", position = position_dodge(width = 0.9)) +
    facet_wrap(~ Scenario, scales = "free") +
    labs(title = "Mean Aboveground Biomass (ABG, tons/ha) by Study Area and Scenario - Forested Areas", x = "Study Area", y = "Mean ABG (tons/ha) ") +
    theme_bw()

# Create the plot to compare the mean_per_hectare by scenario and managed
ggplot(Sh_ShBy_df, aes(x = Scenario, y = Mean, color = Raster, shape = Managed, label = Study_Area)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_text_repel(position = position_dodge(width = 0.5), box.padding = 0.5) +
    labs(title = "Mean Aboveground Biomass (ABG, per Hectare) Across Different Climate Scenarios for Different Masks", x = "Scenario", y = "Mean Biomass (Tons per Hectare)", color = "Mask", shape = "Managed/Unmanaged") +
    scale_color_manual(values = c("ShBy" = "blue", "Sh" = "red")) +
    scale_shape_manual(values = c("M" = 16, "U" = 17)) +
    facet_wrap(~Scenario, scales = "free") +
    theme_bw()


#######################################################################################
# Create a box plot faceted by Study Area and Scenario JUST SHRUBS
ggplot(Sh_df, aes(x = Study_Area, y = Mean, ymin = p25, lower = p25, middle = Mean, upper = p75, ymax = p75, fill = Study_Area)) +
    geom_boxplot(stat = "identity", position = position_dodge(width = 0.9)) +
    facet_wrap(~ Scenario, scales = "free") +
    labs(title = "Mean Values by Study Area and Scenario", x = "Study Area", y = "Mean Value") +
    theme_bw()

# Create a box plot faceted by Study Area and Scenario SHRUBS AND BRYOIDS
ggplot(ShBy_df, aes(x = Study_Area, y = Mean, ymin = p25, lower = p25, middle = Mean, upper = p75, ymax = p75, fill = Study_Area)) +
    geom_boxplot(stat = "identity", position = position_dodge(width = 0.9)) +
    facet_wrap(~ Scenario, scales = "free") +
    labs(title = "Mean Values by Study Area and Scenario", x = "Study Area", y = "Mean Value") +
    theme_bw()




# Create a box plot faceted by Study Area and Scenario SHRUBS AND BRYOIDS
ggplot(ShBy_df, aes(x = Scenario, y = Mean, ymin = p25, lower = p25, middle = Mean, upper = p75, ymax = p75, fill = Scenario)) +
    geom_boxplot(stat = "identity", position = position_dodge(width = 0.9)) +
    facet_wrap(~ Study_Area, scales = "free") +
    labs(title = "Mean Values by Study Area and Scenario", x = "Study Area", y = "Mean Value") +
    theme_bw()



############################################################################################
# Create a bar graph with facets for Sum values
ggplot(ShBy_df, aes(x = Study_Area, y = total_biomass, fill = Scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass in the FAO Forest Layer by Study Area and Scenario",
         x = "Study Area", y = "Total Biomass (Tons)") +
    theme_bw()


##########

# Find the minimum total biomass value for each study area
min_values <- aggregate(total_biomass ~ Study_Area, ShBy_df, min)
max_values <- aggregate(total_biomass ~ Study_Area, ShBy_df, max)

# Subtract a specific amount (e.g., 500,000) from each minimum total biomass
#min_values$start_value <- min_values$total_biomass - 1000000
min_values$start_value <- min_values$total_biomass - 500000
max_values$end_value <- max_values$total_biomass + 500000

# Merge the minimum values back to the original dataframe
ShBy_df_2 <- merge(ShBy_df, min_values[c("Study_Area", "start_value")], by = "Study_Area")
ShBy_df_3 <- merge(ShBy_df_2, max_values[c("Study_Area", "end_value")], by = "Study_Area") %>%
    mutate(total_biomass = round(total_biomass, 0)) %>%
    mutate(start_value = round(start_value, 0)) %>%
    mutate(end_value = round(end_value, 0))
##########################################################################################################################
area_plots <- list()

# Loop through unique Study_Area in the dataframe
for (selected_area in unique(ShBy_df_3$Study_Area)) {
    # Subset the dataframe for the current Study_Area
    subset_df <- ShBy_df_3[ShBy_df_3$Study_Area == selected_area, ]

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



#############################################################################################################################

a <- ShBy_df_3 %>%
    filter(Study_Area == 'M_18S') %>%
    dplyr::select(Study_Area, Scenario, total_biomass, start_value, end_value)


lower <- a %>%
    ggplot(aes(x = Scenario, y = total_biomass)) +
    geom_bar(stat = "identity", aes(fill = Scenario),
             width = 0.7, color = "black") +
    geom_text(aes(label = total_biomass), vjust = -0.2) +
    scale_fill_manual(values = brewer.pal(8, "Set2")) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(hjust =1 )) +
    coord_cartesian(ylim = c(a$start_value[1], a$end_value[1]))


# Subset the dataframe for the specified study area
plot_data <- ShBy_df_3 %>%
    dplyr::select(Study_Area, Scenario, total_biomass, start_value, end_value)

# Create a bar plot with facets for each study area
ggplot(plot_data, aes(x = Scenario, y = total_biomass)) +
    geom_bar(stat = "identity", aes(fill = Scenario),
             width = 0.7, color = "black") +
    geom_text(aes(label = total_biomass), vjust = -0.2) +
    scale_fill_manual(values = brewer.pal(8, "Set2")) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(hjust = 1)) +
    facet_grid(Study_Area ~ ., scales = "free_y") +
    coord_cartesian(ylim = c(min(plot_data$start_value), max(plot_data$end_value)))



# Create a bar graph with facets for total biomass values
ggplot(ShBy_df_2, aes(x = Study_Area, y = total_biomass - start_value, fill = Scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass in the FAO Forest Layer by Study Area and Scenario",
         x = "Study Area", y = "Total Biomass (Tons)") +
    theme_bw()


ggplot(ShBy_df_2, aes(x = Scenario, y = total_biomass - start_value, fill = Scenario)) +
    geom_col(position = "dodge", width = 0.7, aes(ymin = start_value)) +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass in the FAO Forest Layer by Study Area and Scenario",
         x = "Scenario", y = "Total Biomass (Tons)",
         subtitle = "Start value reflected on Y-axis") +
    theme_bw()


# Create a bar graph with facets for total biomass values
ggplot(ShBy_df_2, aes(x = Scenario, y = total_biomass - start_value, fill = Scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~Study_Area) +
    labs(title = "Total Biomass in the FAO Forest Layer by Study Area and Scenario",
         x = "Scenario", y = "Total Biomass (Tons)",
         subtitle = "Start value reflected on Y-axis") +
    theme_bw()

##########
# MAYBE

# Set a threshold for the y-axis
threshold <- 7.5e8

# Plot for the overall sum for each scenario and study area
p <- ggplot(filtered_df3, aes(x = Study_Area, y = Sum, fill = Scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Overall Sum by Study Area and Scenario",
         x = "Study Area",
         y = "Overall Sum") +
    theme_minimal()

# Set y-axis limits
p + coord_cartesian(ylim = c(threshold, max(final_summary_df3$Sum)))






#############################################


# More testing
# Can we load the data for multiple years (ws + wf and plot them over time?)
# Per study area? On the same plot?
# Relative to '0' ?



# Biomass by elevation





#############################################################
# Data over time - THIS IS TO PRESENT TIME


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


##############################################################
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
###############################################################


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

######################################################################

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

###########################################################################


##########################################################################


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


# masked fao example in 2050

w1 <- rast("D:/BP_Layers/M_9S/analysis/s3_ShBy_mask.tif")
w2 <- rast("D:/BP_Layers/M_11S/analysis/s3_ShBy_mask.tif")
w3 <- rast("D:/BP_Layers/M_18S/analysis/s3_ShBy_mask.tif")
w4 <- rast("D:/BP_Layers/U_18S/analysis/s3_ShBy_mask.tif")
w5 <- rast("D:/BP_Layers/U_15S/analysis/s3_ShBy_mask.tif")
w6 <- rast("D:/BP_Layers/U_13N/analysis/s3_ShBy_mask.tif")


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
    filter(grepl("ShBy", Raster) & !grepl("_fao", Raster))

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
    filter(grepl("Sh", Raster) & !grepl("_fao", Raster))

names(Sh_ShBy_df_2)[names(Sh_ShBy_df_2) == "Raster"] <- "Mask"

# Plot biomass by climate scenario, fertilization status, and mask, facet by study area
ggplot(Sh_ShBy_df_2, aes(x = Scenario, y = total_biomass, fill = Fert, color = Mask)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass Comparison by Climate Scenario, Fertilization, and Mask",
         x = "Climate Scenario", y = "Total Biomass (Tons)") +
    scale_fill_manual(values = c("N" = "cadetblue", "Y" = "lightgreen")) +  # Set your own colors for Fert
    theme_bw()


# Plot biomass by climate scenario, fertilization status, and mask, facet by study area
ggplot(Sh_ShBy_df_2, aes(x = Scenario, y = total_biomass, fill = Fert, color = Mask)) +
    geom_bar(stat = "identity", position = "dodge", size = 1.5) +  # Adjust pattern and size as needed
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass Comparison by Climate Scenario, Fertilization, and Mask",
         x = "Climate Scenario", y = "Total Biomass (Tons)") +
    scale_fill_manual(values = c("N" = "cadetblue", "Y" = "lightgreen")) +  # Set your own colors for Fert
    theme_bw()


# Create the plot with cross-hatched patterns for the "Mask" variable and change the color of the "Mask" outlines
ggplot(Sh_ShBy_df_2, aes(x = Scenario, y = total_biomass, fill = Fert, color = Mask)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), size = 1) +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass Comparison by Climate Scenario, Fertilization, and Mask",
         x = "Climate Scenario", y = "Total Biomass (Tons)") +
    scale_fill_manual(values = c("N" = "cadetblue", "Y" = "lightgreen")) +
    scale_color_manual(values = c("ShBy" = "mediumpurple", "Sh" = "orange2")) +  # Change the color of the "Mask" outlines
    theme_bw() +
    theme(legend.position="top") +
    theme(legend.key.size = unit(0.5, "cm")) +
    guides(fill=guide_legend(override.aes=list(colour=c("black", "black"))))


# Create a new variable for adjusted positioning
Sh_ShBy_df_3 <- Sh_ShBy_df_2 %>%
    group_by(Study_Area, Scenario) %>%
    mutate(position_adjust = ifelse(Mask == "Sh", 0.5, 0))

# Create the plot with adjusted positions for "Mask"
ggplot(Sh_ShBy_df_3, aes(x = Scenario, y = total_biomass, fill = Fert, color = Mask)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), size = 1) +
    facet_wrap(~Study_Area, scales = "free") +
    labs(title = "Total Biomass Comparison by Climate Scenario, Fertilization, and Mask",
         x = "Climate Scenario", y = "Total Biomass (Tons)") +
    scale_fill_manual(values = c("N" = "cadetblue", "Y" = "lightgreen")) +
    scale_color_manual(values = c("ShBy" = "mediumpurple", "Sh" = "orange2")) +
    theme_bw() +
    theme(legend.position = "top") +
    theme(legend.key.size = unit(0.5, "cm")) +
    guides(fill = guide_legend(override.aes = list(colour = c("black", "black"))))

# Sum total biomass for each combination of Scenario, Fert, and Study_Area
summarized_df <- ShBy_df_2 %>%
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



Sh_ShBy_df_2_total <- Sh_ShBy_df_2 %>%
    dplyr::select(Study_Area, Mask, Scenario, Managed, Fert, total_biomass)

Sh_ShBy_df_2_sums <- Sh_ShBy_df_2_total %>%
    group_by(Mask, Scenario, Managed, Fert, .drop = TRUE) %>%
    summarize(total_biomass = sum(total_biomass), .groups = 'drop_last')

# Print the updated data frame
print(Sh_ShBy_df_2_total)

Sh_ShBy_df_2_sums_sh <- Sh_ShBy_df_2_sums %>%
    dplyr::filter(Mask == "Sh", Managed == "M")

# Define the reference value
reference_value <- subset(Sh_ShBy_df_2_sums_sh, Scenario == "s2" & Managed == "M" & Fert == "N")$total_biomass

# Create a new variable indicating whether total biomass is higher, lower, or equal to the reference
Sh_ShBy_df_2_sums_sh <- Sh_ShBy_df_2_sums_sh %>%
    mutate(Comparison = case_when(
        total_biomass > reference_value ~ "Higher",
        total_biomass < reference_value ~ "Lower",
        TRUE ~ "Equal"
    ))

#######################################################################################
# Load the required libraries
library(plotly)

# Filter the dataframe by 'Managed' = M
filtered_df <- Sh_ShBy_df_2_sums_sh[Sh_ShBy_df_2_sums_sh$Managed == "M", ]

# Create the 3D plot
plot <- plot_ly(filtered_df, x = ~Scenario, y = ~Fert, z = ~total_biomass, color = ~Comparison, colors = c("red", "grey", "green"), type = "scatter3d", mode = "markers")

# Add labels and title
plot <- plot %>% layout(scene = list(xaxis = list(title = "Scenario"), yaxis = list(title = "Fert"), zaxis = list(title = "Total Biomass")))

# Show the plot
plot
##################################################################################

# Convert Comparison to a factor to maintain the order in the plot
Sh_ShBy_df_2_sums_sh$Comparison <- factor(Sh_ShBy_df_2_sums_sh$Comparison, levels = c("Lower", "Equal", "Higher"))

# Create the 3D plot
plot_ly(Sh_ShBy_df_2_sums_sh, x = ~Scenario, y = ~Fert, z = ~total_biomass, color = ~Comparison,
        colors = c("red", "grey", "green"), type = "scatter3d", mode = "markers") %>%
    layout(scene = list(xaxis = list(title = "Scenario"),
                        yaxis = list(title = "Fert"),
                        zaxis = list(title = "Total Biomass")),
           title = "3D Plot of Total Biomass by Scenario and Fert",
           showlegend = TRUE)


###############################################################################
install.packages("orca")

library(plotly)
library(orca)

# Convert Comparison to a factor to maintain the order in the plot
Sh_ShBy_df_2_sums_sh$Comparison <- factor(Sh_ShBy_df_2_sums_sh$Comparison, levels = c("Lower", "Equal", "Higher"))

# Create the 3D plot with adjusted color scale
plot_ly(Sh_ShBy_df_2_sums_sh, x = ~Scenario, y = ~Fert, z = ~total_biomass, color = ~Comparison,
        colorscale = list(c(0, 1), c("red", "grey", "green")), type = "scatter3d", mode = "markers") %>%
    layout(scene = list(xaxis = list(title = "Scenario"),
                        yaxis = list(title = "Fert"),
                        zaxis = list(title = "Total Biomass")),
           title = "3D Plot of Total Biomass by Scenario and Fert",
           showlegend = TRUE)

# Export the plot to PNG
#orca("3d_plot.png", width = 800, height = 600)





library(rayshader)
library(ggplot2)
d <- read.table(text=' x   y     z
t1   5   high
t1   2   low
t1   4   med
t2   8   high
t2   1   low
t2   3   med
t3  50   high
t3  12   med
t3  35   low', header=TRUE)

p <- ggplot(d, aes(x, z, fill = y)) +
    geom_tile() +
    scale_fill_fermenter(type = "div", palette = "RdYlBu")



d <- read.table(text=' x   y     z
t1   5   high
t1   2   low
t1   4   med
t2   8   high
t2   1   low
t2   3   med
t3  50   high
t3  12   med
t3  35   low', header=TRUE)

library(latticeExtra)

cloud(y~x+z, d, panel.3d.cloud=panel.3dbars, col.facet='grey',
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))



Sh_ShBy_df_2_sums_sh


test <- as.data.frame(Sh_ShBy_df_2_sums_sh)

test$Scenario <- as.factor(test$Scenario)
test$Fert <- as.factor(test$Fert)

cloud(total_biomass~Scenario+Fert, test, panel.3d.cloud=panel.3dbars, col.facet='grey',
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))


# Define a color gradient from light green to dark green
color_gradient <- colorRampPalette(c("lightgreen", "darkgreen"))

# Create a custom color vector based on total_biomass values
color_vector <- color_gradient(nrow(test))

# Plot using cloud and panel.3dbars
cloud(
    total_biomass ~ Scenario + Fert,
    data = test,
    panel.3d.cloud = panel.3dbars,
    col.facet=color_vector,
    xbase = 0.4,
    ybase = 0.4,
    scales = list(arrows = FALSE, col = 1),
    par.settings = list(axis.line = list(col = "transparent")),
    col = color_vector
)




Sh_ShBy_df_2_means <- Sh_ShBy_df_2 %>%
    dplyr::select(Study_Area, Mask, Scenario, Managed, Fert, Mean) %>%
    filter(Mask == 'Sh')

scenario_managed_fert_means <- Sh_ShBy_df_2_means %>%
    group_by(Scenario, Managed, Fert) %>%
    summarize(mean_of_mean = mean(Mean))


test2 <- scenario_managed_fert_means %>%
    filter(Managed == 'M')
test2$Scenario <- as.factor(test2$Scenario)
test2$Fert <- as.factor(test2$Fert)

cloud(mean_of_mean~Scenario+Fert, test2, panel.3d.cloud=panel.3dbars, col.facet='grey',
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))

