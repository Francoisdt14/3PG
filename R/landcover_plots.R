

########################################################
test.rast <- rast("D:/BP_Layers/M_9S/landcover/Sh_30m_old.tif")
# Assuming 'raster_data' is the name of your raster object
# Replace 'raster_data' with the actual name of your raster object

# Define the target resolution and extent
target_res <- 30  # Target resolution in x and y directions
target_ex <- ext(test.rast)  # Use the extent of the original raster
# Define the target CRS
target_crs <- "EPSG:32609"  # Replace with your desired CRS, e.g., "EPSG:32630" for UTM zone 30N

# Create a new raster with the target extent, resolution, and CRS
target_raster <- rast(target_ex, res = target_res, crs = target_crs)

# Resample the original raster to the target resolution
resampled_raster <- resample(test.rast, target_raster, method = "near", threads = T)

writeRaster(resampled_raster, "D:/BP_Layers/M_9S/landcover/Sh_30m.tif")

test.rast2 <- rast("D:/BP_Layers/M_9S/landcover/Sh_30m.tif")

########################################################



vlce <- read.csv("D:/BP_Layers/M_9S/vlce.csv") %>% as.data.frame()


# List of study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N")

# Create an empty list to store the plots
stack_bar_list <- list()

# Loop through each study area
for (area in study_areas) {
    # Read the landcover raster
    landcover_raster <- rast(paste0("D:/BP_Layers/", area, "/landcover/landcover_30m.tif"))

    # Calculate proportions
    landcover_freq <- table(values(landcover_raster))
    landcover_prop <- landcover_freq / sum(landcover_freq)
    landcover_prop2 <- as.data.frame(landcover_prop)
    landcover_prop2 <- landcover_prop2 %>% mutate(Var1 = as.integer(as.character(Var1)))

    # Join the dataframes and multiply the values by 100
    result <- left_join(landcover_prop2, vlce, by = c("Var1" = "class_val")) %>%
        mutate(Freq = Freq * 100)

    # Create a stacked bar chart
    stacked_bar_chart <- ggplot(result, aes(x = "", y = Freq, fill = name_clean)) +
        geom_bar(stat = "identity", width = 1) +
        coord_flip() +
        labs(title = paste(area), fill = "Landcover Type") +
        scale_fill_manual(values = result$ntems_colour,
                          breaks = result$name_clean,
                          labels = result$name_clean) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(y = "Percentage of Landcover", x = NULL)


    # Add the plot to the list
    stack_bar_list[[area]] <- stacked_bar_chart
}

# Combine the plots using cowplot
combined_stack_plot <- cowplot::plot_grid(plotlist = stack_bar_list, ncol = 2, align = "hv")

# extract legend from plot1
legend3 <- cowplot::get_legend(
    stack_bar_list[[1]] +
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
)

# Combine combined plot and legend using plot_grid()
cowplot::plot_grid(combined_stack_plot, legend3, ncol=1, rel_heights = c(1, .1))



# Display the combined plot
print(combined_plot)


################################################################################################################
# Create an empty list to store the total cells for each land cover class in each study area
total_cells_list <- list()
# Loop through each study area
for (area in study_areas) {
    # Read the landcover raster
    landcover_raster <- rast(paste0("D:/BP_Layers/", area, "/landcover/landcover_30m.tif"))

    # Calculate cell frequencies
    landcover_freq <- as.data.frame(table(values(landcover_raster)))
    colnames(landcover_freq) <- c("class_val", "cell_count")

    # Convert class_val to integer
    landcover_freq$class_val <- as.integer(as.character(landcover_freq$class_val))

    # Join with vlce to get class names
    result <- left_join(landcover_freq, vlce, by = "class_val")

    # Store the result in the list
    total_cells_list[[area]] <- result


}

# View the total cells for each land cover class in each study area <- <-
