# Load required packages
library(terra)
library(tidyverse)

output_dir <- "D:/BP_Layers/M_9S/analysis"

# Load in the stem and foliage values at 2080 for each scenario - this is lodgepole pine

s1.ws <- rast("D:/3PG_Cplusplus/_delete/S1_Y4/ws.flt")
s1.wf <- rast("D:/3PG_Cplusplus/_delete/S1_Y4/wf.flt")

s2.ws <- rast("D:/3PG_Cplusplus/_delete/S2_Y4/ws.flt")
s2.wf <- rast("D:/3PG_Cplusplus/_delete/S2_Y4/wf.flt")

s3.ws <- rast("D:/3PG_Cplusplus/_delete/S3_Y4/ws.flt")
s3.wf <- rast("D:/3PG_Cplusplus/_delete/S3_Y4/wf.flt")

# calculate agb for each scenario in 2080
s1 <- s1.ws + s1.wf
s2 <- s2.ws + s2.wf
s3 <- s3.ws + s3.wf

# Load stem and foliage for an example of black spruce
s2.ws.bs <- rast("D:/3PG_Cplusplus/_delete/S2_bs_Y4_output/ws.flt")
s2.wf.bs <- rast("D:/3PG_Cplusplus/_delete/S2_bs_Y4_output/wf.flt")
# calculate agb
s2.bs <- s2.ws.bs + s2.wf.bs

# load the boosted values - Y4 values
s2.ws.boosted <- rast("D:/3PG_Cplusplus/_delete/Boosted_S2_lp_Y4_output/ws.flt")
s2.wf.boosted <- rast("D:/3PG_Cplusplus/_delete/Boosted_S2_lp_Y4_output/wf.flt")
# add the boosted values
s2.boosted <- s2.ws.boosted + s2.wf.boosted

# load the boosted values Y2 values
s2.ws.boosted.y2 <- rast("D:/3PG_Cplusplus/_delete/Boosted_S2_lp_Y2_output/ws.flt")
s2.wf.boosted.y2 <- rast("D:/3PG_Cplusplus/_delete/Boosted_S2_lp_Y2_output/wf.flt")
# add the boosted values
s2.boosted.y2 <- s2.ws.boosted.y2 + s2.wf.boosted.y2

# load the boosted values
s2.ws.y2 <- rast("D:/3PG_Cplusplus/_delete/S2_lp_Y2_output/ws.flt")
s2.wf.y2 <- rast("D:/3PG_Cplusplus/_delete/S2_lp_Y2_output/wf.flt")
# add the boosted values
s2.y2 <- s2.ws.y2 + s2.wf.y2



# Load the masks
fao_mask <- rast("D:/BP_Layers/M_9S/landcover/fao_forest_90m.tif")
shrub_mask <- rast("D:/BP_Layers/M_9S/landcover/shrub_mask_90m.tif")
shrub_plus_fao_mask <-rast("D:/BP_Layers/M_9S/landcover/shrub_and_FAO_90m_mask.tif")

# Create a list of scenarios and masks
scenarios <- list(s1, s2, s3)
mask_names <- c("fao_mask", "shrub_mask", "shrub_plus_fao_mask")

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
        writeRaster(masked_scenario, filename = paste(output_path,".tif", sep = ""), overwrite = T)

        masked_rasters[[output_path]] <- masked_scenario
    }
}

s1.fao.mask <- rast("D:/BP_Layers/M_9S/analysis/s1_fao_mask.tif")
s1.shrub.mask <- rast("D:/BP_Layers/M_9S/analysis/s1_shrub_mask.tif")
s1.shrub.fao.mask <- rast("D:/BP_Layers/M_9S/analysis/s1_shrub_plus_fao_mask.tif")

s2.fao.mask <- rast("D:/BP_Layers/M_9S/analysis/s2_fao_mask.tif")
s2.shrub.mask <- rast("D:/BP_Layers/M_9S/analysis/s2_shrub_mask.tif")
s2.shrub.fao.mask <- rast("D:/BP_Layers/M_9S/analysis/s2_shrub_plus_fao_mask.tif")

s3.fao.mask <- rast("D:/BP_Layers/M_9S/analysis/s3_fao_mask.tif")
s3.shrub.mask <- rast("D:/BP_Layers/M_9S/analysis/s3_shrub_mask.tif")
s3.shrub.fao.mask <- rast("D:/BP_Layers/M_9S/analysis/s3_shrub_plus_fao_mask.tif")

#s.diff <- s3.fao.mask-s1.fao.mask
#s.diff.clamp <- clamp(s.diff, lower = -10, upper = Inf)


s2.bs.shrub.fao.mask <- mask(s2.bs, shrub_plus_fao_mask)
s2.bs.shrub.mask <- mask(s2.bs, shrub_mask)
s2.boosted.shrub.fao.mask <- mask(s2.boosted, shrub_plus_fao_mask)
s2.boosted.shrub.mask <- mask(s2.boosted, shrub_mask)

s2.boosted.y2.shrub.mask <- mask(s2.boosted.y2, shrub_mask)
s2.y2.shrub.mask <- mask(s2.y2, shrub_mask)


library(viridis)
plot(s2.bs.shrub.mask, col = viridis(100))

plot(s2.boosted.shrub.fao.mask, col = viridis(100))

plot(s.diff.clamp, col = magma(100))

# Reclassify values in s.diff - this is not working
s.diff_reclassified <- classify(s.diff, c(0, Inf, 1, -Inf, 0, -1))
s.diff_reclassified <- ifelse(values(s.diff) > 0, 1, -1)
# Create a custom color vector
custom_colors <- c("red", "forestgreen")

# Create a plot with custom colors
plot(s.diff_reclassified, col = custom_colors)


#
s1.fao.mask.sum <- global(s1.fao.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s1.shrub.mask.sum <- global(s1.shrub.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s1.shrub.fao.sum <- global(s1.shrub.fao.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)


s2.fao.mask.sum <- global(s2.fao.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s2.shrub.mask.sum <- global(s2.shrub.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s2.shrub.fao.sum <- global(s2.shrub.fao.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)


s3.fao.mask.sum <- global(s3.fao.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s3.shrub.mask.sum <- global(s3.shrub.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s3.shrub.fao.sum <- global(s3.shrub.fao.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)

s2.bs.shrub.fao.sum <- global(s2.bs.shrub.fao.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s2.bs.shrub.sum <- global(s2.bs.shrub.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s2.boosted.shrub.fao.sum <- global(s2.boosted.shrub.fao.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s2.boosted.shrub.sum <- global(s2.boosted.shrub.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)

s2.boosted.y2.shrub.sum <- global(s2.boosted.y2.shrub.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
s2.y2.shrub.sum <- global(s2.y2.shrub.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)


# Assuming you have already calculated the summaries for all rasters

# Create a list of summary data for each raster
summary_list <- list(
    s1.fao.mask.sum, s1.shrub.fao.sum,
    s2.fao.mask.sum, s2.shrub.fao.sum,
    s3.fao.mask.sum, s3.shrub.fao.sum
)

# Raster and scenario names
raster_names <- c("s1.fao.mask", "s1.shrub.fao.mask",
                  "s2.fao.mask", "s2.shrub.fao.mask",
                  "s3.fao.mask", "s3.shrub.fao.mask")

# Create a data frame with raster names and summary statistics
summary_data <- data.frame(RasterName = raster_names)

# Add columns for each statistic
for (stat in c("mean", "sum", "min", "max")) {
    summary_data[stat] <- sapply(summary_list, function(summary) summary[[stat]])
}

# Print the summary data frame
print(summary_data)

########################################################################################
# PLOTS

# Create a new column for the scenario
summary_data$scenario <- gsub("\\..*", "", summary_data$RasterName)

# Filter the data to include only "sum" values
sum_data <- subset(summary_data, select = c(RasterName, scenario, sum))

# Create a separate plot for each scenario
plots <- lapply(unique(sum_data$scenario), function(scenario) {
    scenario_data <- subset(sum_data, scenario == scenario)
    ggplot(scenario_data, aes(x = RasterName, y = sum, fill = RasterName)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = ifelse(grepl("shrub", scenario_data$RasterName), "dodgerblue4", "green4")) +
        labs(x = "RasterName", y = "Sum", fill = "RasterName") +
        ggtitle(paste("Scenario", scenario)) +
        theme(legend.position = "top") +
        theme_bw() # Set the black and white theme
})

# Display the plots
plots

# Assuming you have a dataframe named 'sum_data'
# Filter the dataframe to select only the specific scenarios you want to compare
scenarios_to_compare <- c("s1.fao.mask", "s2.fao.mask", "s3.fao.mask")
filtered_data <- sum_data[sum_data$RasterName %in% scenarios_to_compare, ]

# Create a bar plot for the selected scenarios
plot_sum_comparison <- ggplot(filtered_data, aes(x = scenario, y = sum, fill = RasterName)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("darkgreen", "blue", "red")) + # Set different colors for each scenario
    labs(x = "Scenario", y = "Sum", fill = "RasterName") +
    ggtitle("Sum Comparison") +
    theme(legend.position = "top")

# Display the plot
plot_sum_comparison

#############################################
# Stats for black spruce

#s2.bs.shrub.sum
#s2.shrub.mask.sum
# Create a list of summary data for each raster
summary_list.bs <- list(
    s2.shrub.mask.sum, s2.bs.shrub.sum
)

# Raster and scenario names
raster_names.bs <- c("s2.shrub.mask.sum", "s2.bs.shrub.sum")

# Create a data frame with raster names and summary statistics
summary_data.bs <- data.frame(RasterName = raster_names.bs)

# Add columns for each statistic
for (stat in c("mean", "sum", "min", "max")) {
    summary_data.bs[stat] <- sapply(summary_list.bs, function(summary) summary[[stat]])
}

# Print the summary data frame
print(summary_data.bs)

# Create separate plots for 'sum' and 'mean'
plot_sum <- ggplot(summary_data.bs, aes(x = RasterName, y = sum, fill = RasterName)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = ifelse(grepl("shrub", summary_data$RasterName), "slateblue4", "seagreen4")) +
    labs(x = "RasterName", y = "Sum", fill = "RasterName") +
    ggtitle("Sum Comparison") +
    theme(legend.position = "top") +
    theme_bw()


plot_mean <- ggplot(summary_data.bs, aes(x = RasterName, y = mean, fill = RasterName)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = ifelse(grepl("shrub", summary_data$RasterName), "slateblue4", "seagreen4")) +
    labs(x = "RasterName", y = "Mean", fill = "RasterName") +
    ggtitle("Mean Comparison") +
    theme(legend.position = "top") +
    theme_bw()

# Stats for fertilized trees..
#############################################
# We want to compare fertilized with unfertilited areas

#s2.boosted.y2.shrub.sum <- global(s2.boosted.y2.shrub.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)
#s2.y2.shrub.sum <- global(s2.y2.shrub.mask, c("mean", "sum", "min", "max"), na.rm=TRUE)


# Create a list of summary data for each raster
summary_list.fert <- list(
    s2.boosted.y2.shrub.sum, s2.y2.shrub.sum
)

# Raster and scenario names
raster_names.fert <- c("s2.boosted.y2.shrub.sum", "s2.y2.shrub.sum")

# Create a data frame with raster names and summary statistics
summary_data.fert <- data.frame(RasterName = raster_names.fert)

# Add columns for each statistic
for (stat in c("mean", "sum", "min", "max")) {
    summary_data.fert[stat] <- sapply(summary_list.fert, function(summary) summary[[stat]])
}

# Print the summary data frame
print(summary_data.fert)

# Create separate plots for 'sum' and 'mean'
plot_sum.fert <- ggplot(summary_data.fert, aes(x = RasterName, y = sum, fill = RasterName)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = ifelse(grepl("shrub", summary_data$RasterName), "slateblue4", "seagreen4")) +
    labs(x = "RasterName", y = "Sum", fill = "RasterName") +
    ggtitle("Sum Comparison") +
    theme(legend.position = "top")

plot_mean.fert <- ggplot(summary_data.fert, aes(x = RasterName, y = mean, fill = RasterName)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = ifelse(grepl("shrub", summary_data$RasterName), "slateblue4", "seagreen4")) +
    labs(x = "RasterName", y = "Mean", fill = "RasterName") +
    ggtitle("Mean Comparison") +
    theme(legend.position = "top")








