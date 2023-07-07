library(terra)

# Load the template raster
template_raster <- rast("D:/BP_Layers/outputs/crops/889_test/100/f_sw_jul889.tif")

# Set the random seed for reproducibility
set.seed(123)

# Create the first raster with random values
max_asw <- template_raster
max_asw[] <- sample(c(100, 150, 200, 250, 300), size = ncell(max_asw), replace = TRUE)

# Create the second raster with random values
random_vals <- sample(c(0.4, 0.6, 0.8, 1), size = ncell(template_raster), replace = TRUE)
fert <- template_raster
fert[] <- random_vals

# Plot the rasters side by side
par(mfrow = c(1, 2))
plot(max_asw, main = "max_asw")
plot(fert, main = "fert_raster")

# Create a dataframe to store the values
inputs3 <- data.frame(max_asw = as.vector(max_asw), fert = as.vector(fert))


# Create a vector of all unique combinations of values
value_combos <- expand.grid(max_asw = c(100, 150, 200, 250, 300), fert = c(0.4, 0.6, 0.8, 1))

# Initialize an empty dataframe to store the counts
counts_df <- data.frame(value_combos, count = 0)

# Loop through each row in inputs3 and update the counts
for (i in 1:nrow(inputs3)) {
  # Get the values from the current row
  max_asw_val <- inputs3$max_asw[i]
  fert_val <- inputs3$fert[i]
  
  # Find the corresponding row in counts_df and increment the count
  counts_df[counts_df$max_asw == max_asw_val & counts_df$fert == fert_val, "count"] <- counts_df[counts_df$max_asw == max_asw_val & counts_df$fert == fert_val, "count"] + 1
}

# Print the counts dataframe
print(counts_df)

max(counts_df$count)


write.csv(inputs3, file = "D:/BP_Layers/outputs/crops/889_test/varied/inputs3.csv", row.names = FALSE)




##################################################

# Load the Biomass and fert rasters
biomass_varied <- rast("D:/BP_Layers/outputs/crops/889_test/varied/biom_full889.tif")

biomass_varied[!mask889] <- NA
max_asw[!mask889] <- NA
fert[!mask889] <- NA

# this is where alpha is varied depending on fertilisation 
biom_alpha <- rast("D:/BP_Layers/outputs/crops/889_test/varied/alpha/both_vary/biom_full889.tif")
biom_alpha[!mask889] <- NA

global(biom_alpha, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)

biom_alpha_150 <- rast("D:/BP_Layers/outputs/crops/889_test/varied/alpha/asw_150/biom_full889.tif")
global(biom_alpha_150, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)

biom_alpha_150[!mask889] <- NA

# Plot the rasters side by side
par(mfrow = c(1, 3))
plot(max_asw, main = "max_asw")
plot(fert, main = "fert_raster")
plot(biomass_varied, main = "3 PG Biomass", col = viridis(100))


# Create a dataframe with max_asw, Biomass, and fert values

stacked_889_varied <- c(biomass_varied, max_asw, fert, biomass889,biom_alpha, biom_alpha_150)

plot(stacked_889_varied, col =viridis(100))

df_889_varied <- as.data.frame(stacked_889_varied)

colnames(df_889_varied)[1] <- "biomass_3pg"
colnames(df_889_varied)[2] <- "max_asw"
colnames(df_889_varied)[3] <- "fert"
colnames(df_889_varied)[4] <- "biomass_ntems"
colnames(df_889_varied)[5] <- "biomass_alpha"
colnames(df_889_varied)[6] <- "biom_alpha_150"

# Create violin plots of Biomass values for each max_asw value
violin_max_asw <- ggplot(df_889_varied, aes(factor(max_asw), biomass_3pg)) +
  geom_violin() +
  geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +
  xlab("max_asw") +
  ylab("3PG Biomass") +
  ggtitle("Comparison of max_asw and Biomass") +
  theme_bw()

# Save the violin plot to a file
#ggsave("violin_plot.png", plot = violin_plot)

# Create violin plots of Biomass values for each max_asw value
violin_fert <- ggplot(df_889_varied, aes(factor(fert), biomass_3pg)) +
  geom_violin() +
  geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +
  xlab("fert") +
  ylab("3PG Biomass") +
  ggtitle("Comparison of max_asw and Biomass") +
  theme_bw()

library(gridExtra)
grid.arrange(violin_max_asw, violin_fert, ncol=2)


# Convert max_asw and fert to factors for better visual representation
df_889_varied$max_asw <- factor(df_889_varied$max_asw)
df_889_varied$fert <- factor(df_889_varied$fert)

# Calculate the mean biomass for each combination of fert and max_asw

mean_biomass <- aggregate(biomass_3pg ~ fert + max_asw, data = df_889_varied, FUN = mean)

####

####

# Calculate summary statistics for each combination of fert and max_asw
summary_stats <- aggregate(biomass_3pg ~ fert + max_asw, data = df_889_varied, FUN = function(x) {
  c(mean = mean(x),
    sd = sd(x),
    min = min(x),
    max = max(x))
})

# Convert aggregated data to a dataframe
summary_df <- do.call(data.frame, summary_stats)

# Rename the column names
colnames(summary_df) <- c("fert", "max_asw", "mean", "sd", "min", "max")


scatter_plot <- ggplot(summary_df, aes(x = max_asw, y = fert)) +
  geom_tile(aes(fill = mean)) +
  geom_text(aes(label = paste0("Mean: ", round(mean, 2))), size = 4, hjust = 0.5, vjust = -0.5) +
  geom_text(aes(label = paste0("SD: ", round(sd, 2))), size = 4, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(label = paste0("Min: ", round(min, 2))), size = 4, hjust = 0.5, vjust = 1.5) +
  geom_text(aes(label = paste0("Max: ", round(max, 2))), size = 4, hjust = 0.5, vjust = 2.5) +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("max_asw") +
  ylab("fert") +
  ggtitle("Mean Biomass for max_asw and fert") +
  theme_bw()


print(scatter_plot)





# Create a scatter plot with mean biomass values
scatter_plot <- ggplot(mean_biomass, aes(x = max_asw, y = fert, fill = biomass_3pg)) +
  geom_point(size = 5, shape = 21) +
  geom_text(aes(label = round(biomass_3pg, 2)), size = 3, nudge_x = 0.2, nudge_y = 0.2) +
  xlab("max_asw") +
  ylab("fert") +
  ggtitle("Mean Biomass for max_asw and fert") +
  theme_bw()

# Display the grid plot
print(grid_plot)


# Extract the values for a specific combination of max_asw and fert
max_asw_value <- 100
fert_value <- 0.4
subset_df <- df[df$max_asw == max_asw_value & df$fert == fert_value, ]




library(dplyr)
library(psych)
library(moments)

# Group the data by max_asw and fert and calculate the summary statistics
summary_stats_test <- df_889_varied %>%
  group_by(max_asw, fert) %>%
  summarize(
    mean_biomass_3pg = mean(biomass_3pg),
    max_biomass_3pg = max(biomass_3pg),
    min_biomass_3pg = min(biomass_3pg),
    sd_biomass_3pg = sd(biomass_3pg),
    skew_biomass_3pg = skew(biomass_3pg),
    kurtosis_biomass_3pg = kurtosis(biomass_3pg)
  )


# Summarize the biomass_3pg values by max_asw and fert combinations
summary_stats_test <- df_889_varied %>%
  group_by(max_asw, fert) %>%
  summarise(
    mean_biomass_3pg = mean(biomass_3pg, na.rm = TRUE),
    sd_biomass_3pg = sd(biomass_3pg, na.rm = TRUE),
    min_biomass_3pg = min(biomass_3pg, na.rm = TRUE),
    max_biomass_3pg = max(biomass_3pg, na.rm = TRUE),
    kurtosis_biomass_3pg = kurtosis(biomass_3pg, na.rm = TRUE),
    skewness_biomass_3pg = skewness(biomass_3pg, na.rm = TRUE)
    
  ) %>%
  ungroup()

# Summary of NTEMS
summary_table_ntems <- df_889_varied %>%
  summarise(
    mean_biomass_ntems = mean(biomass_ntems, na.rm = TRUE),
    sd_biomass_ntems = sd(biomass_ntems, na.rm = TRUE),
    min_biomass_ntems = min(biomass_ntems, na.rm = TRUE),
    max_biomass_ntems = max(biomass_ntems, na.rm = TRUE),
    kurtosis_biomass_ntems = kurtosis(biomass_ntems, na.rm = TRUE),
    skewness_biomass_ntems = skewness(biomass_ntems, na.rm = TRUE)
  ) %>% as.data.frame()


# Summarize the biomass 3pg with VARIED alpha values by max_asw and fert combinations:
summary_stats_varied_alpha <- df_889_varied %>%
  group_by(max_asw, fert) %>%
  summarise(
    mean_biomass_3pg = mean(biomass_alpha, na.rm = TRUE),
    sd_biomass_3pg = sd(biomass_alpha, na.rm = TRUE),
    min_biomass_3pg = min(biomass_alpha, na.rm = TRUE),
    max_biomass_3pg = max(biomass_alpha, na.rm = TRUE),
    kurtosis_biomass_3pg = kurtosis(biomass_alpha, na.rm = TRUE),
    skewness_biomass_3pg = skewness(biomass_alpha, na.rm = TRUE)
    ) %>%
  ungroup() %>% as.data.frame()


# Summarize the biomass 3pg with VARIED alpha values fert and STABLE max_asw combinations:
summary_stats_varied_alpha_150 <- df_889_varied %>%
  group_by(fert) %>%
  summarise(
    mean_biomass_3pg = mean(biom_alpha_150, na.rm = TRUE),
    sd_biomass_3pg = sd(biom_alpha_150, na.rm = TRUE),
    min_biomass_3pg = min(biom_alpha_150, na.rm = TRUE),
    max_biomass_3pg = max(biom_alpha_150, na.rm = TRUE),
    kurtosis_biomass_3pg = kurtosis(biom_alpha_150, na.rm = TRUE),
    skewness_biomass_3pg = skewness(biom_alpha_150, na.rm = TRUE)
  ) %>%
  ungroup() %>% as.data.frame()

# Summary of the varied alpha with varying fertility and water
summary_table_test <- df_889_varied %>%
  summarise(
    mean_biomass_ntems = mean(biomass_alpha, na.rm = TRUE),
    sd_biomass_ntems = sd(biomass_alpha, na.rm = TRUE),
    min_biomass_ntems = min(biomass_alpha, na.rm = TRUE),
    max_biomass_ntems = max(biomass_alpha, na.rm = TRUE),
    kurtosis_biomass_ntems = kurtosis(biomass_alpha, na.rm = TRUE),
    skewness_biomass_ntems = skewness(biomass_alpha, na.rm = TRUE)
  ) %>% as.data.frame()





# Group the data by fert and asw_max and calculate the test statistics

ks_results <- df_889_varied %>%
  group_by(fert, max_asw) %>%
  summarize(ks_stat = ks.test(biomass_3pg, biomass_ntems)$statistic,
            p_value = ks.test(biomass_3pg, biomass_ntems)$p.value)

# Find the combination with the smallest test statistic
closest_combination <- ks_results %>%
  filter(ks_stat == min(ks_stat))

# Print the closest combination
print(closest_combination)



# Plot density distributions for biomass_3pg and biomass_ntems with viridis color palette
density_plot <- ggplot(df_889_varied, aes(x = biomass_3pg)) +
  geom_density(aes(color = interaction(max_asw, fert))) +
  geom_density(aes(x = biomass_ntems), color = "black", fill = "lightgray", alpha = 0.5, linetype = "dotted", size = 1) +
  xlab("Biomass") +
  ylab("Density") +
  ggtitle("Density Distributions of Biomass") +
  scale_color_manual(values = viridis(nlevels(interaction(df_889_varied$max_asw, df_889_varied$fert)))) +
  theme_bw()


# Display the density plot
print(density_plot)



# Plot density distributions for biomass_3pg and biomass_ntems with viridis color palette
density_plot2 <- ggplot(df_889_varied, aes(x = biomass_alpha)) +
  geom_density(aes(color = interaction(max_asw, fert))) +
  geom_density(aes(x = biomass_ntems), color = "black", fill = "lightgray", alpha = 0.5, linetype = "dotted", size = 1) +
  xlab("Biomass") +
  ylab("Density") +
  ggtitle("Density Distributions of Biomass") +
  scale_color_manual(values = viridis(nlevels(interaction(df_889_varied$max_asw, df_889_varied$fert)))) +
  theme_bw()


# Display the density plot
print(density_plot2)



# Plot density distributions for biomass_3pg and biomass_ntems with viridis color palette
density_plot3 <- ggplot(df_889_varied, aes(x = biom_alpha_150)) +
  geom_density(aes(color = interaction(fert))) +
  geom_density(aes(x = biomass_ntems), color = "black", fill = "lightgray", alpha = 0.5, linetype = "dotted", size = 1) +
  xlab("Biomass") +
  ylab("Density") +
  ggtitle("Density Distributions of Biomass") +
  scale_color_manual(values = viridis(nlevels(interaction(df_889_varied$max_asw)))) +
  theme_bw()


# Display the density plot
print(density_plot3)



# Plot density distributions for biomass_3pg and biomass_ntems with viridis color palette
density_plot4 <- ggplot(df_889_varied) +
  geom_density(aes(x = biom_alpha_150), color = "red", fill = "lightgray", alpha = 0.3, size = 1) +
  geom_density(aes(x = biomass_alpha), color = "blue", fill = "lightgray", alpha = 0.3, size = 1) +
  geom_density(aes(x = biomass_ntems), color = "black", fill = "lightgray", alpha = 0.3, size = 1) +
  geom_density(aes(x = biomass_3pg), color = "darkgreen", fill = "lightgray", alpha = 0.3, size = 1) +
  xlab("Biomass") +
  ylab("Density") +
  ggtitle("Density Distributions of Biomass") +
  theme_bw()


# Display the density plot
print(density_plot3)

















biom150_07_889 <- rast("D:/BP_Layers/outputs/crops/889_test/150/biom_full889.tif")

biom150_07_889[!mask889] <- NA

names(biom150_07_889) <- "3PG Biomass"

df_biom150_07_889 <- as.data.frame(biom150_07_889)
colnames(df_biom150_07_889)[1] <- "BIOMASS_3PG"

par(mfrow = c(1, 2))
plot(biomass889, col = viridis(100), main = "NTEMS Biomass")
plot(biom150_07_889, col = viridis(100), main = "3PG Biomass")


df_biomass_889 <- as.data.frame(biomass889)



summary_table_3pg <- df_biom150_07_889 %>%
  summarise(
    mean_biomass_ntems = mean(BIOMASS_3PG, na.rm = TRUE),
    sd_biomass_ntems = sd(BIOMASS_3PG, na.rm = TRUE),
    min_biomass_ntems = min(BIOMASS_3PG, na.rm = TRUE),
    max_biomass_ntems = max(BIOMASS_3PG, na.rm = TRUE),
    kurtosis_biomass_ntems = kurtosis(BIOMASS_3PG, na.rm = TRUE),
    skewness_biomass_ntems = skewness(BIOMASS_3PG, na.rm = TRUE)
  )



