
library(GGally)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Loop to create rasters from dataframes
################################# 
# Loop through folders to make our rasters

mask_crop <- rast("D:/BP_Layers/outputs/tree_mask.tif")
boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")

# This is the folder where we saved our 3PG outputs
csv_folder <- "D:/BP_Layers/outputs/crops/RAD_TEST/r_lodge"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = csv_folder, pattern = "*.csv")

#Need to decide which raster we want to produce - keyword is from the outputs of the 3PG function
keyword <- "biom_full"

# where are things saved?
#target_folder <- paste0("D:/BP_Layers/outputs/crops/output_rasters_full_rad/", keyword)
target_folder <- csv_folder

# Loop through each CSV file
for (file in csv_files) {
  
  csv_name <- gsub(".csv", "", file)
  filename <- paste0(keyword,csv_name,".tif") # check ----
  filepath <- file.path(target_folder, filename)
  
  if (file.exists(filepath)) {
    cat("Skipping file", filename, "because it already exists.\n")
    next  # move to next iteration of the loop
  }
  
  # Read in the CSV file
  df <- read.csv(paste(csv_folder, file, sep = "/"))
  
  # Extract the numbers from the file name
  num.box <- gsub("[^0-9]", "", file)
  num.box <- as.numeric(num.box)
  
  # Perform your function using the extracted numbers
  
  # Do something with the output
  # ...
  polygon <- boxes.v[boxes.v$name == num.box, ]
  mini.r <-  crop(mask_crop, polygon)
  
  crs(mini.r) <- crs(mask_crop)
  
  #n.pix = global(mini.r, "notNA") %>% as.numeric()
  
  #r.id = mini.r
  #r.id[!is.na(r.id)] <- 1:n.pix #npp.df
  #plot(r.id)
  
  #dt = values(r.id, na.rm = T) %>% as.data.table()
  
  #r = rast()
  # How many valid pixels?
  #num.valid = global(r.id, "notNA") %>% as.numeric()
  
  # Read in output (NPP or whatever) -> change this to the relevant output
  vals = df %>% pull(keyword)
  
  # This should be true
  #length(vals) == num.valid
  
  # Fill in the blank raster with the output pixels
  mini.r[!is.na(mini.r)] <- vals
  
  # Add the number to the raster file name
  rast_name <- paste0(target_folder,"/",keyword, num.box, ".tif")
  
  # Write the raster to disk
  terra::writeRaster(mini.r, filename = rast_name, overwrite=T)
  rm(mini.r)
}


# If we want to make and group tiles for larger rasters
###################################### 

# combining groups of tiles :
raster_folder <- paste0("D:/BP_Layers/outputs/crops/output_rasters_full_rad/", keyword)
# Get a list of all CSV files in the folder
raster_files <- list.files(path = raster_folder, pattern = "*.tif")
num_files <- length(raster_files)
# terra::mosaic

group <- terra::vrt(paste(raster_folder, raster_files, sep = "/"))

plot(group, col=viridis(100),  main=paste0(keyword))

# be sure to change the number or
writeRaster(group, filename = paste0("D:/BP_Layers/outputs/crops/output_rasters_50yr/49_composite/",keyword, "_",num_files,".tif"))

############################################

# Looking at BIOMASS FIRST:

######################################################################################################################

# Compare Raster Values

new_lp889_b <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/new_lodge/biom_full889.tif")
orig_lp889_b <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/original_lodge/biom_full889.tif")
orig_lp_half889_b <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/original_lodge_half_rad/biom_full889.tif")
r_lp889_b <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/r_lodge/biom_full889.tif")

#test <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/r_lodge/biom_full889.tif")
#test[!mask889] <- NA

new_lp892_b <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/new_lodge/biom_full892.tif")
orig_lp892_b <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/original_lodge/biom_full892.tif")
orig_lp_half892_b <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/original_lodge_half_rad/biom_full892.tif")
r_lp892_b <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/r_lodge/biom_full892.tif")


# Set up the plot window to have three columns
par(mfrow = c(2, 2))

plot(new_lp892_b)
plot(orig_lp892_b)
plot(orig_lp_half892_b)
plot(r_lp892_b)

par(mfrow=c(1,1))

# Species
species <- rast("D:/BP_Layers/outputs/inputs/leading-species_2019_2.tif")

# Crop species to correct boxes
species889 <- crop(species, new_lp889_b)
species892 <- crop(species, new_lp892_b)

# NTEMS BIOMASS
biomass_ntems_full <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/biomass_ntems_full.tif")

# Crop NTEMS to the correct boxes
biomass889 <- crop(biomass_ntems_full, new_lp889_b)
biomass892 <- crop(biomass_ntems_full, new_lp892_b)

# Mask - we want to focus on Lodgepole pine (species #23)
mask889 <- species889 == 23
mask892 <- species892 == 23

# We want to check specific species so mask:

orig_lp889_b[!mask889] <- NA
new_lp889_b[!mask889] <- NA
orig_lp_half889_b[!mask889] <- NA
r_lp889_b[!mask889] <- NA
biomass889[!mask889] <- NA

names(orig_lp889_b) <- "orig_lp"
names(new_lp889_b) <- "new_lp"
names(orig_lp_half889_b) <- "orig_lp_half"
names(r_lp889_b) <- "r_lp"
names(biomass889) <- "NTEMS"

# Other tile

orig_lp892_b[!mask892] <- NA
new_lp892_b[!mask892] <- NA
orig_lp_half892_b[!mask892] <- NA
r_lp892_b[!mask892] <- NA
biomass892[!mask892] <- NA

names(orig_lp892_b) <- "orig_lp"
names(new_lp892_b) <- "new_lp"
names(orig_lp_half892_b) <- "orig_lp_half"
names(r_lp892_b) <- "r_lp"
names(biomass892) <- "NTEMS"

# quick check - we know that the masked rasters should have fewer pixels than the original species mask, unless the cell is 100% 1 species (unlikely)
global(species892, "notNA")
global(biomass892, "notNA")

# Quick stats

global(biomass889, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
global(orig_lp889_b, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
global(r_lp889_b, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)

# LOOK AT SUMMARY STATISTICS OF THE DATA
stacked_889 <- c(biomass889, orig_lp889_b, new_lp889_b, orig_lp_half889_b, r_lp889_b)

plot(stacked_889, col =viridis(100))

df_bio889 <- as.data.frame(stacked_889)

colnames(df_bio889)[1] <- "NTEMS"
colnames(df_bio889)[2] <- "orig_lp"
colnames(df_bio889)[3] <- "new_lp"
colnames(df_bio889)[4] <- "orig_lp_half"
colnames(df_bio889)[5] <- "r_lp"

stacked_892 <- c(biomass892, orig_lp892_b, new_lp892_b, orig_lp_half892_b, r_lp892_b)

plot(stacked_892, col =viridis(100))

df_bio892 <- as.data.frame(stacked_892)

colnames(df_bio892)[1] <- "NTEMS"
colnames(df_bio892)[2] <- "orig_lp"
colnames(df_bio892)[3] <- "new_lp"
colnames(df_bio892)[4] <- "orig_lp_half"
colnames(df_bio892)[5] <- "r_lp"


# Quick summary - NTEMS here
sum <- df_bio892 %>%
  summarize(
    mean = mean(NTEMS),
    min = min(NTEMS),
    max = max(NTEMS),
    sd = sd(NTEMS),
    median = median(NTEMS),
    quartile_25 = quantile(NTEMS, 0.25),
    quartile_75 = quantile(NTEMS, 0.75)
  )

# Summarize the raster 889
summary_df889 <- df_bio889 %>%
  summarize(
    mean = mean(orig_lp),
    min = min(orig_lp),
    max = max(orig_lp),
    sd = sd(orig_lp),
    median = median(orig_lp),
    quartile_25 = quantile(orig_lp, 0.25),
    quartile_75 = quantile(orig_lp, 0.75)
  ) %>%
  bind_rows(df_bio889 %>%
              summarize(
                mean = mean(new_lp),
                min = min(new_lp),
                max = max(new_lp),
                sd = sd(new_lp),
                median = median(new_lp),
                quartile_25 = quantile(new_lp, 0.25),
                quartile_75 = quantile(new_lp, 0.75)
              )) %>%
  bind_rows(df_bio889 %>%
              summarize(
                mean = mean(orig_lp_half),
                min = min(orig_lp_half),
                max = max(orig_lp_half),
                sd = sd(orig_lp_half),
                median = median(orig_lp_half),
                quartile_25 = quantile(orig_lp_half, 0.25),
                quartile_75 = quantile(orig_lp_half, 0.75)
              )) %>%
  bind_rows(df_bio889 %>%
              summarize(
                mean = mean(r_lp),
                min = min(r_lp),
                max = max(r_lp),
                sd = sd(r_lp),
                median = median(r_lp),
                quartile_25 = quantile(r_lp, 0.25),
                quartile_75 = quantile(r_lp, 0.75)
              )) %>%
  bind_rows(df_bio889 %>%
              summarize(
                mean = mean(NTEMS),
                min = min(NTEMS),
                max = max(NTEMS),
                sd = sd(NTEMS),
                median = median(NTEMS),
                quartile_25 = quantile(NTEMS, 0.25),
                quartile_75 = quantile(NTEMS, 0.75)
              ))

# Summarize the raster 892
summary_df892 <- df_bio892 %>%
  summarize(
    mean = mean(orig_lp),
    min = min(orig_lp),
    max = max(orig_lp),
    sd = sd(orig_lp),
    median = median(orig_lp),
    quartile_25 = quantile(orig_lp, 0.25),
    quartile_75 = quantile(orig_lp, 0.75)
  ) %>%
  bind_rows(df_bio892 %>%
              summarize(
                mean = mean(new_lp),
                min = min(new_lp),
                max = max(new_lp),
                sd = sd(new_lp),
                median = median(new_lp),
                quartile_25 = quantile(new_lp, 0.25),
                quartile_75 = quantile(new_lp, 0.75)
              )) %>%
  bind_rows(df_bio892 %>%
              summarize(
                mean = mean(orig_lp_half),
                min = min(orig_lp_half),
                max = max(orig_lp_half),
                sd = sd(orig_lp_half),
                median = median(orig_lp_half),
                quartile_25 = quantile(orig_lp_half, 0.25),
                quartile_75 = quantile(orig_lp_half, 0.75)
              )) %>%
  bind_rows(df_bio892 %>%
              summarize(
                mean = mean(r_lp),
                min = min(r_lp),
                max = max(r_lp),
                sd = sd(r_lp),
                median = median(r_lp),
                quartile_25 = quantile(r_lp, 0.25),
                quartile_75 = quantile(r_lp, 0.75)
              )) %>%
  bind_rows(df_bio892 %>%
              summarize(
                mean = mean(NTEMS),
                min = min(NTEMS),
                max = max(NTEMS),
                sd = sd(NTEMS),
                median = median(NTEMS),
                quartile_25 = quantile(NTEMS, 0.25),
                quartile_75 = quantile(NTEMS, 0.75)
              ))

# Set row names as the column headings
rownames(summary_df889) <- c("orig_lp", "new_lp", "orig_lp_half", "r_lp", "NTEMS")
rownames(summary_df892) <- c("orig_lp", "new_lp", "orig_lp_half", "r_lp", "NTEMS")
# Transpose the summary dataframe
summary_df889_t <- t(summary_df889)
summary_df892_t <- t(summary_df892)


df_long889 <- df_bio889 %>%
  pivot_longer(cols = everything(), names_to = "Column Heading", values_to = "Value")

df_long892 <- df_bio892 %>%
  pivot_longer(cols = everything(), names_to = "Column Heading", values_to = "Value")

# Define a custom color palette
my_palette <- c("#355EAF", "#005F58", "#0FA100", "#FFBB39", "#FF6F00")

# Plot the density using the long format dataframe
ggplot(df_long892, aes(x = Value, color = `Column Heading`)) +
  geom_density(size = 0.75) +  # Adjust line size
  labs(x = "Value", y = "Density") +
  scale_color_manual(values = my_palette, name = "Column Heading") +  # Use custom color palette
  theme_pt() + 
  ylim(0, 0.1) #+ # Set y-axis limit from 0 to 0.1
  #geom_label(aes(label = `Column Heading`), nudge_y = 0.005, color = "black", fontface = "bold", size = 3)


# Plot the density of NTEMS and new_lp on the same plot
ggplot(df_long892 %>% filter(`Column Heading` %in% c("NTEMS", "orig_lp")), aes(x = Value, color = `Column Heading`)) +
  geom_density(size = 1) +
  labs(x = "Value", y = "Density") +
  scale_color_manual(values = c("#355EAF", "#FF6F00"), name = "Column Heading") +
  #theme_minimal()
  theme_pt()


df_bio <- rbind(df_bio889, df_bio892)

# Randomly sample 10,000 points
df_sample <- df_bio %>%
  sample_n(50000)


#

# Create separate scatter plots for each combination of variables
plot_orig_lp <- ggplot(df_sample, aes(x = NTEMS, y = orig_lp)) +
  geom_point() +
  labs(x = "NTEMS", y = "orig_lp") +
  ggtitle("orig_lp vs. NTEMS") +
  theme_minimal()

plot_new_lp <- ggplot(df_sample, aes(x = NTEMS, y = new_lp)) +
  geom_point() +
  labs(x = "NTEMS", y = "new_lp") +
  ggtitle("new_lp vs. NTEMS") +
  theme_minimal()

plot_orig_lp_half <- ggplot(df_sample, aes(x = NTEMS, y = orig_lp_half)) +
  geom_point() +
  labs(x = "NTEMS", y = "orig_lp_half") +
  ggtitle("orig_lp_half vs. NTEMS") +
  theme_minimal()

plot_r_lp <- ggplot(df_sample, aes(x = NTEMS, y = r_lp)) +
  geom_point() +
  labs(x = "NTEMS", y = "r_lp") +
  ggtitle("r_lp vs. NTEMS") +
  theme_minimal()

# Display the scatter plots side by side
grid.arrange(plot_orig_lp, plot_new_lp, plot_orig_lp_half, plot_r_lp, ncol = 2)

# Create scatterplot matrix
scatterplot_matrix <- ggpairs(df_sample, columns = c("orig_lp", "new_lp", "orig_lp_half", "r_lp", "NTEMS"))

# Display the scatterplot matrix
scatterplot_matrix

# Get the range of NTEMS values
ntems_range <- range(df_sample$NTEMS)

# Create separate heat maps for each combination of variables
plot_orig_lp <- ggplot(df_sample, aes(x = NTEMS, y = orig_lp)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "orig_lp") +
  ggtitle("orig_lp vs. NTEMS") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

plot_new_lp <- ggplot(df_sample, aes(x = NTEMS, y = new_lp)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "new_lp") +
  ggtitle("new_lp vs. NTEMS") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

plot_orig_lp_half <- ggplot(df_sample, aes(x = NTEMS, y = orig_lp_half)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "orig_lp_half") +
  ggtitle("orig_lp_half vs. NTEMS") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

plot_r_lp <- ggplot(df_sample, aes(x = NTEMS, y = r_lp)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "r_lp") +
  ggtitle("r_lp vs. NTEMS") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

# Display the heat maps side by side
grid.arrange(plot_orig_lp, plot_new_lp, plot_orig_lp_half, plot_r_lp, ncol = 2)


correlation <- cor(df_bio$orig_lp, df_bio$NTEMS)
cor_matrix <- cor(df_bio)

############
library(scatter)

calc.error(reference = df_bio$NTEMS, estimate = df_bio$orig_lp)

p.scatter <- scatter(x = df_sample$NTEMS, y = df_sample$orig_lp)

p.scatter2 <- scatter(x = df_sample$orig_lp, y = df_sample$NTEMS)

# Display the heat maps side by side
grid.arrange(plot_orig_lp, p.scatter, p.scatter2, ncol = 3)

#############
# Looking at VOLUME SECOND
######################################################################################################################

# Can we do the same thing for volume?
new_lp892_v <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/new_lodge/volume892.tif")
orig_lp892_v <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/original_lodge/volume892.tif")
orig_lp_half892_v <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/original_lodge_half_rad/volume892.tif")
r_lp892_v <- rast("D:/BP_Layers/outputs/crops/RAD_TEST/r_lodge/volume892.tif")


# Set up the plot window to have three columns
par(mfrow = c(2, 2))

plot(new_lp892_v)
plot(orig_lp892_v)
plot(orig_lp_half892_v)
plot(r_lp892_v)

par(mfrow=c(1,1))


# NTEMS BIOMASS ###########################
volume_ntems_full <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/volume_ntems_full.tif")
###########

# Crop NTEMS to the correct boxes
# NOT DONE
vol889 <- crop(volume_ntems_full, new_lp889_v)

vol892 <- crop(volume_ntems_full, new_lp892_v)

# Mask - we want to focus on Lodgepole pine (species #23)
mask889 <- species889 == 23

mask892 <- species892 == 23

# We want to check specific species so mask:

orig_lp889_v[!mask889] <- NA
new_lp889_v[!mask889] <- NA
orig_lp_half889_v[!mask889] <- NA
r_lp889_v[!mask889] <- NA
vol889[!mask889] <- NA

names(orig_lp889_v) <- "orig_lp"
names(new_lp889_v) <- "new_lp"
names(orig_lp_half889_v) <- "orig_lp_half"
names(r_lp889_v) <- "r_lp"
names(vol889) <- "NTEMS"

# Other tile

orig_lp892_v[!mask892] <- NA
new_lp892_v[!mask892] <- NA
orig_lp_half892_v[!mask892] <- NA
r_lp892_v[!mask892] <- NA
vol892[!mask892] <- NA

names(orig_lp892_v) <- "orig_lp"
names(new_lp892_v) <- "new_lp"
names(orig_lp_half892_v) <- "orig_lp_half"
names(r_lp892_v) <- "r_lp"
names(vol892) <- "NTEMS"

# quick check - we know that the masked rasters should have fewer pixels than the original species mask, unless the cell is 100% 1 species (unlikely)
global(species892, "notNA")
global(vol892, "notNA")

# Quick stats

global(biomass889, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
global(orig_lp889_b, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
global(r_lp889_b, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)

# LOOK AT SUMMARY STATISTICS OF THE DATA
stacked_889 <- c(biomass889, orig_lp889_b, new_lp889_b, orig_lp_half889_b, r_lp889_b)

plot(stacked_889, col =viridis(100))

df_bio889 <- as.data.frame(stacked_889)

colnames(df_bio889)[1] <- "NTEMS"
colnames(df_bio889)[2] <- "orig_lp"
colnames(df_bio889)[3] <- "new_lp"
colnames(df_bio889)[4] <- "orig_lp_half"
colnames(df_bio889)[5] <- "r_lp"

stacked_892_v <- c(vol892, orig_lp892_v, new_lp892_v, orig_lp_half892_v, r_lp892_v)

plot(stacked_892_v, col =viridis(100))

df_vol892 <- as.data.frame(stacked_892_v)

colnames(df_vol892)[1] <- "NTEMS"
colnames(df_vol892)[2] <- "orig_lp"
colnames(df_vol892)[3] <- "new_lp"
colnames(df_vol892)[4] <- "orig_lp_half"
colnames(df_vol892)[5] <- "r_lp"


# Quick summary - NTEMS here
sum <- df_bio892 %>%
  summarize(
    mean = mean(NTEMS),
    min = min(NTEMS),
    max = max(NTEMS),
    sd = sd(NTEMS),
    median = median(NTEMS),
    quartile_25 = quantile(NTEMS, 0.25),
    quartile_75 = quantile(NTEMS, 0.75)
  )

# Summarize the raster 889
summary_df889 <- df_bio889 %>%
  summarize(
    mean = mean(orig_lp),
    min = min(orig_lp),
    max = max(orig_lp),
    sd = sd(orig_lp),
    median = median(orig_lp),
    quartile_25 = quantile(orig_lp, 0.25),
    quartile_75 = quantile(orig_lp, 0.75)
  ) %>%
  bind_rows(df_bio889 %>%
              summarize(
                mean = mean(new_lp),
                min = min(new_lp),
                max = max(new_lp),
                sd = sd(new_lp),
                median = median(new_lp),
                quartile_25 = quantile(new_lp, 0.25),
                quartile_75 = quantile(new_lp, 0.75)
              )) %>%
  bind_rows(df_bio889 %>%
              summarize(
                mean = mean(orig_lp_half),
                min = min(orig_lp_half),
                max = max(orig_lp_half),
                sd = sd(orig_lp_half),
                median = median(orig_lp_half),
                quartile_25 = quantile(orig_lp_half, 0.25),
                quartile_75 = quantile(orig_lp_half, 0.75)
              )) %>%
  bind_rows(df_bio889 %>%
              summarize(
                mean = mean(r_lp),
                min = min(r_lp),
                max = max(r_lp),
                sd = sd(r_lp),
                median = median(r_lp),
                quartile_25 = quantile(r_lp, 0.25),
                quartile_75 = quantile(r_lp, 0.75)
              )) %>%
  bind_rows(df_bio889 %>%
              summarize(
                mean = mean(NTEMS),
                min = min(NTEMS),
                max = max(NTEMS),
                sd = sd(NTEMS),
                median = median(NTEMS),
                quartile_25 = quantile(NTEMS, 0.25),
                quartile_75 = quantile(NTEMS, 0.75)
              ))

# Summarize the raster 892
summary_df892 <- df_bio892 %>%
  summarize(
    mean = mean(orig_lp),
    min = min(orig_lp),
    max = max(orig_lp),
    sd = sd(orig_lp),
    median = median(orig_lp),
    quartile_25 = quantile(orig_lp, 0.25),
    quartile_75 = quantile(orig_lp, 0.75)
  ) %>%
  bind_rows(df_bio892 %>%
              summarize(
                mean = mean(new_lp),
                min = min(new_lp),
                max = max(new_lp),
                sd = sd(new_lp),
                median = median(new_lp),
                quartile_25 = quantile(new_lp, 0.25),
                quartile_75 = quantile(new_lp, 0.75)
              )) %>%
  bind_rows(df_bio892 %>%
              summarize(
                mean = mean(orig_lp_half),
                min = min(orig_lp_half),
                max = max(orig_lp_half),
                sd = sd(orig_lp_half),
                median = median(orig_lp_half),
                quartile_25 = quantile(orig_lp_half, 0.25),
                quartile_75 = quantile(orig_lp_half, 0.75)
              )) %>%
  bind_rows(df_bio892 %>%
              summarize(
                mean = mean(r_lp),
                min = min(r_lp),
                max = max(r_lp),
                sd = sd(r_lp),
                median = median(r_lp),
                quartile_25 = quantile(r_lp, 0.25),
                quartile_75 = quantile(r_lp, 0.75)
              )) %>%
  bind_rows(df_bio892 %>%
              summarize(
                mean = mean(NTEMS),
                min = min(NTEMS),
                max = max(NTEMS),
                sd = sd(NTEMS),
                median = median(NTEMS),
                quartile_25 = quantile(NTEMS, 0.25),
                quartile_75 = quantile(NTEMS, 0.75)
              ))

# Set row names as the column headings
rownames(summary_df889) <- c("orig_lp", "new_lp", "orig_lp_half", "r_lp", "NTEMS")
rownames(summary_df892) <- c("orig_lp", "new_lp", "orig_lp_half", "r_lp", "NTEMS")
# Transpose the summary dataframe
summary_df889_t <- t(summary_df889)
summary_df892_t <- t(summary_df892)


df_long889 <- df_bio889 %>%
  pivot_longer(cols = everything(), names_to = "Column Heading", values_to = "Value")

df_long892 <- df_bio892 %>%
  pivot_longer(cols = everything(), names_to = "Column Heading", values_to = "Value")

# Define a custom color palette
my_palette <- c("#355EAF", "#005F58", "#0FA100", "#FFBB39", "#FF6F00")

# Plot the density using the long format dataframe
ggplot(df_long892, aes(x = Value, color = `Column Heading`)) +
  geom_density(size = 0.75) +  # Adjust line size
  labs(x = "Value", y = "Density") +
  scale_color_manual(values = my_palette, name = "Column Heading") +  # Use custom color palette
  theme_pt() + 
  ylim(0, 0.1) #+ # Set y-axis limit from 0 to 0.1
#geom_label(aes(label = `Column Heading`), nudge_y = 0.005, color = "black", fontface = "bold", size = 3)


# Plot the density of NTEMS and new_lp on the same plot
ggplot(df_long892 %>% filter(`Column Heading` %in% c("NTEMS", "orig_lp")), aes(x = Value, color = `Column Heading`)) +
  geom_density(size = 1) +
  labs(x = "Value", y = "Density") +
  scale_color_manual(values = c("#355EAF", "#FF6F00"), name = "Column Heading") +
  #theme_minimal()
  theme_pt()


df_bio <- rbind(df_bio889, df_bio892)

# Randomly sample 10,000 points
df_sample <- df_bio %>%
  sample_n(50000)


#

# Create separate scatter plots for each combination of variables
plot_orig_lp <- ggplot(df_sample, aes(x = NTEMS, y = orig_lp)) +
  geom_point() +
  labs(x = "NTEMS", y = "orig_lp") +
  ggtitle("orig_lp vs. NTEMS") +
  theme_minimal()

plot_new_lp <- ggplot(df_sample, aes(x = NTEMS, y = new_lp)) +
  geom_point() +
  labs(x = "NTEMS", y = "new_lp") +
  ggtitle("new_lp vs. NTEMS") +
  theme_minimal()

plot_orig_lp_half <- ggplot(df_sample, aes(x = NTEMS, y = orig_lp_half)) +
  geom_point() +
  labs(x = "NTEMS", y = "orig_lp_half") +
  ggtitle("orig_lp_half vs. NTEMS") +
  theme_minimal()

plot_r_lp <- ggplot(df_sample, aes(x = NTEMS, y = r_lp)) +
  geom_point() +
  labs(x = "NTEMS", y = "r_lp") +
  ggtitle("r_lp vs. NTEMS") +
  theme_minimal()

# Display the scatter plots side by side
grid.arrange(plot_orig_lp, plot_new_lp, plot_orig_lp_half, plot_r_lp, ncol = 2)

# Create scatterplot matrix
scatterplot_matrix <- ggpairs(df_sample, columns = c("orig_lp", "new_lp", "orig_lp_half", "r_lp", "NTEMS"))

# Display the scatterplot matrix
scatterplot_matrix

# Get the range of NTEMS values
ntems_range <- range(df_sample$NTEMS)

# Create separate heat maps for each combination of variables
plot_orig_lp <- ggplot(df_sample, aes(x = NTEMS, y = orig_lp)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "orig_lp") +
  ggtitle("orig_lp vs. NTEMS") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

plot_new_lp <- ggplot(df_sample, aes(x = NTEMS, y = new_lp)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "new_lp") +
  ggtitle("new_lp vs. NTEMS") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

plot_orig_lp_half <- ggplot(df_sample, aes(x = NTEMS, y = orig_lp_half)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "orig_lp_half") +
  ggtitle("orig_lp_half vs. NTEMS") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

plot_r_lp <- ggplot(df_sample, aes(x = NTEMS, y = r_lp)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "r_lp") +
  ggtitle("r_lp vs. NTEMS") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

# Display the heat maps side by side
grid.arrange(plot_orig_lp, plot_new_lp, plot_orig_lp_half, plot_r_lp, ncol = 2)


