
library(GGally)
library(ggplot2)
library(dplyr)
library(gridExtra)

df889 <- read.csv("D:/BP_Layers/outputs/crops/889_test/varied/889.csv")


# Loop through folders to make our rasters for display

mask_crop <- rast("D:/BP_Layers/outputs/tree_mask.tif")
boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")

# This is the folder where we saved our 3PG outputs
csv_folder <- "D:/BP_Layers/outputs/crops/889_test/varied/alpha/asw_150"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = csv_folder, pattern = "*.csv")


# where are things saved?
#target_folder <- paste0("D:/BP_Layers/outputs/crops/output_rasters_full_rad/", keyword)
target_folder <- csv_folder


#Need to decide which raster we want to produce - keyword is from the outputs of the 3PG function
#keyword <- "fsw_jul"

# List of keywords
keywords <- c("f_sw_jul", "f_sw_aug", "f_sw_sep", "f_sw_oct", "f_nutr", "biom_full", "dbh", "basal_area", "npp", "volume")

# Loop through each keyword
for (keyword in keywords) {

# Loop through each CSV file
for (i in seq_along(csv_files)) {

  file <- csv_files[i]
  
  # Skip all files except the second file - if we want to do all the files don't change this!
  if (i != 1) {
    next  # move to next iteration of the loop
  }
  
  
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
}
#####################################


# Looking at ASW_MAX:

######################################################################################################################

# Compare Raster Values

fsw_100_jul <- rast("D:/BP_Layers/outputs/crops/889_test/100/f_sw_jul889.tif")
fsw_150_jul <- rast("D:/BP_Layers/outputs/crops/889_test/150/f_sw_jul889.tif")
fsw_300_jul <- rast("D:/BP_Layers/outputs/crops/889_test/300/f_sw_jul889.tif")

fsw_100_aug <- rast("D:/BP_Layers/outputs/crops/889_test/100/f_sw_aug889.tif")
fsw_150_aug <- rast("D:/BP_Layers/outputs/crops/889_test/150/f_sw_aug889.tif")
fsw_300_aug <- rast("D:/BP_Layers/outputs/crops/889_test/300/f_sw_aug889.tif")

fsw_100_sep <- rast("D:/BP_Layers/outputs/crops/889_test/100/f_sw_sep889.tif")
fsw_150_sep <- rast("D:/BP_Layers/outputs/crops/889_test/150/f_sw_sep889.tif")
fsw_300_sep <- rast("D:/BP_Layers/outputs/crops/889_test/300/f_sw_sep889.tif")

fsw_100_oct <- rast("D:/BP_Layers/outputs/crops/889_test/100/f_sw_oct889.tif")
fsw_150_oct <- rast("D:/BP_Layers/outputs/crops/889_test/150/f_sw_oct889.tif")
fsw_300_oct <- rast("D:/BP_Layers/outputs/crops/889_test/300/f_sw_oct889.tif")

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
biomass889[!mask889] <- NA

fsw_100_jul[!mask889] <- NA
fsw_150_jul[!mask889] <- NA
fsw_300_jul[!mask889] <- NA

fsw_100_aug[!mask889] <- NA
fsw_150_aug[!mask889] <- NA
fsw_300_aug[!mask889] <- NA

fsw_100_sep[!mask889] <- NA
fsw_150_sep[!mask889] <- NA
fsw_300_sep[!mask889] <- NA

fsw_100_oct[!mask889] <- NA
fsw_150_oct[!mask889] <- NA
fsw_300_oct[!mask889] <- NA


# quick check - we know that the masked rasters should have fewer pixels than the original species mask, unless the cell is 100% 1 species (unlikely)
global(species892, "notNA")
global(biomass892, "notNA")

# Quick stats

global(biomass889, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
global(fsw_150_aug, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)


# LOOK AT SUMMARY STATISTICS OF THE DATA
stacked_fsw <- c(fsw_100_jul, fsw_150_jul, fsw_300_jul, fsw_100_aug,fsw_150_aug, fsw_300_aug,fsw_100_sep,fsw_150_sep,fsw_300_sep,
                 fsw_100_oct,fsw_150_oct, fsw_300_oct)

raster_names <- c("fsw_100_jul", "fsw_150_jul", "fsw_300_jul", "fsw_100_aug", "fsw_150_aug", "fsw_300_aug", "fsw_100_sep", "fsw_150_sep",
                  "fsw_300_sep", "fsw_100_oct", "fsw_150_oct", "fsw_300_oct")

# Change the names of the values in the raster stack
names(stacked_fsw) <- raster_names

# Set up the plot window to have three columns
par(mfrow = c(4, 3))

#July
plot(stacked_fsw[[1]], main = names(stacked_fsw)[1], col =viridis(100))
plot(stacked_fsw[[2]], main = names(stacked_fsw)[2], col =viridis(100))
plot(stacked_fsw[[3]], main = names(stacked_fsw)[3], col =viridis(100))
#August
plot(stacked_fsw[[4]], main = names(stacked_fsw)[4], col =viridis(100))
plot(stacked_fsw[[5]], main = names(stacked_fsw)[5], col =viridis(100))
plot(stacked_fsw[[6]], main = names(stacked_fsw)[6], col =viridis(100))
#September
plot(stacked_fsw[[7]], main = names(stacked_fsw)[7], col =viridis(100))
plot(stacked_fsw[[8]], main = names(stacked_fsw)[8], col =viridis(100))
plot(stacked_fsw[[9]], main = names(stacked_fsw)[9], col =viridis(100))
#October
plot(stacked_fsw[[10]], main = names(stacked_fsw)[10], col =viridis(100))
plot(stacked_fsw[[11]], main = names(stacked_fsw)[11], col =viridis(100))
plot(stacked_fsw[[12]], main = names(stacked_fsw)[12], col =viridis(100))

par(mfrow=c(1,1))

######################################################################
df_fsw889 <- as.data.frame(stacked_fsw)

# Summarize the raster 889
summary_df889 <- df_fsw889 %>%
  summarize(
    mean = mean(fsw_100_sep),
    min = min(fsw_100_sep),
    max = max(fsw_100_sep),
    sd = sd(fsw_100_sep),
    median = median(fsw_100_sep),
    quartile_25 = quantile(fsw_100_sep, 0.25),
    quartile_75 = quantile(fsw_100_sep, 0.75)
  ) %>%
  bind_rows(df_fsw889 %>%
              summarize(
                mean = mean(fsw_150_sep),
                min = min(fsw_150_sep),
                max = max(fsw_150_sep),
                sd = sd(fsw_150_sep),
                median = median(fsw_150_sep),
                quartile_25 = quantile(fsw_150_sep, 0.25),
                quartile_75 = quantile(fsw_150_sep, 0.75)
              )) %>%
  bind_rows(df_fsw889 %>%
              summarize(
                mean = mean(fsw_300_sep),
                min = min(fsw_300_sep),
                max = max(fsw_300_sep),
                sd = sd(fsw_300_sep),
                median = median(fsw_300_sep),
                quartile_25 = quantile(fsw_300_sep, 0.25),
                quartile_75 = quantile(fsw_300_sep, 0.75)
              ))
 


# Set row names as the column headings
rownames(summary_df889) <- c("fsw_100_sep", "fsw_150_sep", "fsw_300_sep")

# Transpose the summary dataframe
summary_df889_t <- t(summary_df889)

summary_df889_table <- summary_df889_t

df_long889 <- df_fsw889 %>%
  pivot_longer(cols = everything(), names_to = "Column Heading", values_to = "Value")
###########################




# Define a custom color palette
my_palette <- c("#355EAF", "#005F58", "#0FA100", "#FFBB39", "#FF6F00")

# Plot the density using the long format dataframe

# Define custom legend labels
my_labels <- c("New Param", "NTEMS", "Original Double Rad", "Original Half Rad", "Meyer et al. 2013")

# Plot the density using the long format dataframe
ggplot(df_long892, aes(x = Value, color = `Column Heading`)) +
  geom_density(size = 0.75) +
  labs(x = "Value", y = "Density") +
  scale_color_manual(values = my_palette, name = "Column Heading", labels = my_labels) +
  theme_pt() +
  ylim(0, 0.1)


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
  ggtitle("NTEMS vs Original Double Rad") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

plot_new_lp <- ggplot(df_sample, aes(x = NTEMS, y = new_lp)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "new_lp") +
  ggtitle("NTEMS vs New Param") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

plot_orig_lp_half <- ggplot(df_sample, aes(x = NTEMS, y = orig_lp_half)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "orig_lp_half") +
  ggtitle("NTEMS vs Original Half Rad") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

plot_r_lp <- ggplot(df_sample, aes(x = NTEMS, y = r_lp)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "r_lp") +
  ggtitle("NTEMS vs. Meyer et al. 2013") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)

# Display the heat maps side by side
grid.arrange(plot_orig_lp_half, plot_orig_lp, plot_new_lp, plot_r_lp, ncol = 2)


correlation <- cor(df_bio$orig_lp, df_bio$NTEMS)
cor_matrix <- cor(df_bio)

############
library(scatter)

calc.error(reference = df_bio$NTEMS, estimate = df_bio$orig_lp)

p.scatter <- scatter(x = df_sample$NTEMS, y = df_sample$orig_lp)

p.scatter2 <- scatter(x = df_sample$orig_lp, y = df_sample$NTEMS)

# Display the heat maps side by side
grid.arrange(plot_orig_lp, p.scatter, p.scatter2, ncol = 3)