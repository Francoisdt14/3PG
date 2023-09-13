library(terra)
library(GGally)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
# Looking at BIOMASS FIRST:

######################################################################################################################
# M 9 S -

y.rast <- rast("D:/BP_Layers/M_9S/3PG_flt/1_other_inputs/latitude_30m.tif")


# Read in Biomass Rasters for WF (foliage) and WS (Stem) that need to be combined..
# Lodgepole Pine
ws_lp <- rast("D:/3PG_Cplusplus/output_M_9S/ws.flt")
wf_lp <- rast("D:/3PG_Cplusplus/output_M_9S/wf.flt")


# Black Spruce - in M 9 S
#ws_bs <- rast("D:/3PG_Cplusplus/_delete/ws_bs.flt")
#wf_bs <- rast("D:/3PG_Cplusplus/_delete/wf_bs.flt")

# Future Lodgepole Pine
ws_lp_245 <- rast("D:/3PG_Cplusplus/_delete/ws_lp_245.flt")
wf_lp_245 <- rast("D:/3PG_Cplusplus/_delete/wf_lp_245.flt")

# Combine the rasters to get total AGB

lp_agb <- ws_lp + wf_lp

mask <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/Forest_Age_2019_withNA.flt")
crs(lp_agb) <- crs(mask)
compareGeom(lp_agb,mask)

#bs_agb <- ws_bs + wf_bs
lp_245_agb <- ws_lp_245 + wf_lp_245

future.diff <- lp_245_agb-lp_agb
#writeRaster(future.diff, "D:/3PG_Cplusplus/_delete/lp_biomass_future_DIFF_full.tif")


# Read in the forest age mask so that we can mask out the treed areas for current comparisons
tree.mask <- rast("D:/BP_Layers/outputs/tree_mask.tif")
tree.mask.90 <-  terra::aggregate(tree.mask, 100/res(tree.mask)[1])

# Mask out the non-treed areas

lp_agb.mask <- terra::mask(lp_agb, mask)
bs_agb.mask <- terra::mask(bs_agb, tree.mask.90)
lp_245_agb.mask <- terra::mask(lp_245_agb, tree.mask.90)

future.diff.mask <- lp_245_agb.mask - lp_agb.mask

#writeRaster(future.diff, "D:/3PG_Cplusplus/_delete/lp_biomass_future_DIFF.tif")

# Read in the NTEMS Biomass for the study area

ntems_agb <- rast("D:/Landcover/M_9S/biomass_ntems_full.tif")
ntems_agb.90 <-  terra::aggregate(ntems_agb, 100/res(ntems_agb)[1], cores = 8)

# Read landcover?
land <- rast("D:/BP_Layers/M_9S/landcover/LC_Class_HMM_2021_v20_v20.dat")
land.proj <- terra::project(land, ntems_agb, method = 'near', threads = T)
land.90 <- terra::aggregate(land.proj, 100/res(y.rast)[1], fun = 'modal', cores = 8)

compareGeom(lp_agb.mask, land.90)

#writeRaster(land.90, "D:/BP_Layers/M_9S/landcover/landcover_90m.tif")

# Set up the plot window to have 2 columns
par(mfrow = c(2, 2))

plot(ntems_agb.90)
plot(lp_agb.mask)
plot(bs_agb.mask)
plot(lp_245_agb.mask)

par(mfrow=c(1,1))

# Species
species <- rast("D:/BP_Layers/outputs/inputs/leading-species_2019_2.tif")
species.90 <-  terra::aggregate(species, 100/res(y.rast)[1], fun = 'modal', cores = 8)
species.90.f <- as.factor(species.90)

#writeRaster(species.90.f, "D:/3PG_Cplusplus/_delete/species_90m.tif")

# We can check the number of pixels for each species:
# Calculate pixel counts for each category - this is for later...
species.df <- as.data.frame(species.90.f)

category_counts <- table(species.df)
lp_fraction <- category_counts[6] / sum(category_counts)

lp_bs <- c(6, 3, 4, 5)
lp_fraction <- sum(category_counts[lp_bs]) / sum(category_counts)

global(species.90.f, "notNA")

############################################
# mask by species

# Can do something like this:
mask.lp <- species.90.f == 23

# We want to check specific species so mask:

lp.abg.masked.species <- mask(lp_agb, mask.lp, maskvalues = c(NA,0))

#lp.abg.masked.species[!mask.lp] <- NA

ntems_agb.90.mask.lp <- mask(ntems_agb.90, mask.lp, maskvalues = c(NA,0))

#ntems_agb.90.mask.lp[!mask.lp] <- NA

lp_245_agb.mask.lp <- mask(lp_245_agb, mask.lp, maskvalues = c(NA,0))
#lp_245_agb.mask.lp[!mask.lp] <- NA


compareGeom(lp.abg.masked.species,ntems_agb.90.mask.lp)


######

names(lp_245_agb.mask) <- "lp_245"
names(bs_agb.mask) <- "bs"
names(lp_agb.mask) <- "lp"
names(ntems_agb.90) <- "NTEMS"

names(lp.abg.masked.species) <- "lp"
names(ntems_agb.90.mask.lp) <- "NTEMS"
names(lp_245_agb.mask.lp) <- "lp_future"

# quick check - we know that the masked rasters should have fewer pixels than the original species mask, unless the cell is 100% 1 species (unlikely)
global(lp.abg.masked.species, "notNA")
global(ntems_agb.90.mask.lp, "notNA")

#############
# WE ARE HERE - there is a difference in number of pixels (3000 or so)
#############

# Quick stats

global(ntems_agb.90.mask.lp, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
global(lp.abg.masked.species, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
#global(lp_agb.mask, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)


#######################################################################################################

# LOOK AT SUMMARY STATISTICS OF THE DATA
stacked.bio <- c(lp.abg.masked.species, ntems_agb.90.mask.lp, lp_245_agb.mask.lp)

plot(stacked.bio, col =viridis(100))

df.bio <- as.data.frame(stacked.bio)

colnames(df.bio)[1] <- "lp"
colnames(df.bio)[2] <- "NTEMS"
colnames(df.bio)[3] <- "lp_future"

# Summarize the raster 889
summary.df.bio <- df.bio %>%
  summarize(
    mean = mean(NTEMS, na.rm = T),
    min = min(NTEMS, na.rm = T),
    max = max(NTEMS, na.rm = T),
    sd = sd(NTEMS, na.rm = T),
    median = median(NTEMS, na.rm = T),
    quartile_25 = quantile(NTEMS, 0.25, na.rm = T),
    quartile_75 = quantile(NTEMS, 0.75, na.rm = T)
  ) %>%
  bind_rows(df.bio %>%
              summarize(
                mean = mean(lp, na.rm = T),
                min = min(lp, na.rm = T),
                max = max(lp, na.rm = T),
                sd = sd(lp, na.rm = T),
                median = median(lp, na.rm = T),
                quartile_25 = quantile(lp, 0.25, na.rm = T),
                quartile_75 = quantile(lp, 0.75, na.rm = T)
              ))%>%
    bind_rows(df.bio %>%
                  summarize(
                      mean = mean(lp_future, na.rm = T),
                      min = min(lp_future, na.rm = T),
                      max = max(lp_future, na.rm = T),
                      sd = sd(lp_future, na.rm = T),
                      median = median(lp_future, na.rm = T),
                      quartile_25 = quantile(lp_future, 0.25, na.rm = T),
                      quartile_75 = quantile(lp_future, 0.75, na.rm = T)
                  ))



# Set row names as the column headings
rownames(summary.df.bio) <- c("NTEMS", "lp", "lp_future")

# Transpose the summary dataframe
summary.df.bio_t <- t(summary.df.bio)

#summary_df892_table <- summary_df892_t

#colnames(summary_df892_table) <- c("Original Double Rad", "New Param", "Original Half Rad", "Meyer et al. 2013", "NTEMS")

df_long <- df.bio %>%
  pivot_longer(cols = everything(), names_to = "Column Heading", values_to = "Value")

# Define a custom color palette
my_palette <- c("#2AB7CA", "#ED217C", "#5AD660")

# Plot the density using the long format dataframe

# Define custom legend labels
my_labels <- c("LP", "LP Future", "NTEMS")

# Define the data frame for means
mean_data <- data.frame(
    Category = c("LP", "LP Future", "NTEMS"),
    MeanValue = c(127.8, 138.6, 103.8))
#mean_data <- mean_data[complete.cases(mean_data), ]

# Plot the density using the long format dataframe
density_plot <- ggplot(df_long, aes(x = Value, color = `Column Heading`)) +
  geom_density(size = 0.75) +
  labs(x = "Value", y = "Density") +
  scale_color_manual(values = my_palette, name = "Prediction", labels = my_labels) +
  theme_bw() +
  ylim(0, 0.04)+
    geom_vline(data = mean_data, aes(xintercept = MeanValue),
                          linetype = "dashed", color = my_palette, size = 0.75, show.legend = FALSE) +
    guides(linetype = FALSE)+
    coord_cartesian(xlim = c(0, 300))  # Set x-axis limits

##
# Plot the density using the long format dataframe
density_plot <- ggplot(df_long, aes(x = Value, color = `Column Heading`)) +
    geom_density(size = 0.75) +
    labs(x = "Value", y = "Density") +
    scale_color_manual(values = my_palette, name = "Prediction", labels = my_labels) +
    theme_bw() +
    ylim(0, 0.04) +
    geom_vline(data = mean_data, aes(xintercept = MeanValue),
               linetype = "dashed", color = my_palette, size = 0.75, show.legend = FALSE) +
    guides(linetype = FALSE) +
    coord_cartesian(xlim = c(0, 300)) +  # Set x-axis limits
    geom_text(data = mean_data, aes(x = MeanValue, y = 0.037, label = MeanValue),
              vjust = -1, color = "black", size = 3)


# Randomly sample 50,000 points
df_sample <- df.bio %>%
  sample_n(50000)


#

# Create separate scatter plots for each combination of variables
plot_lp <- ggplot(df_sample, aes(x = NTEMS, y = lp)) +
  geom_point() +
  labs(x = "NTEMS", y = "lp") +
  ggtitle("lp vs. NTEMS") +
  theme_minimal()

# Get the range of NTEMS values
ntems_range <- range(df_sample$NTEMS)

# Create separate heat maps for each combination of variables
plot_lp2 <- ggplot(df_sample, aes(x = NTEMS, y = lp)) +
  geom_hex(bins = 100) +
  labs(x = "NTEMS", y = "lp") +
  ggtitle("lp vs. NTEMS") +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlim(ntems_range) +
  ylim(ntems_range) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)


correlation <- cor(df.bio$lp, df.bio$NTEMS, use = "complete.obs")


############
library(scatter)

calc.error(reference = df_bio$NTEMS, estimate = df_bio$lp)

p.scatter <- scatter(x = df_sample$NTEMS, y = df_sample$lp)

p.scatter2 <- scatter(x = df_sample$lp, y = df_sample$NTEMS)

# Display the heat maps side by side
grid.arrange(plot_lp2, p.scatter, p.scatter2, ncol = 3)



##################################################################################
# M_18S

ws_bs <- rast("D:/3PG_Cplusplus/Output_100_18S/ws202007.flt")
wf_bs <- rast("D:/3PG_Cplusplus/Output_100_18S/wf202007.flt")

# Combine the rasters to get total AGB

bs_agb <- ws_bs + wf_bs

# Read in the forest age mask so that we can mask out the treed areas for current comparisons
tree.mask <- rast("D:/BP_Layers/M_18S/tree_mask.tif")
tree.mask.90 <-  terra::aggregate(tree.mask, 100/res(y.rast)[1])

# Mask out the non-treed areas

bs_agb.mask <- terra::mask(lp_agb, tree.mask.90)

# Read in the NTEMS Biomass for the study area

ntems_agb.18 <- rast("D:/Landcover/francois5/Study_Area_M_eighteenS/18S/structure/total_biomass/UTM_18S_total_biomass_2021.dat")
ntems_agb.18 <- ntems_agb.18 / 100

ntems_agb.18.90 <-  terra::aggregate(ntems_agb.18, 100/res(y.rast)[1], cores = 8)

bs_agb.proj <- terra::project(bs_agb, crs(ntems_agb.18.90), gdal = TRUE, threads = TRUE)

crs(bs_agb) <- crs(ntems_agb.18.90)

compareGeom(bs_agb, ntems_agb.18.90)


# Read landcover?
#land <- rast("")
#land.proj <- terra::project(land, ntems_agb, method = 'near', threads = T)
#land.90 <- terra::aggregate(land.proj, 100/res(y.rast)[1], fun = 'modal', cores = 8)

#compareGeom(lp_agb.mask, land.90)

# Set up the plot window to have 2 columns
par(mfrow = c(1, 2))

plot(ntems_agb.18.90)
plot(bs_agb)

par(mfrow=c(1,1))

# Species
species.18 <- rast("D:/BP_Layers/M_18S/inputs/leading-species_2019_18S.tif")
species.18.90 <-  terra::aggregate(species.18, 100/res(y.rast)[1], fun = 'modal', cores = 8)
species.18.90.f <- as.factor(species.18.90)

# We can check the number of pixels for each species:
# Calculate pixel counts for each category - this is for later...
species.df <- as.data.frame(species.18.90.f)

category_counts <- table(species.df)
bs_fraction <- category_counts[7] / sum(category_counts)

global(species.18.90.f, "notNA")

# Can do something like this:
mask.bs <- species.18.90.f == 18

# We want to check specific species so mask:
bs.abg.masked.species <- mask(bs_agb, mask.bs, maskvalues = c(NA,0))

ntems_agb.90.mask.bs <- mask(ntems_agb.18.90, mask.bs, maskvalues = c(NA,0))

# Set up the plot window to have 2 columns
par(mfrow = c(1, 2))

plot(ntems_agb.90.mask.bs)
plot(bs.abg.masked.species)

par(mfrow=c(1,1))

# Quick stats

global(ntems_agb.90.mask.bs, "notNA")
global(bs.abg.masked.species, "notNA")


global(ntems_agb.90.mask.bs, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)
global(bs.abg.masked.species, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)


##

names(bs.abg.masked.species) <- "bs"
names(ntems_agb.90.mask.bs) <- "NTEMS"

# LOOK AT SUMMARY STATISTICS OF THE DATA
stacked.bio.18 <- c(bs.abg.masked.species, ntems_agb.90.mask.bs)

plot(stacked.bio.18, col =viridis(100))

df.bio.18 <- as.data.frame(stacked.bio.18)

# Summarize the raster 889
summary.df.bio.18 <- df.bio.18 %>%
    summarize(
        mean = mean(NTEMS, na.rm = T),
        min = min(NTEMS, na.rm = T),
        max = max(NTEMS, na.rm = T),
        sd = sd(NTEMS, na.rm = T),
        median = median(NTEMS, na.rm = T),
        quartile_25 = quantile(NTEMS, 0.25, na.rm = T),
        quartile_75 = quantile(NTEMS, 0.75, na.rm = T)
    ) %>%
    bind_rows(df.bio.18 %>%
                  summarize(
                      mean = mean(bs, na.rm = T),
                      min = min(bs, na.rm = T),
                      max = max(bs, na.rm = T),
                      sd = sd(bs, na.rm = T),
                      median = median(bs, na.rm = T),
                      quartile_25 = quantile(bs, 0.25, na.rm = T),
                      quartile_75 = quantile(bs, 0.75, na.rm = T)
                  ))


# Set row names as the column headings
rownames(summary.df.bio.18) <- c("NTEMS", "bs")

# Transpose the summary dataframe
summary.df.bio.18_t <- t(summary.df.bio.18)

#summary_df892_table <- summary_df892_t

#colnames(summary_df892_table) <- c("Original Double Rad", "New Param", "Original Half Rad", "Meyer et al. 2013", "NTEMS")

df_long.18 <- df.bio.18 %>%
    pivot_longer(cols = everything(), names_to = "Column Heading", values_to = "Value")

# Define a custom color palette
my_palette <- c("#355EAF", "#FF6F00")

# Plot the density using the long format dataframe

# Define custom legend labels
my_labels <- c("NTEMS", "bs")

# Plot the density using the long format dataframe
ggplot(df_long.18, aes(x = Value, color = `Column Heading`)) +
    geom_density(size = 0.75) +
    labs(x = "Value", y = "Density") +
    scale_color_manual(values = my_palette, name = "Column Heading", labels = my_labels) +
    theme_bw() +
    ylim(0, 0.1)

# Randomly sample 50,000 points
df_sample <- df.bio %>%
    sample_n(50000)


#

# Create separate scatter plots for each combination of variables
plot_lp <- ggplot(df_sample, aes(x = NTEMS, y = lp)) +
    geom_point() +
    labs(x = "NTEMS", y = "lp") +
    ggtitle("lp vs. NTEMS") +
    theme_minimal()

# Get the range of NTEMS values
ntems_range <- range(df_sample$NTEMS)

# Create separate heat maps for each combination of variables
plot_lp2 <- ggplot(df_sample, aes(x = NTEMS, y = lp)) +
    geom_hex(bins = 100) +
    labs(x = "NTEMS", y = "lp") +
    ggtitle("lp vs. NTEMS") +
    scale_fill_viridis_c() +
    theme_minimal() +
    xlim(ntems_range) +
    ylim(ntems_range) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1)


correlation <- cor(df.bio$lp, df.bio$NTEMS, use = "complete.obs")

