library(terra)
library(GGally)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
# Looking at BIOMASS FIRST:

######################################################################################################################
# M 9 S -

# Read in Biomass Rasters for WF (foliage) and WS (Stem) that need to be combined..
# Lodgepole Pine
ws_lp <- rast("D:/3PG_Cplusplus/_delete/ws_lp.flt")
wf_lp <- rast("D:/3PG_Cplusplus/_delete/wf_lp.flt")
# Black Spruce
ws_bs <- rast("D:/3PG_Cplusplus/_delete/ws_bs.flt")
wf_bs <- rast("D:/3PG_Cplusplus/_delete/wf_bs.flt")
# Future Lodgepole Pine
ws_lp_245 <- rast("D:/3PG_Cplusplus/_delete/ws_lp_245.flt")
wf_lp_245 <- rast("D:/3PG_Cplusplus/_delete/wf_lp_245.flt")

# Combine the rasters to get total AGB

lp_agb <- ws_lp + wf_lp
bs_agb <- ws_bs + wf_bs
lp_245_agb <- ws_lp_245 + wf_lp_245

# Read in the forest age mask so that we can mask out the treed areas for current comparisons
tree.mask <- rast("D:/BP_Layers/outputs/tree_mask.tif")
tree.mask.90 <-  terra::aggregate(tree.mask, 100/res(y.rast)[1])

# Mask out the non-treed areas

lp_agb.mask <- terra::mask(lp_agb, tree.mask.90)
bs_agb.mask <- terra::mask(bs_agb, tree.mask.90)
lp_245_agb.mask <- terra::mask(lp_245_agb, tree.mask.90)

# Read in the NTEMS Biomass for the study area

ntems_agb <- rast("D:/Landcover/M_9S/biomass_ntems_full.tif")
ntems_agb.90 <-  terra::aggregate(ntems_agb, 100/res(y.rast)[1], cores = 8)

# Read landcover?
land <- rast("D:/BP_Layers/M_9S/landcover/LC_Class_HMM_2021_v20_v20.dat")
land.proj <- terra::project(land, ntems_agb, method = 'near', threads = T)
land.90 <- terra::aggregate(land.proj, 100/res(y.rast)[1], fun = 'modal', cores = 8)

compareGeom(lp_agb.mask, land.90)

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

# We can check the number of pixels for each species:
# Calculate pixel counts for each category - this is for later...
species.df <- as.data.frame(species.90.f)

category_counts <- table(species.df)
lp_fraction <- category_counts[6] / sum(category_counts)

lp_bs <- c(6, 3, 4, 5)
lp_fraction <- sum(category_counts[lp_bs]) / sum(category_counts)

global(species.90.f, "notNA")

# Can do something like this:
mask.lp <- species.90.f == 23

# We want to check specific species so mask:

lp.abg.masked.species <- lp_agb

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
my_palette <- c("#355EAF", "#FF6F00", "forestgreen")

# Plot the density using the long format dataframe

# Define custom legend labels
my_labels <- c("NTEMS", "lp", "lp_future")

# Plot the density using the long format dataframe
ggplot(df_long, aes(x = Value, color = `Column Heading`)) +
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


############
library(scatter)

calc.error(reference = df_bio$NTEMS, estimate = df_bio$lp)

p.scatter <- scatter(x = df_sample$NTEMS, y = df_sample$lp)

p.scatter2 <- scatter(x = df_sample$lp, y = df_sample$NTEMS)

# Display the heat maps side by side
grid.arrange(plot_lp2, p.scatter, p.scatter2, ncol = 3)



##################################################################################
# M_18S





