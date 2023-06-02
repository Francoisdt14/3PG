library(sgsR)
library(terra)
library(tidyverse)

dem <- rast("D:/BP_Layers/outputs/inputs/dem_crop_M_9S.tif")
dem_crop <- crop(dem, volume_3pg)

species <- rast("D:/BP_Layers/outputs/inputs/leading-species_2019_2.tif")
species_crop <- crop(species, volume_3pg)

age <- rast("D:/BP_Layers/outputs/inputs/Forest_Age_2019.tif")
age_crop <- crop(age, volume_3pg)

tmax08 <- rast("D:/BP_Layers/outputs/climate/Tmax08.tif")
tmax08_crop <- crop(tmax08, volume_3pg)

layer_stack <- c(dem_crop, species_crop, age_crop, tmax08_crop)

# Set new names for the raster layers
new_names <- c("dem", "species", "age", "tmax08")

# Change the names of the raster layers in the stack
names(layer_stack) <- new_names

##### We have the following rasters for 3PG and NTEMS:


basal_3pg <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/basal_49.tif")
biomas_3pg <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/biom_stem_49.tif")
volume_3pg <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/volume_49.tif")

basal_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/basal_49.tif")
biomas_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/biomass_49.tif")
volume_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/volume_49.tif")

# NTEMS rasters are saved with different converison factors due to size constraints
basal_ntems <- (basal_ntems / 100)
biomas_ntems <- (biomas_ntems / 100)
volume_ntems <- (volume_ntems / 10)

basal_3pg_NORM <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/basal_3pg_NORM.tif")
biomass_3pg_NORM <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/biomass_3pg_NORM.tif")
volume_3pg_NORM <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/volume_3pg_NORM.tif")

basal_ntems_NORM <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/basal_ntems_NORM.tif")
biomass_ntems_NORM <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/biomas_ntems_NORM.tif")
volume_ntems_NORM <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/volume_ntems_NORM.tif")

############################################################################################################

############################## LOOK AT SUMMARY STATISTICS OF THE DATA
# Convert raster to data frame
df <- as.data.frame(biomas_ntems, xy = TRUE)
colnames(df)[3] <- "value"
df2 <- as.data.frame(biomas_3pg, xy = TRUE)
colnames(df2)[3] <- "value"
df3 <- as.data.frame(volume_ntems, xy = TRUE)
colnames(df3)[3] <- "value"
df4 <- as.data.frame(volume_3pg, xy = TRUE)
colnames(df4)[3] <- "value"

# Summarize the raster
summary1 <- df %>%
    summarize(
        mean = mean(value),
        min = min(value),
        max = max(value),
        sd = sd(value),
        median = median(value),
        quartile_25 = quantile(value, 0.25),
        quartile_75 = quantile(value, 0.75)
    )

# Summarize the raster
summary2 <- df2 %>%
    summarize(
        mean = mean(value),
        min = min(value),
        max = max(value),
        sd = sd(value),
        median = median(value),
        quartile_25 = quantile(value, 0.25),
        quartile_75 = quantile(value, 0.75)
    )

# Combine the data frames

df_tot <- cbind(df$value, df2$value, df3$value, df4$value)
colnames(df_tot) <- c("bio_ntems", "bio_3pg", "vol_ntems", "vol_3pg")
df_tot <-  as.data.frame(df_tot)

# Create the density plot

biom_comp <- ggplot(df_tot, aes(x = bio_ntems, fill = "bio_ntems")) +
    geom_density(alpha = 0.5) +
    geom_density(aes(x = bio_3pg, fill = "bio_3pg"), alpha = 0.5) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(title = "Density Plot Comparison", x = "Values", y = "Density") +
    theme_minimal()


vol_comp <- ggplot(df_tot, aes(x = vol_ntems, fill = "vol_ntems")) +
    geom_density(alpha = 0.5) +
    geom_density(aes(x = vol_3pg, fill = "vol_3pg"), alpha = 0.5) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(title = "Density Plot Comparison", x = "Values", y = "Density") +
    theme_minimal()



##############################
# sgsR

# we select the DEM and the temperature rasters to look at
ls <- layer_stack[[c(1,4)]]
# select just the species stack
sp <- layer_stack[[2]]


# 2 layers, 3 strata, then add species:
qt_strat <- strat_quantiles(mraster = ls, nStrata = list(3,3), plot = TRUE)

qt_sp <- c(qt_strat,sp)

# here are the mapped strata stacked created in above, plus species:
mapped <- strat_map(qt_sp, stack = TRUE, plot = TRUE)

# just taking the 1st strata - the DEM
mapped_1 <- mapped[[1]]
names(mapped_1) <- "strata"

mapped_2 <- mapped[[2]]
names(mapped_2) <- "strata"


# single layer, 3 strata example"
qt_strat_2 <- strat_quantiles(layer_stack[[1]], nStrata = 3, plot = TRUE)
#qt_breaks <- strat_breaks(layer_stack[[1]], breaks = c(700,1000,1500,2000), plot = TRUE)

#qt_sp <- c(qt_strat,sp)

# take 200 samples from the DEM stratification
s.dem <- sample_strat(sraster = qt_sp[[1]], nSamp = 200, plot= TRUE)
s.clim <-
s.sp <-

#sample_strat(sraster = qt_breaks, nSamp = 100, plot=TRUE, allocation = "equal", mindist = 100)

# systematic stratification
s1 <- sample_systematic(raster = ls[[1]], cellsize = 1000, square = FALSE, plot = TRUE)


calculate_representation(sraster = mapped_1$strata, existing = s, plot = TRUE)


bio <- c(biomas_ntems, biomas_3pg)
names(bio) <- c("ntems_bio","pg_bio")


s2 <- extract_metrics(mraster = bio, existing = s.dem)


####


s2.dat <- as.data.frame(s2) %>% select(-c(type, rule, geometry))


ggplot(filter(s2.dat, strata == 1), aes(x = ntems_bio, color = "ntems")) +
    geom_density() +
    geom_density(aes(x = pg_bio, color = "pg")) +
    labs(title = "Density Plot of ntems and pg (strata = 1)", x = "Value", y = "Density") +
    scale_color_manual(values = c("ntems" = "blue", "pg" = "red")) +
    theme_minimal()


ggplot(filter(s2.dat), aes(x = ntems_bio, color = "ntems")) +
    geom_density() +
    geom_density(aes(x = pg_bio, color = "pg")) +
    labs(title = "Density Plot of ntems and pg (strata = 1)", x = "Value", y = "Density") +
    scale_color_manual(values = c("ntems" = "blue", "pg" = "red")) +
    theme_minimal()


# Assuming 'data' is the dataframe containing the columns ntems, pg, and strata

# Create separate density plots for ntems and pg with different lines for each strata


ntems_plot <- ggplot(s2.dat, aes(x = ntems_bio, color = factor(strata))) +
    geom_density() +
    labs(title = "Density Plot of ntems by Strata",
         x = "ntems",
         y = "Density") +
    scale_color_manual(values = c("blue", "red", "green")) +
    theme_minimal()

pg_plot <- ggplot(s2.dat, aes(x = pg_bio, color = factor(strata))) +
    geom_density() +
    labs(title = "Density Plot of pg by Strata",
         x = "pg",
         y = "Density") +
    scale_color_manual(values = c("blue", "red", "green")) +
    theme_minimal()

# Combine the plots using cowplot
combined_plot <- cowplot::plot_grid(ntems_plot, pg_plot, nrow = 1)

# Display the combined plot
print(combined_plot)

# Assuming 'data' is the dataframe containing the columns ntems, pg, and strata

combined_plot <- ggplot(s2.dat) +
    geom_density(aes(x = ntems_bio, color = factor(strata), linetype = "ntems"), linewidth = 1) +
    geom_density(aes(x = pg_bio, color = factor(strata), linetype = "pg"), linewidth = 1) +
    labs(title = "Density Plot of ntems and pg by Strata",
         x = "Value",
         y = "Density") +
    scale_color_manual(values = c("#fde725", "#21918c", "#440154")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_minimal()
# Display the combined plot
print(combined_plot)


#fde725","#5ec962","#21918c","#3b528b","#440154"

s2.dat %>%
    group_by(strata) %>%
    summarise(mean = mean(pg_bio))



################################################


# what raster are we interested in?
biom_3pg2 <- (biomas_3pg*2)

raster <- age_crop


# Calculate statistics

summary_stats <- global(raster, c("max", "min", "mean", "sd", "rms"), na.rm=TRUE)

# Convert raster values to a dataframe
df <- as.data.frame(values(raster)) %>% na.omit()

df$focal_mean <- ifelse(df$focal_mean <= 1869, 1869, df$focal_mean)

# Create a density plot
density_plot <- ggplot(df, aes(x = df$focal_mean)) +
    geom_density() +
    labs(title = "Density Plot of Raster Values")

# Display the density plot
print(density_plot)

# basic histogram
p <- ggplot(df, aes(x=focal_mean)) +
    geom_histogram()

print(p)





###########################

sl <- terrain(dem_crop, "slope", unit = "radians")
plot(sl)
asp <- terrain(dem_crop, "aspect", unit = "radians")
plot(asp)


# calculate the hillshade effect with 45º of elevation
hill_single <- shade(sl, asp,
                     angle = 45,
                     direction = 300,
                     normalize= TRUE)

# final hillshade
plot(hill_single, col = grey(1:100/100))


# pass multiple directions to shade()
hillmulti <- map(c(270, 15, 60, 330), function(dir){
    shade(sl, asp,
          angle = 45,
          direction = dir,
          normalize= TRUE)}
)

# create a multidimensional raster and reduce it by summing up
hillmulti <- rast(hillmulti) %>% sum()

# multidirectional
plot(hillmulti, col = grey(1:100/100))

# plot raster data
plot(hill_single,
     main = "DEM overlayed on top of a hillshade",
     col = grey(1:100/100),
     legend = FALSE)

plot(dem_crop,
     add = TRUE, alpha = .5)

###############################



