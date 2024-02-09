library(sgsR)
library(terra)
library(tidyverse)
library(viridis)

dem <- rast("D:/BP_Layers/outputs/inputs/dem_crop_M_9S.tif")
dem <- rast("D:/BP_Layers/M_9S/dem/dem_crop_M_9S.tif")
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


summary_biomas_3pg <- global(biomas_3pg, c("max", "min", "mean", "sd", "rms"), na.rm=TRUE)

biomas_50 <- rast("D:/BP_Layers/outputs/crops/output_rasters_50yr/49_composite/biom_stem_49.tif")
biomas_80 <- rast("D:/BP_Layers/outputs/crops/output_rasters_80yr/49_composite/biom_stem_49.tif")

summary_biomas_50 <- global(biomas_50, c("max", "min", "mean", "sd", "rms"), na.rm=TRUE)
summary_biomas_80 <- global(biomas_80, c("max", "min", "mean", "sd", "rms"), na.rm=TRUE)


basal_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/basal_49.tif")
biomas_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/biomass_49.tif")
volume_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/volume_49.tif")

# NTEMS rasters are saved with different converison factors due to size constraints
basal_ntems <- (basal_ntems / 100)
biomas_ntems <- (biomas_ntems / 100)
volume_ntems <- (volume_ntems / 10)


summary_biomas_ntems <- global(biomas_ntems, c("max", "min", "mean", "sd", "rms"), na.rm=TRUE)

# Set up the plot window to have three columns
par(mfrow = c(1, 2))

plot(biomas_3pg, col=viridis(100),  main="3PG basal")
plot(biomas_80, col=viridis(100),  main="3PG basal - 80 yr")


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
colnames(df)[3] <- "NTEMS"
df2 <- as.data.frame(biomas_3pg, xy = TRUE)
colnames(df2)[3] <- "THREEPG"
df3 <- as.data.frame(biomas_50, xy = TRUE)
colnames(df3)[3] <- "THREEPG_50"
df4 <- as.data.frame(biomas_80, xy = TRUE)
colnames(df4)[3] <- "THREEPG_80"

df_bio <- cbind(df$NTEMS, df2$THREEPG, df3$THREEPG_50, df4$THREEPG_80) %>% as.data.frame()

colnames(df_bio)[1] <- "NTEMS"
colnames(df_bio)[2] <- "THREEPG"
colnames(df_bio)[3] <- "THREEPG_50"
colnames(df_bio)[4] <- "THREEPG_80"

# Summarize the raster
summary1 <- df_bio %>%
    summarize(
        mean = mean(NTEMS),
        min = min(NTEMS),
        max = max(NTEMS),
        sd = sd(NTEMS),
        median = median(NTEMS),
        quartile_25 = quantile(NTEMS, 0.25),
        quartile_75 = quantile(NTEMS, 0.75)
    )

# Summarize the raster
summary2 <- df_bio %>%
    summarize(
        mean = mean(THREEPG),
        min = min(THREEPG),
        max = max(THREEPG),
        sd = sd(THREEPG),
        median = median(THREEPG),
        quartile_25 = quantile(THREEPG, 0.25),
        quartile_75 = quantile(THREEPG, 0.75)
    )


summary3 <- df_bio %>%
    summarize(
        mean = mean(THREEPG_80),
        min = min(THREEPG_80),
        max = max(THREEPG_80),
        sd = sd(THREEPG_80),
        median = median(THREEPG_80),
        quartile_25 = quantile(THREEPG_80, 0.25),
        quartile_75 = quantile(THREEPG_80, 0.75)
    )



# Create the density plot

biom_comp <- ggplot(df_bio, aes(x = NTEMS, fill = "NTEMS BIOMASS VALUES")) +
    geom_density(alpha = 0.5) +
    geom_density(aes(x = THREEPG, fill = "3PG BIOMASS VALUES"), alpha = 0.5) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(x = "Biomass", y = "Density") +
    theme_minimal()


# vol_comp <- ggplot(df_tot, aes(x = vol_ntems, fill = "vol_ntems")) +
#     geom_density(alpha = 0.5) +
#     geom_density(aes(x = vol_3pg, fill = "vol_3pg"), alpha = 0.5) +
#     scale_fill_manual(values = c("blue", "red")) +
#     labs(title = "Density Plot Comparison", x = "Values", y = "Density") +
#     theme_minimal()





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
#s.clim <-
#s.sp <-

#sample_strat(sraster = qt_breaks, nSamp = 100, plot=TRUE, allocation = "equal", mindist = 100)

# systematic stratification
s1 <- sample_systematic(raster = age_crop, cellsize = 1000, square = FALSE, plot = TRUE)


calculate_representation(sraster = mapped_1$strata, existing = s, plot = TRUE)


bio <- c(biomas_ntems, biomas_3pg, age_crop)
names(bio) <- c("ntems_bio","pg_bio", "age")


s2 <- extract_metrics(mraster = bio, existing = s1)


####
#s2.dat <- as.data.frame(s2)
s2.dat <- as.data.frame(s2) %>% select(-geometry)
#s2.dat <- as.data.frame(s2) %>% select(-c(type, rule, geometry))


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


# what raster are we interested in? - don

raster <- age_crop


# Calculate statistics

summary_stats <- global(raster, c("max", "min", "mean", "sd", "rms"), na.rm=TRUE)

# Convert raster values to a dataframe
df_age <- as.data.frame(values(raster)) %>% na.omit()

df_age$focal_mean <- ifelse(df$focal_mean <= 1869, 1869, df$focal_mean)

summary3 <- df_age %>%
    summarize(
        mean = mean(focal_mean),
        min = min(focal_mean),
        max = max(focal_mean),
        sd = sd(focal_mean),
        median = median(focal_mean),
        quartile_25 = quantile(focal_mean, 0.25),
        quartile_75 = quantile(focal_mean, 0.75)
    )


# Create a density plot
density_plot <- ggplot(df_age, aes(x = df_age$focal_mean, fill = "Age")) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("blue")) +
    labs(x = "Age", y = "Density") +
    theme_minimal()


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

stacked_rasters <- c(biomas_ntems, biomas_3pg, age_crop)

df_stack <- as.data.frame(stacked_rasters)

colnames(df_stack)[1] <- "NTEMS"
colnames(df_stack)[2] <- "THREEPG"
colnames(df_stack)[3] <- "AGE"

# Plot NTEMS against AGE
age_plot <- ggplot(s2.dat, aes(x = age, y = pg_bio)) +
    geom_point() +
    xlab("AGE") +
    ylab("NTEMS") +
    ggtitle("Plot of NTEMS vs AGE")

age_plot_c <- ggplot(s2.dat) +
    # Scatterplot layer with small circles for NTEMS_BIO
    geom_point(aes(x = age, y = ntems_bio, color = "NTEMS_BIO"), size = 1) +
    # Scatterplot layer with small crosses for PG_BIO
    geom_point(aes(x = age, y = pg_bio, color = "PG_BIO"), size = 1) +
    xlab("AGE") +
    ylab("Biomass") +
    # Customize the legend
    scale_color_manual(values = c("blue", "red"), labels = c("NTEMS BIOMASS", "3PG BIOMASS")) +
    labs(color = "Data") +
    theme_minimal()


ntems_pg <- ggplot(s2.dat) +
 aes(x = ntems_bio, y = pg_bio) +
 geom_point(shape = "circle open", size = 1.5, colour = "#112446") +
 labs(x = "NTEMS", y = "3PG") +
 theme_minimal()



ggplot(s2.dat) +
    aes(x = pg_bio, y = ntems_bio) +
    geom_point(shape = "circle open", size = 1.5, colour = "#112446") +
    labs(x = "NTEMS", y = "3PG") +
    theme_minimal()
####################################################################################################

# Assuming your large dataframe is called "df" with columns "x" and "y"
# Randomly sample 5000 points from the dataframe
sampled_bio <- df_bio %>% sample_n(5000)

# Plot the sampled points in ggplot
test_plot <- ggplot(sampled_bio, aes(x = NTEMS  , y =THREEPG )) +
    geom_point() +
    xlab("X-axis") +
    ylab("Y-axis") +
    ggtitle("Randomly Sampled Points")




mask_crop <- rast("D:/BP_Layers/outputs/tree_mask.tif")
biomass_canada <- rast("Y:/Muise/francois4/Study_Area_M_nineS/mosaiced/structure/total_biomass/total_biomass_2021.dat")
height <- rast("Y:/Muise/francois4/Study_Area_M_nineS/mosaiced/structure/elev_p95/elev_p95_2021.dat")

# Set the projection of the large raster to match the small raster
biomass_canada <- project(biomass_canada, crs(mask_crop))

height <- project(height, crs(mask_crop))

# Crop the masked large raster to the extent of the small raster
biomass_ntems_full <- crop(biomass_canada, mask_crop)
height <- crop(height, mask_crop)


biomass_ntems_full <- biomass_ntems_full / 10
# be sure to change the number or
writeRaster(biomass_ntems_full, filename = "D:/BP_Layers/outputs/crops/ntems_rasters/biomass_ntems_full.tif")
