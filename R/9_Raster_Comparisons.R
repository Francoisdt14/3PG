# Creating a raster of the outputs

source("R/lib.R")


####################### Cropping the NTEM Rasters for comparisons
# LoAD NTEMS
basal_canada <- rast("Y:/Muise/francois4/Study_Area_M_nineS/mosaiced/structure/basal_area/basal_area_2021.dat") # note supercomputer error!
volume_canada <- rast("Y:/Muise/francois4/Study_Area_M_nineS/mosaiced/structure/gross_stem_volume/gross_stem_volume_2021.dat")
biomass_canada <- rast("Y:/Muise/francois4/Study_Area_M_nineS/mosaiced/structure/total_biomass/total_biomass_2021.dat")

##################
# Set the projection of the large raster to match the small raster
volume_canada <- project(volume_canada, crs(volume_3pg))

# Crop the masked large raster to the extent of the small raster
volume_canada_cropped <- crop(volume_canada, volume_3pg)

# Write the raster
# writeRaster(volume_canada_cropped, filename = paste0("D:/BP_Layers/outputs/crops/ntems_rasters/","volume", "_","49",".tif"))

# Check that it displays correctly
plot(volume_canada_cropped, col=viridis(100),  main="NTEMS")

##################### DIFFERENCE OUR OUTPUT WITH NTEMS
# These rasters are produced in script 8, and compared with the NTEMS rasters which are cropped from above

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

diff_basal <- (basal_3pg - basal_ntems)
diff_biomass <- (biomas_3pg - biomas_ntems)
diff_volumes <- (volume_3pg - volume_ntems)

# Set up the plot window to have three columns
par(mfrow = c(1, 3))

plot(basal_3pg, col=viridis(100),  main="3PG basal")
plot(basal_ntems, col=viridis(100),  main="NTEMS basal")
plot(diff_basal, col=viridis(100),  main="DIFFERENCE")

plot(biomas_3pg, col=viridis(100),  main="3PG biomass")
plot(biomas_ntems, col=viridis(100),  main="NTEMS biomass")
plot(diff_biomass, col=viridis(100),  main="DIFFERENCE")

plot(volume_3pg, col=viridis(100),  main="3PG volume")
plot(volume_ntems, col=viridis(100),  main="NTEMS volume")
plot(diff_volumes, col=viridis(100),  main="DIFFERENCE")

# Reset the par to the default of one plot per window
par(mfrow = c(1, 1))


# Define the maximum value for the common scale
common_max <- minmax(biomas_ntems)[2,]

par(mfrow = c(1, 2))

# Set manually the breaks
breaks = seq(0,common_max,30)
pal <- colorRampPalette(c("white","orange","yellow","green","forestgreen"))
pal <- colorRampPalette(c("#fde725","#5ec962","#21918c","#3b528b","#440154"))

plot(biomas_3pg, breaks=breaks, col = pal(length(breaks)-1))
plot(biomas_ntems, breaks=breaks, col = pal(length(breaks)-1))


par(mfrow = c(1, 1))

# WORTHWHILE TO NORMALIZE EVERYTHING TO 1 TO LOOK FOR SPATIAL PATTERNS??

rast1 <- volume_ntems
# Get the minimum and maximum values of the raster
min_value <- minmax(rast1)[1,]
max_value <- minmax(rast1)[2,]

# Rescale the raster between 0 and 1
rast_NORM <- (rast1 - min_value) / (max_value - min_value)
plot(rast_NORM, col=viridis(100),  main="volume_ntems Normalized")

writeRaster(rast_NORM, filename = "D:/BP_Layers/outputs/crops/ntems_rasters/volume_ntems_NORM.tif")

######
basal_3pg_NORM <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/basal_3pg_NORM.tif")
biomass_3pg_NORM <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/biomass_3pg_NORM.tif")
volume_3pg_NORM <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/volume_3pg_NORM.tif")

basal_ntems_NORM <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/basal_ntems_NORM.tif")
biomass_ntems_NORM <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/biomas_ntems_NORM.tif")
volume_ntems_NORM <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/volume_ntems_NORM.tif")


norm_diff_basal <- (basal_3pg_NORM - basal_ntems_NORM)
norm_diff_biomass <- (biomass_3pg_NORM - biomass_ntems_NORM)
norm_diff_volume <- (volume_3pg_NORM - volume_ntems_NORM)

# Set up the plot window to have three columns
par(mfrow = c(1, 3))

plot(basal_3pg_NORM, col=viridis(100),  main="Normalized 3PG basal")
plot(basal_ntems_NORM, col=viridis(100),  main="Normalized NTEMS basal")
plot(norm_diff_basal, col=viridis(100),  main="DIFFERENCE")


par(mfrow = c(1, 1))



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

ggplot(df_tot, aes(x = bio_ntems, fill = "bio_ntems")) +
    geom_density(alpha = 0.5) +
    geom_density(aes(x = bio_3pg, fill = "bio_3pg"), alpha = 0.5) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(title = "Density Plot Comparison", x = "Values", y = "Density") +
    theme_minimal()


ggplot(df_tot, aes(x = vol_ntems, fill = "vol_ntems")) +
    geom_density(alpha = 0.5) +
    geom_density(aes(x = vol_3pg, fill = "vol_3pg"), alpha = 0.5) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(title = "Density Plot Comparison", x = "Values", y = "Density") +
    theme_minimal()


