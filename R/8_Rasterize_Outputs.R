# Creating a raster of the outputs

source("R/lib.R")
library(viridis)

#df.test <- result_df
#write.csv(df.424.50y, file = "D:/BP_Layers/outputs/crops/npp/df.424.50y.csv")
df.test <- read.csv("D:/BP_Layers/outputs/crops/dataframes_2/454.csv")
df.test2 <- read.csv("D:/BP_Layers/outputs/crops/dataframes_50yr/454.csv")

lat.test <- read.csv("D:/BP_Layers/outputs/crops/lat/515.csv") %>% na.omit
inputs.test <- read.csv("D:/BP_Layers/outputs/crops/climate/454.csv") %>% na.omit

#colnames(lai.df.033)[1]  <- "LAI"    # change column name for x column

## making a large raster

# mask_crop = rast("D:/BP_Layers/outputs/tree_mask.tif")


# k = number of box we are on (424 here)

k = 454

polygon <- boxes.v[k, ]
mini.r <-  crop(mask_crop, polygon)

n.pix = global(mini.r, "notNA") %>% as.numeric()

r.id = mini.r
r.id[!is.na(r.id)] <- 1:n.pix #npp.df
plot(r.id)

#dt = values(r.id, na.rm = T) %>% as.data.table()

# Read in blank raster
#r = rast()

# How many valid pixels?
num.valid = global(r.id, "notNA") %>% as.numeric()


# Read in output (NPP or whatever) -> change this to the relevant output
vals = df.test %>% pull("npp")

# This should be true
length(vals) == num.valid

# Fill in the blank raster with the output pixels
mini.r[!is.na(mini.r)] <- vals

plot(mini.r)

df.424.50y.rast <- mini.r

#writeRaster(lai.rast, "...crops/lai_034.tif")

plot(df.424.50y.rast)

terra::writeRaster(dbh.488.rast, filename = "D:/BP_Layers/outputs/crops/output_rasters/dbh488.tif")






#################################
# Loop through this to make our rasters

mask_crop <- rast("D:/BP_Layers/outputs/tree_mask.tif")
boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")

csv_folder <- "D:/BP_Layers/outputs/crops/dataframes_50yr_2"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = csv_folder, pattern = "*.csv")


keyword <- "lai"

# where are things saved?
target_folder <- paste0("D:/BP_Layers/outputs/crops/output_rasters_50yr/", keyword)


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


########################### COMBINING AND GROUPING INDIVIDUAL TILES

# combining groups of tiles :
raster_folder <- paste0("D:/BP_Layers/outputs/crops/output_rasters/", keyword)
# Get a list of all CSV files in the folder
raster_files <- list.files(path = raster_folder, pattern = "*.tif")
# terra::mosaic

group.of.49 <- terra::vrt(paste(raster_folder, raster_files, sep = "/"))

plot(group.of.49, col=viridis(100),  main=paste0(keyword))

writeRaster(group.of.49, filename = paste0("D:/BP_Layers/outputs/crops/output_rasters_50yr/49_composite/",keyword, "_","49",".tif"))


######################## Cropping the NTEM Rasters for comparisons

# LoAD NTEMS
basal_canada <- rast("Y:/Muise/francois4/Study_Area_M_nineS/mosaiced/structure/basal_area/basal_area_2021.dat")
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

basal_3pg <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/basal_49.tif")
biomas_3pg <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/biom_stem_49.tif")
volume_3pg <- rast("D:/BP_Layers/outputs/crops/output_rasters/49_composite/volume_49.tif")

basal_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/basal_49.tif")
biomas_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/biomass_49.tif")
volume_ntems <- rast("D:/BP_Layers/outputs/crops/ntems_rasters/volume_49.tif")

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


diff <- norm_diff_basal

# Define the thresholds and colors
thresh_red <- 0.7
thresh_yellow <- 0.3

color_red <- "red"
color_yellow <- "blue"
color_green <- "green"

# Reclassify the raster
reclassified_raster <- diff
# Reclassify the raster
reclassified_raster <- reclassify(diff,
                                  cbind(-Inf, -thresh_red, NA,
                                        -thresh_red, thresh_red, NA,
                                        thresh_red, Inf, NA,
                                        -thresh_yellow, thresh_yellow, 1,
                                        -thresh_red, thresh_red, 2,
                                        thresh_yellow, thresh_red, 2,
                                        -thresh_red, -thresh_yellow, 3,
                                        -thresh_red, thresh_red, 3))

# Define the color palette
color_palette <- c(color_green, color_yellow, color_red)

# Plot the reclassified raster
plot(reclassified_raster, col = color_palette)
############################## LOOK AT SUMMARY STATISTICS OF THE DATA
# Convert raster to data frame
df <- as.data.frame(volume_canada_cropped, xy = TRUE)
colnames(df)[3] <- "value"

df2 <- as.data.frame(group.of.49, xy = TRUE)
colnames(df2)[3] <- "value"
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

