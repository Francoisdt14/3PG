
# r3PG needs latitude information.. right now for a general climate, we can just get the center of a BOX. Eventually need the center of each pixel

source("R/lib.R")
# polygon information comes from 5_Tile_Raster

# i = 44 to 46...

boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")
polygon <- boxes.v[i, ]

#mask_crop <- rast("D:/BP_Layers/outputs/tree_mask.tif")
lat.mask = crop(mask_crop, polygon)

# For the center of the entire raster (box):

# Get the extent of the raster
r_extent <- ext(lat.mask)
# Get the CRS of the raster
r_crs <- crs(lat.mask)
# Calculate the center point
center_x <- (r_extent[1] + r_extent[2]) / 2
center_y <- (r_extent[3] + r_extent[4]) / 2
center_point <- cbind(center_x, center_y)

center_spatial <- SpatialPoints(center_point)
proj4string(center_spatial) <- CRS("+init=epsg:32609")
center_spatial_lat <- spTransform(center_spatial, CRS("+init=epsg:4326")) %>% as.data.frame

lat <- center_spatial_lat[1,2]
# lat is then used in the next script.. right now it is a placeholder and needs to be integrated correctly per pixel!

########################################################################################################################
# TODO fix this bogusness
# Latitude from each cell found below:
# For each cell in the raster

#### just set lat.mask to mask_crop and do the whole thing?
#lat.mask <-

lat_long <- project(lat.mask, "+proj=longlat +datum=WGS84")

# Extract y coordinate of each cell center
y_coordinate <- yFromCell(lat_long, 1:ncell(lat_long))

# TUrn y coords into a raster
y.rast = lat_long
values(y.rast) <- y_coordinate %>% round(4)

# remove non-forested pixels
y.rast = project(y.rast, crs(lat.mask)) %>%
         resample(., lat.mask, "bilinear") %>%
         focal(w = 5, fun = "mean", na.policy = "only", na.rm = T) %>%
         mask(lat.mask)

global(y.rast, "notNA")
global(lat.mask, "notNA")

plot(y.rast)

# Save as dataframe
df_y_coordinate <- data.frame(y_coordinate)

# I THINK THIS IS THE ONE TO SAVE:
lat.test <- values(y.rast, xy = TRUE) %>% as.data.table()

#write.csv(df_y_coordinate, file = "D:/BP_Layers/outputs/crops/lat/044.csv", row.names = FALSE)
write.csv(lat.test, file = "D:/BP_Layers/outputs/crops/lat/044.csv", row.names = FALSE)


# Put y.rast through tiling to match boxes???
