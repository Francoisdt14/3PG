
# r3PG needs latitude information.. right now for a general climate, we can just get the center of a BOX. Eventually need the center of each pixel

source("R/lib.R")
# polygon information comes from 5_Tile_Raster

boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")

polygon <- boxes.v[i, ]
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

# Latitude from each cell found below:
# For each cell in the raster
lat_long <- project(lat.mask, "+proj=longlat +datum=WGS84")
# Extract y coordinate of each cell center
y_coordinate <- yFromRow(lat_long, 1:nrow(lat_long))
# Save as dataframe
df_y_coordinate <- data.frame(y_coordinate)


