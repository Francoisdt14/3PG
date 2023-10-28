# Create a latitude raster and float
library(terra)
# Import the tree mask - aligned to everything
tree.mask <- rast("D:/BP_Layers/M_11S/tree_mask.tif")

# Rename
lat.mask = tree.mask
# Set NAs to 1 - we need everything
lat.mask[is.na(lat.mask)] <- 1
# Project
lat_long <- terra::project(lat.mask, "+proj=longlat +datum=WGS84", threads = T, gdal = TRUE, by_util = TRUE)
# Extract y coordinate of each cell center
y_coordinate <- yFromCell(lat_long, 1:ncell(lat_long))

# Turn y coords into a raster
y.rast = lat_long
values(y.rast) <- y_coordinate %>% round(4)
# Project back to original CRS - different for each tree mask!
y.rast = terra::project(y.rast, crs(lat.mask), threads = T, gdal = TRUE, by_util = TRUE)

y.rast <- resample(y.rast, lat.mask, "bilinear") %>%
    focal(w = 5, fun = "mean", na.policy = "only", na.rm = T)

# Write the raster
writeRaster(y.rast, "D:/BP_Layers/M_11S/large_rasters/latitude_30m.tif")

# Aggregate
lat_90 <-  terra::aggregate(y.rast, 100/res(y.rast)[1])
# Write again - tif and flt

writeRaster(lat_90, "D:/BP_Layers/M_11S/large_rasters/latitude_90m.tif")
#writeRaster(lat_90, "D:/BP_Layers/M_18S/3PG_flt/6_90m_flt_other_inputs/latitude_90.flt", datatype = "FLT4S", overwrite = TRUE)


#test.rast <- rast("D:/BP_Layers/M_18S/3PG_flt/5_90m_inputs_all/Tmax02.tif")


compareGeom(test.rast,lat_90)

