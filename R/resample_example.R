library(terra)


# If we need to resample

# example raster that isn't perfect resolution
mask.30ish <- rast("D:/BP_Layers/outputs/tree_mask.tif")

# raster is 30.78478 in resolution

# Raster that is the correct resolution - set by the 'res' argument and using the correct epsg
rast.30 <- terra::project(landcover, "epsg:32609", threads = T, gdal = TRUE, by_util = TRUE, res = 30, mask = TRUE)

# Crop them to have the same extent as our original raster
rast30.crop <- terra::crop(test1, mask.30ish)

#now resample
mask30.correct <- terra::resample(mask.30m, test2, method = "near", threads = T)

# If we want to aggregate to 90 m we can do that here
test3.agg <- terra::aggregate(test3, 100/res(test3)[1], cores = 12)

