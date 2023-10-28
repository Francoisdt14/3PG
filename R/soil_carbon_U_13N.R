library(terra)
library(sf)
library(tictoc)


dem.rast <- rast("D:/climate/U_13N/dem_crop_U_13N.tif")
###

soil.carbon <- rast("D:/BP_Layers/NCC_DEM/GSOCmap1.5.0.tif")
boreal <- vect("D:/BP_Layers/shapefiles/Boreal_Forest.shp")

crs.soil <- crs(soil.carbon)

#project the study area shapefile
boreal.proj <- terra::project(boreal, crs.soil)

#crop the large raster using the projected study area
soil.crop <- terra::crop(soil.carbon, boreal.proj, threads = T, mask = T)

#shapefile of study area
#study.area <- vect("D:/Landcover/francois2/Shapefiles/Study_Area_M_eighteenS.shp")

study.area.sf <- st_read("D:/Landcover/francois2/Shapefiles/Study_Area_U_thirteenN.shp")

study.buff <- st_buffer(study.area.sf, dist = 300)

study.buff2 <- vect(study.buff)

#project the study area shapefile
 # buffer this moving forward?
study.proj <- terra::project(study.buff2, crs.soil)


soil.crop.study <- terra::crop(soil.carbon, study.proj, threads = T, mask = T)


soil.utm <- terra::project(soil.crop.study, crs(dem.rast), threads = T, gdal = TRUE, by_util = TRUE)

#study.area.utm <- terra::project(study.area,"EPSG:32618")

soil.utm.crop <- terra::crop(soil.utm, dem.rast)


# Resample the large raster to match the resolution of the smaller raster
soil.utm.30m <- resample(soil.utm, dem.rast, method = "cubicspline", threads = T)

soil.utm.30m <- focal(soil.utm.30m, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)


###


compareGeom(dem.rast, soil.utm.30m)


writeRaster(soil.utm.30m, "D:/BP_Layers/U_13N/inputs/soil_carbon_aligned_30m.tif", overwrite = T)


#test <- rast("Y:/Francois/_dem/soil_carbon_aligned.tif")

# based on entire boreal values
soil.scale <- (0.0033 * soil.utm.30m) + 0.33333

soil.scale[is.na(soil.scale)] <- 0.5
soil.scale <- focal(soil.scale, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

#writeRaster(soil.scale, "D:/BP_Layers/U_13N/inputs/soil_carbon_aligned_scaled_noNA.tif", overwrite = T)

soil.scale.90m <- terra::aggregate(soil.scale, 100/res(soil.scale)[1], cores = 12)

writeRaster(soil.scale.90m, "D:/BP_Layers/U_13N/inputs/soil_carbon_aligned_scaled_noNA_90m.tif", overwrite = T)

#soil.scale <- rast("D:/BP_Layers/M_18S/inputs/soil_carbon_aligned_scaled_noNA.tif")
######


# Write the raster in FLT4S format to the output file
#writeRaster(soil.scale, "D:/BP_Layers/M_18S/inputs/soil_carbon_aligned_scaled_noNA.flt", datatype = "FLT4S", overwrite = TRUE)

#writeRaster(cti.align, "D:/BP_Layers/NCC_DEM/Scaled_CTI_aligned.flt",  datatype = "FLT4S", overwrite = TRUE )

writeRaster(soil.scale.90m, "D:/BP_Layers/M_18S/inputs/soil_carbon_aligned_scaled_noNA_90m.flt",  datatype = "FLT4S", overwrite = TRUE )


###
# rewrite HDR files!

library(stringr)

# Set the directory where the .hdr and .flt files are located

#directory <- "Y:/Francois/flt_test_100_noNA"
directory <- "D:/BP_Layers/M_18S/inputs/"
# Get the list of .hdr files in the directory
hdr_files <- list.files(directory, pattern = "\\.hdr$", full.names = TRUE)

# very important to write this correctly depending on raster size!!

# Process each .hdr file
for (hdr_file in hdr_files) {
  # Define the new content for the .hdr file
  new_content <- c(
    "NROWS          3249",
    "NCOLS          3249",
    "xllcenter         703617.363255706",
    "yllcenter         6612634.12879149",
    "cellsize           92.35435",
    "nodata_value -9999.000000",
    "byteorder lsbfirst"
  )



  # Write the new content to the .hdr file, overwriting the existing contents
  writeLines(new_content, hdr_file)
}

# random final checks

check.raster5 <- rast("D:/Radiation/test_crop_project/6_90m_flt/Tmin02.flt")
check.raster6 <- rast("D:/BP_Layers/NCC_DEM/Scaled_CTI_aligned.flt")
check.raster7 <- rast("D:/BP_Layers/NCC_DEM/hillshade.flt")

compareGeom(check.raster5, check.raster7)


##############################################################################################################################

#####
sl <- terrain(dem.align, "slope", unit = "radians")
plot(sl)

# estimate the aspect or orientation
asp <- terrain(dem.align, "aspect", unit = "radians")
plot(asp)

# calculate the hillshade effect with 45º of elevation
hill_single <- shade(sl, asp,
                     angle = 45,
                     direction = 300,
                     normalize= TRUE)

# final hillshade
plot(hill_single, col = viridis(100))

#writeRaster(hill_single, "D:/BP_Layers/M_18S/inputs/hillshade.tif", overwrite = TRUE )

