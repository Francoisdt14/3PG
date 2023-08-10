library(terra)
library(sf)


dem.rast <- rast("D:/BP_Layers/NCC_DEM/dem_crop_M_9S_30m.tif")

sca <- rast("D:/BP_Layers/NCC_DEM/SCA.tif")
slope <- rast("D:/BP_Layers/NCC_DEM/SLOPE.tif")

cti <- rast("D:/BP_Layers/NCC_DEM/Scaled_CTI.tif")

cti.df <- as.data.frame((cti$Scaled_CTI))

####

# reference raster
check.rast <- rast("D:/Radiation/test_crop_project/5_90m_inputs_all/Rad06.tif")

# Aggregate DEM and CTI
dem.agg <-  terra::aggregate(dem.rast, 100/res(dem.rast)[1])
cti.agg <- terra::aggregate(cti, 100/res(cti)[1])

# Align the input raster to the reference raster
tic()
dem.align <- resample(dem.agg, check.rast, method = 'cubicspline', threads = T)
toc()

cti.align <- resample(cti.agg, check.rast, method = 'cubicspline', threads = T)

###
# Compare geom
compareGeom(dem.align, check.rast)
compareGeom(cti.align, check.rast)
####

#writeRaster(dem.align, "D:/BP_Layers/NCC_DEM/Scaled_CTI_aligned.tif" )

###

soil.carbon <- rast("D:/BP_Layers/NCC_DEM/GSOCmap1.5.0.tif")
boreal <- vect("D:/BP_Layers/shapefiles/Boreal_Forest.shp")


crs.soil <- crs(soil.carbon)

#project the study area shapefile
boreal.proj <- terra::project(boreal, crs.soil)

#crop the large raster using the projected study area
soil.crop <- terra::crop(soil.carbon, boreal.proj, threads = T, mask = T)

#shapefile of study area
study.area <- vect("D:/Landcover/francois2/Shapefiles/Study_Area_M_nineS.shp")
study.area.sf <- st_read("D:/Landcover/francois2/Shapefiles/Study_Area_M_nineS.shp")
study.buff <- st_buffer(study.area.sf, dist = 300)
study.buff2 <- vect(study.buff)

#project the study area shapefile
 # buffer this moving forward?
study.proj <- terra::project(study.buff2, crs.soil)


soil.crop.study <- terra::crop(soil.carbon, study.proj, threads = T, mask = T)

soil.utm9 <- terra::project(soil.crop.study, "EPSG:32609")

study.area.utm9 <- terra::project(study.area,"EPSG:32609")

soil.utm9.crop <- terra::crop(soil.utm9, study.area.utm9)

# Resample the large raster to match the resolution of the smaller raster
soil.utm9.90m <- resample(soil.utm9, check.rast, method = "cubicspline", threads = T)
soil.utm9.90m <- focal(soil.utm9.90m, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

###


compareGeom(check.rast, soil.utm9.90m)


writeRaster(soil.utm9.90m, "Y:/Francois/_dem/soil_carbon_aligned.tif", overwrite = T)

soil.scale <- (0.0033 * soil.utm9.90m) + 0.33333

soil.scale[is.na(soil.scale)] <- 0.5
soil.scale <- focal(soil.scale, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

writeRaster(soil.scale, "D:/BP_Layers/NCC_DEM/soil_carbon_aligned_scaled_noNA.tif", overwrite = T)

######

# Write the raster in FLT4S format to the output file
writeRaster(soil.scale, "D:/BP_Layers/NCC_DEM/soil_carbon_aligned_scaled_noNA.flt", datatype = "FLT4S", overwrite = TRUE)
writeRaster(cti.align, "D:/BP_Layers/NCC_DEM/Scaled_CTI_aligned.flt",  datatype = "FLT4S", overwrite = TRUE )

writeRaster(dem.align, "D:/BP_Layers/NCC_DEM/dem.flt",  datatype = "FLT4S", overwrite = TRUE )


###
# rewrite HDR files!

library(stringr)

# Set the directory where the .hdr and .flt files are located

#directory <- "Y:/Francois/flt_test_100_noNA"
directory <- "D:/BP_Layers/NCC_DEM/"
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


dbh.3pg <- rast("Y:/Francois/_dem/C++/Output_100/dbh.flt")
vol.3pg <- rast("Y:/Francois/_dem/C++/Output_100/s_vol.flt")



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

writeRaster(hill_single, "D:/BP_Layers/NCC_DEM/hillshade.flt",  datatype = "FLT4S", overwrite = TRUE )

