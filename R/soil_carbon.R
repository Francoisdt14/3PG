library(terra)
library(sf)


dem.rast <- rast("D:/BP_Layers/NCC_DEM/dem_crop_M_9S_30m.tif")

sca <- rast("D:/BP_Layers/NCC_DEM/SCA.tif")
slope <- rast("D:/BP_Layers/NCC_DEM/SLOPE.tif")

# reload the slope and flow accumulation rasters that we created above
slope_wb <- ("D:/BP_Layers/NCC_DEM/SLOPE.tif")
sca_wb<- ("D:/BP_Layers/NCC_DEM/SLOPE.tif")

# run TWI tool
wbt_wetness_index(sca_wb,slope_wb,
                 "D:/BP_Layers/M_9S/3PG_flt/TWI_test_ncc.tif",
                  wd = NULL, verbose_mode = FALSE)


cti_test <- rast("D:/BP_Layers/M_9S/3PG_flt/TWI_test_ncc.tif")

cti.df <- as.data.frame(cti_test)

cti_test.new = 168.1807 * cti_test - 273.80884

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

writeRaster(dem.align, "D:/BP_Layers/NCC_DEM/Scaled_CTI_aligned.tif" )

###

soil.carbon <- rast("D:/BP_Layers/NCC_DEM/GSOCmap1.5.0.tif")
boreal <- vect("D:/BP_Layers/shapefiles/Boreal_Forest.shp")


crs.soil <- crs(soil.carbon)

#project the study area shapefile
#boreal.proj <- terra::project(boreal, crs.soil, threads = T)
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

soil.utm9.90m <- rast("D:/BP_Layers/M_9S/3PG_flt/1_other_inputs/soil_carbon_aligned.tif")


# Scaling based on entire boreal range of values - see excel sheet
#################
# Can do this multiple ways:
# Calculate the 5th and 95th percentiles of the original data
soil.crop.df <- as.data.frame(soil.crop)

percentile_5 <- quantile(soil.crop.df$GSOCmap1.5.0, probs = 0.1)
percentile_95 <- quantile(soil.crop.df$GSOCmap1.5.0, probs = 0.9)

# Define the original and scaled percentile values
original_percentiles <- c(percentile_5, percentile_95)
scaled_percentiles <- c(0.05, 0.95)

# Calculate the slope and intercept of the linear equation
slope <- (scaled_percentiles[2] - scaled_percentiles[1]) / (original_percentiles[2] - original_percentiles[1])
intercept <- scaled_percentiles[1] - slope * original_percentiles[1]


###########################################################################################################################################################
## I clipped the organic carbon data across the entire boreal first - that is 'soil.crop'

# first look at a histogram of the entire boreal forest to get an idea of the range of values
soil.crop.df <- as.data.frame(soil.crop)
hist(soil.crop.df$GSOCmap1.5.0)

# This is the 10th and 98th percentile of the entire boreal..
percentile_10 <- quantile(soil.crop.df$GSOCmap1.5.0, probs = 0.1)
percentile_98 <- quantile(soil.crop.df$GSOCmap1.5.0, probs = 0.98)

# Define the original and scaled percentile values
original_percentiles <- c(percentile_10, percentile_98)
# When scaling I tried to keep the fertility relatively high - so the 10th percentile of my values are still 'forced' to have a fertility of 0.45. Did this after discussion with NCC
# Looking at the histogram 200 is about as high as needed that can be the 99th percentile
scaled_percentiles <- c(0.45, 0.99)

# Calculate slope and intercept
slope <- (scaled_percentiles[2] - scaled_percentiles[1]) / (original_percentiles[2] - original_percentiles[1])
intercept <- scaled_percentiles[1] - slope * original_percentiles[1]

# soil.utm9.90m is soil carbon layer clipped to my area
soil.scale.3 <- (slope  * soil.utm9.90m) + intercept
# calculate mean fertility just to fill NAs
mean.fert <- global(soil.scale.3, fun = "mean", na.rm = TRUE)
# fill NAs
soil.scale.3[is.na(soil.scale.3)] <- mean.fert
# Smooth the raster
soil.scale.3 <- focal(soil.scale.3, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

#writeRaster(soil.scale.2, "D:/BP_Layers/M_18S/inputs/soil_carbon_aligned_scaled_noNA.tif", overwrite = T)

#################

# ORIGINAL:
#soil.scale <- (0.0033 * soil.utm9.90m) + 0.33333

#soil.scale[is.na(soil.scale)] <- 0.5
#soil.scale <- focal(soil.scale, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)


#soil.scale <- rast("D:/BP_Layers/NCC_DEM/soil_carbon_aligned_scaled_noNA.tif")
######

# Write the raster in FLT4S format to the output file
#writeRaster(soil.scale, "D:/BP_Layers/NCC_DEM/soil_carbon_aligned_scaled_noNA.flt", datatype = "FLT4S", overwrite = TRUE)
#writeRaster(cti.align, "D:/BP_Layers/NCC_DEM/Scaled_CTI_aligned.flt",  datatype = "FLT4S", overwrite = TRUE )

#writeRaster(dem.align, "D:/BP_Layers/NCC_DEM/dem.flt",  datatype = "FLT4S", overwrite = TRUE )


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

check.raster5 <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/Forest_Age_2019.flt")
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

