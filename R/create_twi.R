library(whitebox)
library(tidyverse)
library(terra)
library(rgdal)

setwd("D:/BP_Layers/M_18S/large_rasters")

dem <- rast("Z:/_CanadaLayers/Rasters/DEM/UTM18S_DEM.dat") #Z:\_CanadaLayers\Rasters\DEM
climate <- rast("D:/BP_Layers/M_18S/climate/Tmax09.tif")

#study <- vect("D:/BP_Layers/study_areas/Study_Area_M_eighteenS.shp") #D:\PostDoc\Layers\BP_Maps\Study_Area_1_18S

dem_crop <- terra::crop(dem, climate)

# Load your raster with NA values (replace 'your_raster_file.tif' with your actual file)
raster <- dem.rast
plot(raster, colNA="red")
raster[raster == 0] <- NA

raster_filled <- focal(raster, w = 9, fun = 'mean' , na.policy="only", na.rm=TRUE)
#plot(raster_filled, colNA="red")

global(raster, fun="isNA")

global(raster_filled, fun = 'isNA')

writeRaster(raster_filled, "D:/BP_Layers/M_18S/inputs/dem_filled_M_18S.tif")

#Create TWI for a raster - to be used in arc pro? 

#input for whiteboxtools - don't read the DEM into R with the raster package
dem <- ("D:/BP_Layers/M_18S/inputs/dem_filled_M_18S.tif")
dem.rast <- rast("D:/BP_Layers/M_18S/inputs/dem_filled_M_18S.tif")

# first we can create the slope - can't see output of whiteboxtools in R, need to load in after to view
wbt_slope(dem, "D:/BP_Layers/M_18S/large_rasters/slope_M_18S.tif", zfactor = 1, units = "degrees")

######

# create a flow accumulation raster to use for TWI
wbt_d8_flow_accumulation(dem, "D:/BP_Layers/M_18S/large_rasters/flow_accum_M_18S.tif", out_type = "cells",
                         log = FALSE, clip = FALSE)

#####

# reload the slope and flow accumulation rasters that we created above
slope_wb <- ("D:/BP_Layers/M_18S/large_rasters/slope_M_18S.tif")
sca_wb<- ("D:/BP_Layers/M_18S/large_rasters/flow_accum_M_18S.tif")

# run TWI tool
wbt_wetness_index(sca_wb,slope_wb,
                  "D:/BP_Layers/M_18S/large_rasters/TWI_M_18S.tif",
                  wd = NULL, verbose_mode = FALSE)



# now we can load in the rasters using the raster package to view the products

slope <- rast("D:/BP_Layers/M_18S/large_rasters/slope_M_18S.tif")
flow_accum <- rast("D:/BP_Layers/M_18S/large_rasters/flow_accum_M_18S.tif")
TWI <- rast("D:/BP_Layers/M_18S/large_rasters/TWI_M_18S.tif")

# plot
par(mfrow=c(2,2))
plot(dem.rast, main="DEM")
plot(slope, main="Slope")
plot(flow_accum, main="Flow Accumulation")
plot(TWI, main="TWI")
