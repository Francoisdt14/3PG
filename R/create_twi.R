library(whitebox)
library(tidyverse)
library(terra)
library(rgdal)

setwd("D:/BP_Layers/U_15S/large_rasters")

# Check if the DEM has NAs - if yes need to fill them:
##########################################################################################################
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

#writeRaster(raster_filled, "D:/BP_Layers/M_18S/inputs/dem_filled_M_18S.tif")
##########################################################################################################

#Create TWI for a raster - to be used in arc pro?

#input for whiteboxtools - don't read the DEM into R with the raster package
dem <- ("D:/climate/M_11S/dem_crop_M_11S.tif")
dem.rast <- rast("D:/climate/M_11S/dem_crop_M_11S.tif")

# first we can create the slope - can't see output of whiteboxtools in R, need to load in after to view
wbt_slope(dem, "D:/BP_Layers/M_11S/large_rasters/slope_M_11S.tif", zfactor = 1, units = "degrees")

######

# create a flow accumulation raster to use for TWI
wbt_d8_flow_accumulation(dem, "D:/BP_Layers/M_11S/large_rasters/flow_accum_M_11S.tif", out_type = "cells",
                         log = FALSE, clip = FALSE)

#####

# reload the slope and flow accumulation rasters that we created above
slope_wb <- ("D:/BP_Layers/M_11S/large_rasters/slope_M_11S.tif")
sca_wb<- ("D:/BP_Layers/M_11S/large_rasters/flow_accum_M_11S.tif")

# run TWI tool
wbt_wetness_index(sca_wb,slope_wb,
                  "D:/BP_Layers/M_11S/large_rasters/TWI_M_11S.tif",
                  wd = NULL, verbose_mode = FALSE)



# now we can load in the rasters using the raster package to view the products

slope <- rast("D:/BP_Layers/M_11S/large_rasters/slope_M_11S.tif")
flow_accum <- rast("D:/BP_Layers/M_11S/large_rasters/flow_accum_M_11S.tif")
TWI <- rast("D:/BP_Layers/M_11S/large_rasters/TWI_M_11S.tif")

# plot
par(mfrow=c(2,2))
plot(dem.rast, main="DEM")
plot(slope, main="Slope")
plot(flow_accum, main="Flow Accumulation")
plot(TWI, main="TWI", col = viridis(100))
par(mfrow=c(1,1))


#############################################################################################################

TWI.df <- as.data.frame(TWI)

#######
# Can do this multiple ways:
# Calculate the 5th and 95th percentiles of the original data
percentile_5 <- quantile(TWI.df$TWI_M_11S, probs = 0.05)
percentile_95 <- quantile(TWI.df$TWI_M_11S, probs = 0.95)

# Define the original and scaled percentile values
original_percentiles <- c(percentile_5, percentile_95)
scaled_percentiles <- c(100, 300)

# Calculate the slope and intercept of the linear equation
slope <- (scaled_percentiles[2] - scaled_percentiles[1]) / (original_percentiles[2] - original_percentiles[1])
intercept <- scaled_percentiles[1] - slope * original_percentiles[1]
####
# OR:
# Create a linear regression model
#model <- lm(scaled_values ~ original_values)
#summary(model)
# formula to follow:
#scaled_values = intercept + slope * original_values

TWI.scaled <- (intercept) + (slope * TWI)
hist(TWI.scaled)

TWI.clamp <- clamp(TWI.scaled, lower = 0, upper = Inf)
TWI.clamp
######

mean.TWI <- global(TWI.clamp, fun = mean, na.rm = TRUE)


#writeRaster(TWI.clamp, "D:/BP_Layers/M_11S/inputs/scaled_TWI.tif")

TWI.clamp.noNA <- TWI.clamp

TWI.clamp.noNA[is.na(TWI.clamp.noNA)] <- mean.TWI
#plot(TWI.clamp.noNA, col = viridis(100))
writeRaster(TWI.clamp.noNA, "D:/BP_Layers/M_11S/inputs/scaled_TWI_noNA.tif")

TWI.90m = terra::aggregate(TWI.clamp.noNA, 100/res(TWI.clamp.noNA)[1], cores = 12)

writeRaster(TWI.90m, "D:/BP_Layers/M_11S/3PG_flt/5_90m_inputs_all/scaled_TWI_noNA.tif")



