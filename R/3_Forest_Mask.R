# Create a Mask to keep only forested areas in our datasets!

library(tidyverse); library(terra); library(data.table); library(silvR21); library(parallel); library(pbapply); library(arrow); library(readxl)

##################################################################################
# test site = M_9S
# folder of inputs: D:/BP_Layers/M_9S

# study area shapefile
study <- vect("D:/BP_Layers/M_9S/study_area_polygon/Study_Area_M_nineS.shp") %>% project("EPSG:32609")

# Landcover - use this to create a mask for all other layers to be used in 3PG

landcover <-  rast("D:/BP_Layers/M_9S/landcover/LC_Class_HMM_2021_v20_v20.dat") %>% project("EPSG:32609") #project raster
vlce <- read.csv("D:/BP_Layers/M_9S/vlce.csv") # numeric codes for the landcover types

x <- droplevels(landcover) # get rid of un-necessary data
xnum <- as.numeric(x) # convert the raster to numeric - need to keep an eye on which codes we want
xnum[!xnum %in% c(81, 210, 220, 230)] <- NA # here we want to keep the forested landcover types, so set everything that isn't to NA
xnum[!is.na(xnum)] <- 1 # set the forested landcovers to 1

#plot(xnum, col = "darkgreen")
mask_crop <- terra::crop(xnum, study)

#writeRaster(mask_crop, "D:/BP_Layers/outputs/tree_mask.tif", datatype = "INT2U", overwrite = T) # write this mask as it will be used moving forward!
