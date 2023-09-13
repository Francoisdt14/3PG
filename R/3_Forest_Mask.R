# Create a mask to keep only forested areas in our datasets!
# This raster is important because it will be the reference for all the rasters later on


library(tidyverse); library(terra); library(data.table); library(silvR21); library(parallel); library(pbapply); library(arrow); library(readxl)

##################################################################################
# test site = M_9S
# folder of inputs: D:/BP_Layers/M_9S

# study area shapefile
# MAKE SURE TO PROJECT TO CORRECT UTM ZONE
study <- vect("D:/BP_Layers/study_areas/Study_Area_M_eighteenS.shp") %>% project("EPSG:32618")

# Landcover - use this to create a mask for all other layers to be used in 3PG

landcover <-  rast("D:/Landcover/francois5/Study_Area_M_eighteenS/18S/VLCE2.0/LC_Class_HMM_18S_2021_v20_v20.dat") %>% project("EPSG:32618") #project raster to correct projection
vlce <- read.csv("D:/BP_Layers/M_9S/vlce.csv") # numeric codes for the landcover types

x <- droplevels(landcover) # get rid of un-necessary data
xnum <- as.numeric(x) # convert the raster to numeric - need to keep an eye on which codes we want
xnum[!xnum %in% c(81, 210, 220, 230)] <- NA # here we want to keep the forested landcover types, so set everything that isn't to NA
xnum[!is.na(xnum)] <- 1 # set the forested landcovers to 1

#plot(xnum, col = "darkgreen")
mask_crop <- terra::crop(xnum, study)

writeRaster(mask_crop, "D:/BP_Layers/M_18S/tree_mask.tif", datatype = "INT2U", overwrite = T) # write this mask as it will be used moving forward!


###############################
# If we want to make the mask for different forest types - we can do that here:

x <- droplevels(landcover) # get rid of un-necessary data
xnum <- as.numeric(x) # convert the raster to numeric - need to keep an eye on which codes we want
xnum[!xnum %in% c(81, 210, 220, 230, 40, 50, 100, 33)] <- NA # here we want to keep the forested PLUS potentially forested, so set everything that isn't to NA
xnum[!is.na(xnum)] <- 1 # set the forested landcovers to 1
