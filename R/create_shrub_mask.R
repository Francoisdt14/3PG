library(terra)

test <- rast("D:/3PG_Cplusplus/data_100/dem_90.flt")
plot(test)



test2 <- rast("D:/BP_Layers/M_9S/3PG_flt/5_90m_inputs_all/Forest_Age_2019_withNA.tif")
plot(test2)

test3 <- rast("D:/BP_Layers/M_9S/3PG_flt/5_90m_inputs_all/Forest_Age_2019_2025.tif")
plot(test3)

# study area shapefile
# MAKE SURE TO PROJECT TO CORRECT UTM ZONE
study <- vect("D:/BP_Layers/study_areas/Study_Area_U_thirteenN.shp")
# project to WGS 84 ...
study <- vect("D:/BP_Layers/study_areas/Study_Area_M_eighteenS.shp") %>% project(current_proj)

# Landcover - use this to create a mask for all other layers to be used in 3PG

landcover <-  rast("D:/Landcover/francois5/Study_Area_M_nineS/mosaiced/VLCE2.0/LC_Class_HMM_2020_v20_v20.dat")# %>% project(current_proj) #project raster to correct projection
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


# STEP 1 - load landcover map
landcover <-  rast("D:/Landcover/francois5/Study_Area_M_nineS/mosaiced/VLCE2.0/LC_Class_HMM_2020_v20_v20.dat")

lc.proj <- terra::project(landcover, mask.30m, threads = T, gdal = TRUE, by_util = TRUE)

writeRaster(lc.proj, "D:/BP_Layers/M_9S/landcover/landcover_30m.tif")

vlce <- read.csv("D:/BP_Layers/M_9S/vlce.csv")

# STEP 2 - isolate the correct layers from landcover.. mask OUT forest (from FAO), and non relevant landcover types
x <- droplevels(landcover) # get rid of un-necessary data
xnum <- as.numeric(x) # convert the raster to numeric - need to keep an eye on which codes we want

xnum[!xnum %in% c(40, 50, 100)] <- NA # here we want to keep the bryoid, shrubland, herbs 40, 50, 100.. so set everything that isn't to NA
#forested landcover types (81, 210, 220, 230)

xnum[!is.na(xnum)] <- 1 # set the forested landcovers to 1

#
proj <- terra::project(xnum, mask.30m, threads = T, gdal = TRUE, by_util = TRUE)

# STEP 3 - load FAO FOREST - same as AGE Raster? use mask to identify future growth ... mask out in scenarios

age.2019 <- rast("D:/BP_Layers/M_9S/3PG_flt/4_30m_inputs_all/Forest_Age_2019_withNA.tif")

age.mask <- ifel(!is.na(age.2019), 1, age.2019)

# change all values to 0 (except NA) and NAs to 1:
# this is an inverted mask - everytthing that doesn't have an age is 1, everything with an age is 0
age.mask.inverse <- ifel(!is.na(age.2019), 0, 1)

shrub.mask <- proj * age.mask.inverse

#
fao <- rast("Y:/Muise/francois5/CA_FAO_forest_2019/CA_FAO_forest_2019.tif")

fao.crop <- crop(fao, landcover)

fao.proj <- terra::project(fao.crop, mask.30m, threads = T, gdal = TRUE, by_util = TRUE)

fao.reclass <- ifel(fao.proj == 0, NA, fao.proj)

fao.reclass2 <- ifel(!is.na(fao.reclass), 1, fao.reclass)
fao.reclass2.agg <- terra::aggregate(fao.reclass2, 100/res(fao.reclass2)[1], cores = 12)

#writeRaster(fao.reclass2.agg, "D:/BP_Layers/M_9S/landcover/fao_forest_90m.tif")

# invert the above mask
fao.reclass.inverse <- ifel(!is.na(fao.reclass2), 0, 1)

shrub.mask2 <- proj * fao.reclass.inverse

shrub.mask.final <- ifel(shrub.mask2 == 0, NA, shrub.mask2)

combined_raster_30m <- ifel(!is.na(fao.reclass2) | !is.na(shrub.mask.final), 1, NA)

#writeRaster(shrub.mask.final, "D:/BP_Layers/M_9S/landcover/shrub_mask_30m.tif")
#writeRaster(combined_raster_30m, "D:/BP_Layers/M_9S/landcover/shrub_and_FAO_30m_mask.tif")

# 30 meter comparison

mask.30m <- rast("D:/BP_Layers/outputs/tree_mask.tif")
compareGeom(mask.30m, shrub.mask.final)

# 90 meter comparison
mask.90m <- rast("D:/3PG_Cplusplus/data_100/dem_90.flt")

shrub.agg <- terra::aggregate(shrub.mask.final, 100/res(shrub.mask.final)[1], cores = 12)

#writeRaster(shrub.agg, "D:/BP_Layers/M_9S/landcover/shrub_mask_90m.tif")

global(shrub.agg, "notNA")

combined_raster_90m <- ifel(!is.na(fao.reclass2.agg) | !is.na(shrub.agg), 1, NA)

writeRaster(combined_raster_90m, "D:/BP_Layers/M_9S/landcover/shrub_and_FAO_90m_mask.tif")

#########################
# Fertility

# get age WITH NA

# year trees were planted
planted.NA <- rast("D:/BP_Layers/M_9S/3PG_flt/5_90m_inputs_all/Forest_Age_2019_withNA.tif")

# change all values to 0 (except NA) and NAs to 1:
# this is where we should boost fertility to 1 in 2025
modified_raster <- ifel(!is.na(planted.NA), 0, 1)

# Now read in fertility:

fertility <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/soil_carbon_aligned_scaled_noNA.flt")

# Replace fertility values where there is a 1 in the previous raster
result_raster <- ifel(modified_raster == 1, 1, fertility)

writeRaster(result_raster, "D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/boosted_fertility.flt", , datatype = "FLT4S")
