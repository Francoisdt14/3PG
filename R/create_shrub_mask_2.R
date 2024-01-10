library(terra)
library(dplyr)

# Creating various landcover scenarios

# Starting inputs to change

mask.30m <- rast("D:/BP_Layers/outputs/tree_mask.tif")
landcover <-  rast("D:/Landcover/francois5/Study_Area_M_nineS/mosaiced/VLCE2.0/LC_Class_HMM_2020_v20_v20.dat")
fao <- rast("Y:/Muise/francois5/CA_FAO_forest_2019/CA_FAO_forest_2019.tif")

# select an output directory
out.dir = "D:/BP_Layers/M_9S/landcover/"
dir.create(output_folder, showWarnings = FALSE)
#################################################################################################################
# STEP 1 - load landcover map

# project landcover and write it to the appropriate folder
lc.proj <- terra::project(landcover, mask.30m, threads = T, gdal = TRUE, by_util = TRUE)

#writeRaster(lc.proj, "D:/BP_Layers/M_9S/landcover/landcover_30m.tif")

# vlce codes
vlce <- read.csv("D:/BP_Layers/M_9S/vlce.csv")

# STEP 2 - isolate the correct layers from landcover.. mask OUT forest (from FAO), and non relevant landcover types
x <- droplevels(lc.proj) # get rid of un-necessary data

# Shrubs AND Bryoids
xnum.ShBr <- as.numeric(x) # convert the raster to numeric - need to keep an eye on which codes we want
xnum.ShBr[!xnum.ShBr %in% c(40, 50, 100)] <- NA # here we want to keep the bryoid, shrubland, herbs 40, 50, 100.. so set everything that isn't to NA
#forested landcover types (81, 210, 220, 230)
xnum.ShBr[!is.na(xnum.ShBr)] <- 1 # set the desired landcovers to 1


# Shrubs and herbs here - No BRYOID (50)... Need a version with and without
xnum.Sh <- as.numeric(x)
xnum.Sh[!xnum.Sh %in% c(40, 100)] <- NA
xnum.Sh[!is.na(xnum.Sh)] <- 1

# STEP 3 - load FAO FOREST. Use mask to identify future growth ... mask out in scenarios

# Load the FAO forest

# Crop and project to the correct study area
fao.crop <- crop(fao, landcover)
fao.proj <- terra::project(fao.crop, mask.30m, threads = T, gdal = TRUE, by_util = TRUE)

# reclassify non-forest to NA
fao.reclass <- ifel(fao.proj == 0, NA, fao.proj)
# reclassify all forest to 1
fao.reclass2 <- ifel(!is.na(fao.reclass), 1, fao.reclass)

# invert the above mask because we want areas that AREN'T forest
fao.reclass.inverse <- ifel(!is.na(fao.reclass2), 0, 1)

# multiply for areas to remove any potential overlap
ShBy.mask <- xnum.ShBr * fao.reclass.inverse
Sh.mask <- xnum.Sh * fao.reclass.inverse

#create NA
ShBy.mask.final <- ifel(ShBy.mask == 0, NA, ShBy.mask)
Sh.mask.final <- ifel(Sh.mask == 0, NA, Sh.mask)

# combine forest and shrub area

forest.ShBy.comb <- c(fao.reclass2, ShBy.mask.final) %>% sum(na.rm = T)
forest.Sh.comb <- c(fao.reclass2, Sh.mask.final) %>% sum(na.rm = T)

##############################################################################################
# Write the rasters to the correct folders

#fao forest
terra::writeRaster(fao.reclass2, paste(out.dir, "fao_forest_30m.tif", sep = ""), overwrite = T)
# shrub layer(s)
terra::writeRaster(ShBy.mask.final, paste(out.dir, "Sh_Bry_30m.tif", sep = ""), overwrite = T)
terra::writeRaster(Sh.mask.final, paste(out.dir, "Sh_30m.tif", sep = ""), overwrite = T)
# combined layer(s)
terra::writeRaster(forest.ShBy.comb, paste(out.dir, "Sh_Bry_fao_30m.tif", sep = ""), overwrite = T)
terra::writeRaster(forest.Sh.comb, paste(out.dir, "Sh_fao_30m.tif", sep = ""), overwrite = T)

##############################################################################################

# aggregate things
fao.reclass2.agg <- terra::aggregate(fao.reclass2, 100/res(fao.reclass2)[1], cores = 12)

ShBy.mask.final.agg <- terra::aggregate(ShBy.mask.final, 100/res(ShBy.mask.final)[1], cores = 12)
Sh.mask.final.agg <- terra::aggregate(Sh.mask.final, 100/res(Sh.mask.final)[1], cores = 12)

forest.ShBy.comb.agg <- terra::aggregate(forest.ShBy.comb, 100/res(forest.ShBy.comb)[1], cores = 12)
forest.Sh.comb.agg <- terra::aggregate(forest.Sh.comb, 100/res(forest.Sh.comb)[1], cores = 12)

##############################################################################################
# Write the aggregated rasters to the correct folders

#fao forest
terra::writeRaster(fao.reclass2.agg, paste(out.dir, "fao_forest_90m.tif", sep = ""), overwrite = T)
# shrub layer(s)
terra::writeRaster(ShBy.mask.final.agg, paste(out.dir, "Sh_Bry_90m.tif", sep = ""), overwrite = T)
terra::writeRaster(Sh.mask.final.agg, paste(out.dir, "Sh_90m.tif", sep = ""), overwrite = T)
# combined layer(s)
terra::writeRaster(forest.ShBy.comb.agg, paste(out.dir, "Sh_Bry_fao_90m.tif", sep = ""), overwrite = T)
terra::writeRaster(forest.Sh.comb.agg, paste(out.dir, "Sh_fao_90m.tif", sep = ""), overwrite = T)

##################################################################################################



#writeRaster(fao.reclass2.agg, "D:/BP_Layers/M_9S/landcover/fao_forest_90m.tif")





# 30 meter comparison

mask.30m <- rast("D:/BP_Layers/outputs/tree_mask.tif")
compareGeom(mask.30m, shrub.mask.final)

# 90 meter comparison
mask.90m <- rast("D:/3PG_Cplusplus/data_100/dem_90.flt")

shrub.agg <- terra::aggregate(shrub.mask.final, 100/res(shrub.mask.final)[1], cores = 12)

#writeRaster(shrub.agg, "D:/BP_Layers/M_9S/landcover/shrub_mask_90m.tif")

global(shrub.agg, "notNA")

#combined_raster_90m <- ifel(!is.na(fao.reclass2.agg) | !is.na(shrub.agg), 1, NA)

combined_raster_90m <- c(fao.reclass2.agg, shrub.agg) %>% sum(na.rm = T)

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

writeRaster(result_raster, "D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/boosted_fertility.flt", datatype = "FLT4S")






# example raster that isn't perfect resolution
mask.30m
# Raster that is the correct resolution
test1 <- terra::project(landcover, "epsg:32609", threads = T, gdal = TRUE, by_util = TRUE, res = 30, mask = TRUE)
# Crop them to have the same extent
test2 <- terra::crop(test1, mask.30m)
# now resample
test3 <- terra::resample(mask.30m, test2, method = "near", threads = T)
test3.agg <- terra::aggregate(test3, 100/res(test3)[1], cores = 12)


