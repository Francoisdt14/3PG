library(terra)
library(dplyr)

# Creating various landcover scenarios
# DONE: M_9S, M_11S, M_18S, U_18S, U_15S,
# Starting inputs to change

#mask.30m <- rast("D:/BP_Layers/outputs/tree_mask.tif")
#landcover <-  rast("D:/Landcover/francois5/Study_Area_M_nineS/mosaiced/VLCE2.0/LC_Class_HMM_2020_v20_v20.dat")
#fao <- rast("Y:/Muise/francois5/CA_FAO_forest_2019/CA_FAO_forest_2019.tif")

mask.30m <- rast("D:/BP_Layers/U_13N/tree_mask.tif")
landcover <-  rast("D:/Landcover/francois5/Study_Area_U_thirteenN/mosaiced/VLCE2.0/LC_Class_HMM_2021_v20_v20.dat")
fao <- rast("Y:/Muise/francois5/CA_FAO_forest_2019/CA_FAO_forest_2019.tif")

# select an output directory
out.dir = "D:/BP_Layers/U_13N/landcover/"
dir.create(out.dir, showWarnings = FALSE)
#################################################################################################################
# STEP 1 - load landcover map

# project landcover and write it to the appropriate folder
lc.proj <- terra::project(landcover, mask.30m, threads = T, gdal = TRUE, by_util = TRUE)

#write landcover for future use if necessary
terra::writeRaster(lc.proj, paste(out.dir, "landcover_30m.tif", sep = ""), overwrite = T)

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
# This could be tricky depending on study area
fao.crop <- crop(fao, landcover)
fao.proj <- terra::project(fao.crop, mask.30m, threads = T, gdal = TRUE, by_util = TRUE)

####### IF FAIL #############

# load in vector of study area
#study <- vect("D:/Landcover/francois5/Study_Area_M_eighteenS.shp")
#study.project <- project(study, crs(fao))
# Crop and project to the correct study area
#fao.crop <- crop(fao, study.project)
#fao.proj <- terra::project(fao.crop, mask.30m, threads = T, gdal = TRUE, by_util = TRUE)

#############################

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
# TESTING BARREN LAND MASKS


# Create mask for barren land
mask.30m <- rast("D:/BP_Layers/M_9S/tree_mask.tif")
lc.proj <- rast("D:/BP_Layers/M_9S/landcover/landcover_30m.tif")

x <- droplevels(lc.proj) # get rid of un-necessary data

# Shrubs AND Bryoids
xnum.barren <- as.numeric(x) # convert the raster to numeric - need to keep an eye on which codes we want
xnum.barren[!xnum.barren %in% c(33)] <- NA # here we want to keep the bryoid, shrubland, herbs 40, 50, 100.. so set everything that isn't to NA
#forested landcover types (81, 210, 220, 230)
xnum.barren[!is.na(xnum.barren)] <- 1 # set the desired landcovers to 1

fao.reclass2 <- rast("D:/BP_Layers/M_9S/landcover/fao_forest_30m.tif")

# invert the above mask because we want areas that AREN'T forest
fao.reclass.inverse <- ifel(!is.na(fao.reclass2), 0, 1)

# multiply for areas to remove any potential overlap
barren.mask <- xnum.barren * fao.reclass.inverse

#create NA
barren.mask.final <- ifel(barren.mask == 0, NA, barren.mask)

plot(barren.mask.final, col = 'black')

barren.mask.final.agg <- terra::aggregate(barren.mask.final, 100/res(barren.mask.final)[1], cores = 12)

test <- rast("D:/BP_Layers/M_9S/biomass_3PG/S1/Y3_output/ws.flt")

# Get the EPSG code from fao.mask
epsg_code <- crs(lc.proj, describe = T)$code
# Set the CRS of s1 using the extracted EPSG code
crs(test) <- paste0("EPSG:", epsg_code)

test.mask <- terra::mask(test, barren.mask.final.agg)

#writeRaster(fao.reclass2.agg, "D:/BP_Layers/M_9S/landcover/fao_forest_90m.tif")



################################################################################################


##############################################################################################################################################
study <- vect("D:/Landcover/francois5/Study_Area_M_eighteenS.shp")
study.project <- project(study, crs(fao))
# Crop and project to the correct study area
fao.crop <- crop(fao, study.project)
fao.proj <- terra::project(fao.crop, mask.30m, threads = T, gdal = TRUE, by_util = TRUE)

compareGeom(fao.proj, mask.30m)
