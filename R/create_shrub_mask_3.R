library(terra)
library(dplyr)

# Creating various landcover scenarios - this is if landcover and fao mask are already cropped correctly

# large rasters - probably don't need

#mask.30m <- rast("D:/BP_Layers/U_13N/tree_mask.tif")
#landcover <-  rast("D:/Landcover/francois5/Study_Area_U_thirteenN/mosaiced/VLCE2.0/LC_Class_HMM_2021_v20_v20.dat")
#fao <- rast("Y:/Muise/francois5/CA_FAO_forest_2019/CA_FAO_forest_2019.tif")

#################################################################################################################
# STEP 1 - load landcover map

area <- 'M_9S'
out.dir <- paste0("D:/BP_Layers/", area, '/landcover/')

#
lc.proj <- rast(paste0('D:/BP_Layers/', area, '/landcover/landcover_30m_old.tif'))

# vlce codes
vlce <- read.csv("D:/BP_Layers/M_9S/vlce.csv")

# STEP 2 - isolate the correct layers from landcover.. mask OUT forest (from FAO), and non relevant landcover types
x <- droplevels(lc.proj) # get rid of un-necessary data

# Shrubs and herbs here - No BRYOID (40)... Need a version with and without
xnum.Sh <- as.numeric(x)
xnum.Sh[!xnum.Sh %in% c(50, 100)] <- NA


###### Checking the sum values for each class...
#test_data_frame <- as.data.frame(xnum.Sh)
# Use table to get counts of each unique value
#category_counts <- table(test_data_frame$category)
# Convert to a dataframe
#df_counts <- as.data.frame(category_counts)

#################################################

# set them all to 1
xnum.Sh[!is.na(xnum.Sh)] <- 1

# STEP 3 - load FAO FOREST. Use mask to identify future growth ... mask out in scenarios

# if it is already written:
fao.proj <- rast(paste0('D:/BP_Layers/', area, '/landcover/fao_forest_30m.tif'))

#############################

# reclassify non-forest to NA
fao.reclass <- ifel(fao.proj == 0, NA, fao.proj)
# reclassify all forest to 1
fao.reclass2 <- ifel(!is.na(fao.reclass), 1, fao.reclass)

# invert the above mask because we want areas that AREN'T forest
fao.reclass.inverse <- ifel(!is.na(fao.reclass2), 0, 1)
# multiply for areas to remove any potential overlap
Sh.mask <- xnum.Sh * fao.reclass.inverse

#create NA

Sh.mask.final <- ifel(Sh.mask == 0, NA, Sh.mask)

plot(Sh.mask.final)
##############################################################################################
# Write the rasters to the correct folders
# shrub layer(s)
terra::writeRaster(Sh.mask.final, paste(out.dir, "Sh_30m.tif", sep = ""), overwrite = T)
# combined layer(s)

##############################################################################################

# aggregate things
Sh.mask.final.agg <- terra::aggregate(Sh.mask.final, 100/res(Sh.mask.final)[1], cores = 12)

##############################################################################################
# Write the aggregated rasters to the correct folders

terra::writeRaster(Sh.mask.final.agg, paste(out.dir, "Sh_90m.tif", sep = ""), overwrite = T)
