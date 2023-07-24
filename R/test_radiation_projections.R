# Read the shapefile

#shapefile of study area
study_area <- vect("D:/Landcover/francois2/Shapefiles/Study_Area_M_nineS.shp")

#raster of interest
rast1 <- rast("D:/Radiation/1km_radiation/NACID_rswd_mon_norm_1971to2000_si_hist_v1_1.tif")

#radiation data of study area with NAs
rad <- rast("D:/Radiation/30m_crop_align/M_nineS/NACID_rswd_mon_norm_1971to2000_si_hist_v1_1.tif")

#project the study area shapefile 
projected_vector <- terra::project(study_area, "+proj=lcc +lat_0=40 +lon_0=-100 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#crop the large raster using the projected study area 
crop <- terra::crop(rast1, projected_vector)

#this is what every single raster is aligned to 
tree.mask <- rast("D:/BP_Layers/outputs/tree_mask.tif")

crop_proj <- terra::project(crop, tree.mask)


ext_mask <- ext(tree.mask)
ext_crop <- ext(crop_proj)


########


plot(ext_mask, col = 'red')
plot(ext_crop, col = 'blue', add = T)
plot(ext_mask, col = 'red')
plot(crop_proj, add = T)


#####

# Loop for radiation ...
####################################
# 

fl <- ("D:/Radiation/1km_radiation/")
out.dir <- ("D:/Radiation/test_crop_project/M_nineS")


#### CHECK THIS AND ADJUST FOR SPECIES CODE - SOLUTION SEEMS TO BE AS FACTOR
# PRE-PROJECTION! - NEED TO MAKE SURE THAT THE RASTER IS CLASSIFIED AS FACTORS

for (i in 1:length(fl)) {
  
    # load in big raster
  r <- terra::rast(fl[i])
  
  #crop the large raster using the projected study area 
  crop <- terra::crop(r, projected_vector)
  
  crop_proj <- terra::project(crop, tree.mask)
  
  ## IS THIS NECESSARY??
  
  ###
  # align
  ra <- resample(crop_proj, tree.mask, method = "near")
  ra <- focal(ra, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)
  
  #mask
  rm <- terra::crop(ra, tree.mask)
  
  ###
  
  file.name <- basename(fl[i])
  file.name <- gsub('.tif','',file.name)
  
  # write
  terra::writeRaster(rm, paste(outdir, file.name, ".tif", sep = ""), overwrite = T)
  
  }
