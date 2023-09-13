library(terra)
library(tidyverse)

# year trees were planted
planted <- rast("D:/3PG_Cplusplus/data_100/forest_age_2019.flt")

# age in 2019
age.2019 <- 2019 - planted

# age in 2040
age.2040 <- age.2019 + 21

#writeRaster(age.2040, "D:/3PG_Cplusplus/future_forest_age_input_TEST/age2040.flt", overwrite = TRUE)

# Agtes in 2060, 2080, 20100
age.2060 <- age.2019 + 41
age.2080 <- age.2019 + 61
age.2100 <- age.2019 + 81

# biomass inputs into the future model
wf.input <- rast('D:/3PG_Cplusplus/future_forest_age_input_TEST/wf.flt')
ws.input <- rast('D:/3PG_Cplusplus/future_forest_age_input_TEST/ws.flt')
wr.input <- rast('D:/3PG_Cplusplus/future_forest_age_input_TEST/wr.flt')

global(ws.input, c("max", "min", "mean"), na.rm = T)

# first output from the future model
wf.2020 <- rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/wf202007.flt")
ws.2020  <-  rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/ws202007.flt")
wr.2020 <-  rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/wr202007.flt")

global(ws.2020, c("max", "min", "mean"), na.rm = T)

# 2030 output from the future model
wf.2030 <- rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/wf203007.flt")
ws.2030  <-  rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/ws203007.flt")
wr.2030 <-  rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/wr203007.flt")

global(ws.2030, c("max", "min", "mean"), na.rm = T)

par(mfrow = c(3, 1))
plot(ws.input, main = "stem biomass input")
plot(ws.2020, main = "stem biomass 2020")
plot(ws.2030, main = "stem biomass 2030")

par(mfrow = c(1, 1))

##########

planted.df <- as.data.frame(planted)

planted.df$focal_mean <- as.integer(round(planted.df$focal_mean))

unique_values <- unique(planted.df$focal_mean)

num_unique_values <- length(unique_values)




test <- rast("D:/3PG_Cplusplus/future_forest_age_input_TEST/ws202007.flt")
plot(test)
#global(test, c("max", "min", "mean"), na.rm = T)
crs(test) <- crs(planted)

#writeRaster(test, "D:/3PG_Cplusplus/future_forest_age_input_TEST/stemno202007.flt", datatype = "FLT4S")


test <- rast("D:/3PG_Cplusplus/future_forest_age_input_TEST/ws202007.flt")

test2 <- rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/ws202007.flt")


plot(test2)

global(test, c("max", "min", "mean"), na.rm = T)
global(test2, c("max", "min", "mean"), na.rm = T)




#############################################


wf <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/wf.flt")

crs(wf) <- crs(planted)

wr <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/wr.flt")

crs(wr) <- crs(planted)

ws <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/ws.flt")

crs(ws) <- crs(planted)

stemno <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/stemno.flt")
crs(stemno) <- crs(planted)

asw <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/asw.flt")
crs(asw) <- crs(planted)

writeRaster(ws, "D:/3PG_Cplusplus/future_forest_age_input_TEST/Y4_S2/ws.flt")

######################################


library(stringr)

# Set the directory where the .hdr and .flt files are located

#directory <- "Y:/Francois/flt_test_100_noNA"
directory <- "D:/3PG_Cplusplus/future_forest_age_input_TEST/Y4_S2"
# Get the list of .hdr files in the directory
hdr_files <- list.files(directory, pattern = "\\.hdr$", full.names = TRUE)

# very important to write this correctly depending on raster size!!

# Process each .hdr file
for (hdr_file in hdr_files) {
  # Define the new content for the .hdr file
  new_content <- c(
    "NROWS          3249", # 90m = 3249 , 30m = 9745
    "NCOLS          3249", # 90m 3249  , 30m 9746
    "xllcenter         403650.448601884", # 90m = 403650.448601884 , 30m = 403619.663820003
    "yllcenter         6312667.21413766", # 90m = 6312667.21413766" , 30m = 6312636.42935578"
    "cellsize           92.35435", # 90m = 92.35435 , 30m = 30.78478
    "nodata_value -9999.000000",
    "byteorder lsbfirst"
  )
  
  # Write the new content to the .hdr file, overwriting the existing contents
  writeLines(new_content, hdr_file)
}




global(ws, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)


###############################################################################
wf <- rast("D:/3PG_Cplusplus/_delete/new/wf202007.flt")
ws <- rast("D:/3PG_Cplusplus/_delete/new/ws202007.flt")
tot <- wf + ws

crs(tot) <- crs(planted)

planted_NA <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/Forest_Age_2019_withNA.flt")

tot_mask <- mask(tot, planted_NA)

writeRaster(tot_mask, "D:/3PG_Cplusplus/_delete/new/biom_2020.tif")


current <- rast("D:/3PG_Cplusplus/_delete/new/biom_2100_currentT.tif")
S2 <- rast("D:/3PG_Cplusplus/_delete/new/biom_2100_s2.tif")

diff <- S2 - current

diff[diff<10] <- NA

writeRaster(diff, "D:/3PG_Cplusplus/_delete/new/S2_current_DIFF_10plus.tif")

landcover <- rast("D:/BP_Layers/M_9S/landcover/landcover_90m.tif")

x <- droplevels(landcover) # get rid of un-necessary data
xnum <- as.numeric(x) # convert the raster to numeric - need to keep an eye on which codes we want
xnum[!xnum %in% c(81, 210, 220, 230, 40, 50, 100)] <- NA # here we want to keep the forested landcover types, so set everything that isn't to NA
xnum[!is.na(xnum)] <- 1 # set the forested landcovers to 1

#plot(xnum, col = "darkgreen")
mask_crop <- terra::crop(xnum, landcover)

#writeRaster(mask_crop, "D:/BP_Layers/M_9S/landcover/tree_plus_40_50_100_90m.tif", datatype = "INT1U", overwrite = T) # write this mask as it will be used moving forward!


landcover_1 <- droplevels(landcover) 
landcover_1 <- as.numeric(landcover_1)
landcover_1[!landcover_1 %in% c(81, 210, 220, 230)] <- NA # here we want to keep the forested landcover types, so set everything that isn't to NA
landcover_1[!is.na(landcover_1)] <- 1

plot(landcover_1)
#writeRaster(landcover_1, "D:/BP_Layers/M_9S/landcover/tree_mask_90m.tif", datatype = "INT1U", overwrite = T) # write this mask as it will be used moving forward!



no.lim <- rast("D:/3PG_Cplusplus/_delete/new/biom_2100_currentT_unmasked.tif")

no.lim.shrub <- mask(no.lim, mask_crop)

# Load your raster data (replace with actual file paths)
raster_base <- rast("D:/3PG_Cplusplus/data_100/hillshade.flt")

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(tot_mask, add = TRUE, alpha = 0.9, col = viridis(100), title = "Treed Areas Only")

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(no.lim.shrub, add = TRUE, alpha = 0.9, col = viridis(100), title = "Treed and Shrubland Areas")

# Add a title to the plot
title("Treed, Shrubland, Bryoid, and Herb Areas")

# Create a legend
#legend("topright", legend = "tDM ha-1", bty = "n")


