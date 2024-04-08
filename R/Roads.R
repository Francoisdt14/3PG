# Clipping Road Buffers
library(terra)
# areas

#area = "M_18S"
#area = "M_11S"
area = "U_18S"
#area = "U_15S"
#area = "U_13N"
#area = "M_9S"

## test for study area M 18
# load in roads - only once

#roads <- vect("D:/Roads/lrnf000r23a_e.shp")

# load in study area
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_eighteenS.shp')
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_elevenS.shp')
vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_U_eighteenS.shp')
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_U_fifteenS.shp')
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_U_thirteenN.shp')
#vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_nineS.shp')

# crop roads to study area
roads.study <- crop(roads, vect.study)
# create a roads buffer
roads.study.buffer <- terra::buffer(roads.study, 1000)
#roads.study.buffer <- terra::buffer(roads.study, 50000)

# read in study area mask
study.mask <- rast(paste("D:/BP_Layers/", area, "/landcover/Sh_90m.tif", sep = ""))

# project
roads.study.buffer.proj <- terra::project(roads.study.buffer, study.mask)
# aggregate
roads.study.buffer.proj.agg <- terra::aggregate(roads.study.buffer.proj, dissolve = TRUE)
# crop back to extent of mask if necessary
#roads.study.buffer.proj.agg.crop <- terra::crop(roads.study.buffer.proj.agg, study.mask)

# mask to the study area mask
study.mask.road <- terra::mask(study.mask, roads.study.buffer.proj.agg)

plot(roads.study.buffer.proj.agg.crop)
plot(study.mask.road, add = T, col = "red")

study.mask.road



writeRaster(study.mask.road, paste("D:/BP_Layers/", area, "/landcover/Sh_90m_road_1km.tif", sep = ""))

#########
#########


vect.study <- vect('D:/PostDoc_Maps_Original/Projected_Layers/Study_Area_M_elevenS.shp')

#roads <- vect("D:/Roads/lrnf000r23a_e.shp")

roads.study <- crop(roads, vect.study)

roads.study.buffer <- terra::buffer(roads.study, 1000)

study.mask <- rast("D:/BP_Layers/M_11S/landcover/Sh_90m.tif")

roads.study.buffer.proj <- project(roads.study.buffer, study.mask)

roads.study.buffer.proj.agg <- terra::aggregate(roads.study.buffer.proj, dissolve = TRUE)

study.mask.road <- terra::mask(study.mask, roads.study.buffer.proj.agg)


