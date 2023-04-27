# Creating a raster of the outputs


source("R/lib.R")

#df.424 <- result_df
#df.425 <- result_df
# df.426 <- result_df
#df.455 <- result_df
#df.456 <- result_df
#df.457 <- result_df
#df.486 <- result_df
#df.487 <- result_df
#df.488 <- result_df

df <- read.csv("D:/BP_Layers/outputs/crops/dataframes/df.486.csv")

#colnames(lai.df.033)[1]  <- "LAI"    # change column name for x column

## making a large raster

# mask_crop = rast("D:/BP_Layers/outputs/tree_mask.tif")
# boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")
# k = number of box we are on (424 here)
k = 486

polygon <- boxes.v[k, ]
mini.r <-  crop(mask_crop, polygon)

n.pix = global(mini.r, "notNA") %>% as.numeric()

r.id = mini.r
r.id[!is.na(r.id)] <- 1:n.pix #npp.df
plot(r.id)

#dt = values(r.id, na.rm = T) %>% as.data.table()

# Read in blank raster
#r = rast()

# How many valid pixels?
num.valid = global(r.id, "notNA") %>% as.numeric()


# Read in output (NPP or whatever) -> change this to the relevant output
vals = df %>% pull("npp")

# This should be true
length(vals) == num.valid

# Fill in the blank raster with the output pixels
mini.r[!is.na(mini.r)] <- vals

npp.487.rast <- mini.r

#writeRaster(lai.rast, "...crops/lai_034.tif")

plot(npp.487.rast)

# combining groups of tiles :

# fl = file.list
# terra::vrt(fl)
# terra::mosaic

group.of.9 <- terra::vrt(fl)
writeRaster(group.of.9)


####################################################################

library(terra)

rast.basal_area425 <- rast("D:/BP_Layers/outputs/crops/output_rasters/basal/basal_area425.tif")
plot(rast.basal_area425)

rast.dbh.487 <- rast("D:/BP_Layers/outputs/crops/output_rasters/dbh487.tif")
plot(rast.dbh.487)


plot(mask_crop, ext = ext(boxes.v), col = "darkgreen")
#plot(rast_m, ext = ext(boxes.v))
plot(boxes.v, add = T)
text(boxes.v, boxes.v$name)



crs(raster_cropped) <- crs(raster_reference)

# Check if the CRS of the cropped raster matches the reference raster
identical(crs(mask_crop), crs(mini.r))


