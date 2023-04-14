# Creating a raster of the outputs


source("R/lib.R")

dbh.df.044 <- result_df
#colnames(lai.df.033)[1]  <- "LAI"    # change column name for x column

## making a large raster

# mask_crop = rast("D:/BP_Layers/outputs/tree_mask.tif")
# k = number of box we are on (44 here)

polygon <- boxes.v[k, ]
r <-  crop(mask_crop, polygon)

n.pix = global(r, "notNA") %>% as.numeric()

r.id = r
r.id[!is.na(r.id)] <- 1:n.pix #npp.df
plot(r.id)

#dt = values(r.id, na.rm = T) %>% as.data.table()

# Read in blank raster
#r = rast()

# How many valid pixels?
num.valid = global(r.id, "notNA") %>% as.numeric()


# Read in output (NPP or whatever) -> change this to the relevant output
vals = dbh.df.044 %>% pull("dbh")

# This should be true
length(vals) == num.valid

# Fill in the blank raster with the output pixels
r[!is.na(r)] <- vals

dbh.df.044.rast <- r

#writeRaster(lai.rast, "...crops/lai_034.tif")

plot(dbh.df.044.rast)
