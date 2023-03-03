# Creating a raster of the outputs


source("R/lib.R")

lai.df.033 <- lai.df
colnames(lai.df.033)[1]  <- "LAI"    # change column name for x column


## making a large raster

# mask_crop = rast("D:/BP_Layers/outputs/tree_mask.tif")
# i = number of box we are on (33 here)

polygon <- boxes.v[i, ]
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
vals = lai.df.034 %>% pull("LAI")

# This should be true
length(vals) == num.valid

# Fill in the blank raster with the output pixels
r[!is.na(r)] <- vals

lai.df.033.rast <- r

#writeRaster(lai.rast, "...crops/lai_034.tif")

plot(lai.df.034.rast)
