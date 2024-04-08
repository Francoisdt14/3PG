library(terra)
library(tidyverse)


bs <- rast("D:/BP_Layers/M_18S/analysis/s2_Sh_mask.tif")

rast.2 <- rast("I:/data_2024_03_22_deciduous/M_18S/S2_dec/Y3_output/ws205007.flt")

rast.mask <- rast('D:/BP_Layers/M_18S/landcover/Sh_90m.tif')

crs(rast.2) <- crs(rast.mask)

decid <- terra::mask(rast.2, rast.mask)

hist(decid)
hist(bs)

summary_decid <- global(decid, c("mean", "sum", "min", "max", "sd"), na.rm = TRUE)

summary_bs <- global(bs, c("mean", "sum", "min", "max", "sd"), na.rm = TRUE)

rast.mask.1 <- rast('D:/BP_Layers/M_18S/landcover/Sh_30m.tif')
