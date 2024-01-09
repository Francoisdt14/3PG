library(terra)
# stem number raster

test <- rast("D:/BP_Layers/U_15S/3PG_flt/4_30m_inputs_all/Forest_Age_2019_withNA.tif")
plot(test)
test[!is.na(test)] <- 4000
test[is.na(test)] <- 1650

#test
plot(test, col = viridis(2))

writeRaster(test, "D:/BP_Layers/U_15S/3PG_flt/4_30m_inputs_all/stemno_init.tif")
writeRaster(test, "D:/BP_Layers/U_15S/3PG_flt/6_30m_flt/stemno_init.flt", datatype = "FLT4S", overwrite = TRUE)

test.90 <- rast("D:/BP_Layers/U_15S/3PG_flt/5_90m_inputs_all/Forest_Age_2019_withNA.tif")
plot(test.90)

test.90[!is.na(test.90)] <- 4000
test.90[is.na(test.90)] <- 1650

plot(test.90, col = viridis(2))

writeRaster(test.90, "D:/BP_Layers/U_15S/3PG_flt/5_90m_inputs_all/stemno_init.tif")
writeRaster(test.90, "D:/BP_Layers/U_15S/3PG_flt/6_90m_flt_other_inputs/stemno_init.flt", datatype = "FLT4S", overwrite = TRUE)



