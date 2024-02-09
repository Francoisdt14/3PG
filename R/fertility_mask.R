#########################
# Fertility
# Create a mask where we can 'boost' fertility for a given area

# get age WITH NA

# year trees were planted
planted.NA <- rast("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all/Forest_Age_2019_withNA.tif")

planted.NA <- rast("D:/BP_Layers/U_13N/3PG_flt/6_90m_flt/Forest_Age_2019_wthNA.flt")


# change all values to 0 (except NA) and NAs to 1:
# this is where we should boost fertility to 1 in 2025
modified_raster <- ifel(!is.na(planted.NA), 0, 1)
plot(modified_raster)

# Now read in fertility:

#fertility <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/soil_carbon_aligned_scaled_noNA.flt")
fertility <- rast("D:/BP_Layers/U_13N/3PG_flt/6_90m_flt/soil_carbon_aligned_scaled_noNA_90m.flt")

# Replace fertility values where there is a 1 in the previous raster
result_raster <- ifel(modified_raster == 1, 1, fertility)

plot(result_raster)

result_raster_2 <- clamp(result_raster, lower=-Inf, upper=1)

plot(result_raster_2)

writeRaster(result_raster_2, "D:/BP_Layers/U_13N/3PG_flt/6_90m_flt_other_inputs/boosted_fertility.flt", datatype = "FLT4S")
