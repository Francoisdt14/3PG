# Fertility
# Define study areas
study_areas <- c("M_9S", "M_11S", "M_18S", "U_18S", "U_15S", "U_13N") # , "U_13N"

# Loop through study areas
for (area in study_areas) {

    # Set the output directory based on the current study area
    output_dir <- file.path("D:/BP_Layers", area, "3PG_flt/6_90m_flt_other_inputs/")
    # Check if the folder exists, and create it if it doesn't
    if (!file.exists(output_dir)) {
        dir.create(output_dir)
    }

    # # year trees were planted
    planted.NA <- rast(file.path("D:/BP_Layers", area, "3PG_flt/5_90m_inputs_all/Forest_Age_2019_withNA.tif"))

    # change all values to 0 (except NA) and NAs to 1:
    # this is where we should boost fertility to 1 in 2025
    modified_raster <- ifel(!is.na(planted.NA), 0, 1)

    # Now read in fertility:

    fertility <- rast(file.path("D:/BP_Layers", area, "3PG_flt/6_90m_flt/soil_carbon_aligned_scaled_noNA_90m.flt"))

    # Replace fertility values where there is a 1 in the previous raster
    result_raster <- ifel(modified_raster == 1, 1, fertility)

    # Create the file path and name for the output raster
    output_path <- file.path(output_dir, paste("boosted_fertility"))

    # Save the masked raster
    writeRaster(result_raster, filename = paste(output_path, ".tif", sep = ""), overwrite = TRUE)

    cat("Processed Study Area:", area, "\n")
}
