library(terra)

master.rast <- rast("D:/BP_Layers/U_13N/3PG_flt/6_90m_flt/Forest_Age_2019.flt")
folder <- "D:/BP_Layers/U_13N/3PG_flt/6_90m_flt_other_inputs/"
input.files <- list.files(folder, pattern = ".flt$", full.names = TRUE)

# Loop through each raster input
for (file in input.files) {

    # Load the test raster
    test.rast <- rast(file)

    # Compare geometries
    geom_match <- compareGeom(master.rast, test.rast)

    # Check if geometries match
    if (geom_match) {
        # If TRUE, go to the next file
        cat("geom match for file:", file, "\n")
    } else {
        # If FALSE, print filename and move on to the next file
        cat("!Geometries DO NOT match for file:", file, "\n")
    }
}




#
raster <- rast("I:/data_2024_01_12/U_18S/S3/Y4_output/ws.flt")

df <- as.data.frame(raster)
