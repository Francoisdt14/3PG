library(stringr)

# Set the directory where the .hdr and .flt files are located

#directory <- "Y:/Francois/flt_test_100_noNA"
directory <- "D:/3PG_Cplusplus/_delete/S1_Y4"
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


# Set the path to the folder containing .flt and .hdr files
folder_path <- "D:/3PG_Cplusplus/_delete/S2_lp_Y2_output"  # Replace with the path to your folder
# List .hdr files in the folder
hdr_files <- list.files(folder_path, pattern = "\\.hdr$", full.names = TRUE)

# Add projection information to each .hdr file
for (hdr_file in hdr_files) {
    hdr_content <- readLines(hdr_file)

    # Add the projection information
    projection_info <- c(
        "projection UTM",
        "zone 9",
        "datum WGS84",
        "units meters"
    )

    # Find the line number with "cellsize" and insert the projection info after it
    cellsize_index <- which(grepl("cellsize", hdr_content))
    hdr_content <- append(hdr_content, projection_info, after = cellsize_index)

    # Write the modified content back to the .hdr file
    writeLines(hdr_content, con = hdr_file)

    cat("Projection information added to", basename(hdr_file), "\n")
}
