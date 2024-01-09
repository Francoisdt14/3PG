library(terra)
library(tictoc)
library(tidyverse)

# Single test to see if the project/crop works - it does in this order!
# Align any layers to the tree.mask - in the case of U_13N it looks like the files are already aligned!

#this is what every single raster is aligned to
#tree.mask <- rast("D:/BP_Layers/U_18S/tree_mask.tif")

#this is what every single raster is aligned to
tree.mask <- rast("D:/BP_Layers/U_13N/tree_mask.tif")
# need to fill the NA's so that we can make sure we have full climate layers!
tree.mask[is.na(tree.mask)] <- 1


############################ If we need to align any layers - do it here:
scen  <- "Y2_S1"

#folder with data
fl <- paste0("D:/climate/Future/U_13N/tif_30m/", scen)
#list of files in folder
input.files <- list.files(fl, pattern = ".tif$", full.names = TRUE)
#output folder
out.dir <- paste0("D:/BP_Layers/M_9S/3PG_flt/2_future_climate_align/", scen, "/") # don't forget the forward /

# Loop through each raster input
for (i in 1:length(input.files)) {

  # load in big raster
  r <- terra::rast(input.files[i])

  ## IS THIS NECESSARY?? - NOT FOR RADIATION
  ###
  # align
  ra <- resample(r, tree.mask, method = "near", threads = T)
  ra <- focal(ra, w = 3, fun = "mean", na.policy = "only", na.rm = T, expand = T)

  #mask
  rm <- terra::crop(ra, tree.mask)

  ###

  file.name <- basename(input.files[i])
  file.name <- gsub('.tif','',file.name)

  # write
  terra::writeRaster(rm, paste(out.dir, file.name, ".tif", sep = ""), overwrite = T)

}


###############################################################################################################################################
# Creating Frost Free Days

# List of scenarios
scenarios <- c("Y2_S1", "Y2_S2", "Y2_S3", "Y3_S1", "Y3_S2", "Y3_S3", "Y4_S1", "Y4_S2", "Y4_S3", "Y5_S1", "Y5_S2", "Y5_S3")

# Loop through scenarios
for (scen in scenarios) {
    # Folder with data
    fp <- paste0("D:/climate/Future/U_13N/tif_30m/", scen)

    # Check if the output folder already exists, if yes, skip to the next scenario
    output_folder <- paste0("D:/BP_Layers/U_13N/3PG_flt/3_future_frost/", scen, "/")
    if (file.exists(output_folder)) {
        cat("Output folder", output_folder, "already exists. Skipping...\n")
        next  # Skip to the next scenario
    }

    # List of files in folder
    tiffs <- list.files(fp, pattern = ".tif$", full.names = TRUE, recursive = TRUE)

    # Create the output folder
    dir.create(output_folder, showWarnings = FALSE)

    # Loop through the list of files
    for (file in tiffs) {
        # Check if the filename contains 'nffd'
        if (grepl("nffd", basename(file), ignore.case = TRUE)) {

            # Read the raster
            raster <- rast(file)

            # Extract the month number from the filename - 5th and 6th number
            month_number <- as.integer(substr(basename(file), 5, 6))

            # Set the correct number of days based on the month
            if (month_number %in% c(04, 06, 09, 11)) {
                # April, June, September, November have 30 days
                num_days <- 30
            } else if (month_number == 02) {
                # February has 28 days
                num_days <- 28
            } else {
                # All other months have 31 days
                num_days <- 31
            }

            # Perform the processing on the raster (31 - raster and round to nearest full day)
            processed_raster <- round(num_days - raster)

            # Get the filename without the full path
            filename <- basename(file)

            # Create the output path for the processed raster
            output_path <- file.path(output_folder, filename)

            # Save the processed raster to the output folder
            writeRaster(processed_raster, output_path)
        }
    }

    cat("Converted Frost For:", scen, "\n")
}

####
# Aggregate - do FUTURE FROST FIRST - THEN OTHER FOLDERS
# 2 places to switch folders - 'input_folder' and 'out_path'
# List of scenarios
scenarios <- c("Y2_S1", "Y2_S2", "Y2_S3", "Y3_S1", "Y3_S2", "Y3_S3", "Y4_S1", "Y4_S2", "Y4_S3", "Y5_S1", "Y5_S2", "Y5_S3")

# Loop through scenarios
for (scen in scenarios) {
    # Input folder

    #input_folder <- paste0("D:/BP_Layers/U_13N/3PG_flt/3_future_frost/", scen)
    input_folder <- paste0("D:/climate/Future/U_13N/tif_30m/", scen)

    # List of files in the input folder
    tiffs <- list.files(input_folder, pattern = ".tif$", full.names = TRUE, recursive = TRUE)

    # Output folder for processed rasters
    output_folder <- paste0("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_future/", scen)

    # Check if the output folder exists, if not, create it
    if (!file.exists(output_folder)) {
        dir.create(output_folder, showWarnings = FALSE)
    }

    # Loop through the list of files
    for (t in tiffs) {

        # Skip files starting with 'rad'
        if (grepl("^rad", basename(t), ignore.case = TRUE)) {
            cat("Skipping file", t, "as it starts with 'rad'.\n")
            next  # Skip to the next file
        }
        # Create the output path
        #out_path <- str_replace(t, "BP_Layers/U_13N/3PG_flt/3_future_frost", paste0("BP_Layers/U_13N/3PG_flt/5_90m_inputs_future"))
        out_path <- str_replace(t, "D:/climate/Future/U_13N/tif_30m", paste0("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_future"))

        # Check if the output file already exists, if yes, skip to the next file
        if (file.exists(out_path)) {
            cat("Output file", out_path, "already exists. Skipping...\n")
            next  # Skip to the next file
        }

        # Read in the raster
        r <- rast(t) %>% terra::aggregate(., 100 / res(.)[1], cores = 12)

        # Write the processed raster to the output folder
        writeRaster(r, out_path)
    }
    cat("Aggregated:", scen, "\n")
}

######

###
# Compare geom
check.rast <- rast("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all/Tmin03.tif")
check.rast2 <- rast("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all/Forest_Age_2019.tif")
compareGeom(check.rast, check.rast2)

###
# create float files
# List of scenarios
scenarios <- c("Y2_S1", "Y2_S2", "Y2_S3", "Y3_S1", "Y3_S2", "Y3_S3", "Y4_S1", "Y4_S2", "Y4_S3", "Y5_S1", "Y5_S2", "Y5_S3")

# Loop through scenarios
for (scen in scenarios) {
    # Input folder
    input_folder <- paste0("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_future/", scen)

    # List of files in the input folder
    tif_files <- list.files(input_folder, pattern = ".tif$", full.names = TRUE)

    # Output folder for processed rasters
    output_folder <- paste0("D:/BP_Layers/U_13N/3PG_flt/6_90m_flt_future/", scen)

    # Check if the output folder exists, if not, create it
    if (!file.exists(output_folder)) {
        dir.create(output_folder, showWarnings = FALSE)
    }

    # Loop through the list of files
    for (tif_file in tif_files) {

                # Build the output file path by replacing the input directory with the output directory
        output_file <- gsub(input_folder, output_folder, tif_file)

        # Change the file extension to ".flt"
        output_file <- sub("\\.tif$", ".flt", output_file)

        # Check if the output file already exists, if yes, skip to the next file
        if (file.exists(output_file)) {
            cat("Output file", output_file, "already exists. Skipping...\n")
            next  # Skip to the next file
        }

        # Read the TIFF file using terra
        raster <- rast(tif_file)
        raster[is.na(raster)] <- -9999.000000

        # Write the raster in FLT4S format to the output file
        writeRaster(raster, output_file, datatype = "FLT4S", overwrite = TRUE)

    }
    cat("Converted to FLT:", scen, "\n")
}



######################################################################################################################################

# Set the input and output directories
input_dir <- "D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_future/Y2_S1"

output_dir <- "D:/BP_Layers/U_13N/3PG_flt/6_90m_flt_future/Y2_S1"

# Get the list of TIFF files in the input directory
tif_files <- list.files(input_dir, pattern = ".tif$", full.names = TRUE)
# Create the output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE)
# Loop over each TIFF file
for (tif_file in tif_files) {
  # Build the output file path by replacing the input directory with the output directory
  output_file <- gsub(input_dir, output_dir, tif_file)

  # Change the file extension to ".flt"
  output_file <- sub("\\.tif$", ".flt", output_file)

  if (file.exists(output_file)) {
    cat(" -- file already exists")
    next
  }

  # Read the TIFF file using terra
  raster <- rast(tif_file)

  # Write the raster in FLT4S format to the output file
  writeRaster(raster, output_file, datatype = "FLT4S", overwrite = TRUE)

  # Print the file name to track progress
  cat("Converted:", tif_file, "\n")
}

check.rast3 <- rast("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all/PPT09.tif")
################################################################################################################################################

# Create the header files that match the 3PG input:
# An original aligned tif gives us the information
template.rast <- rast("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all/tmin01.tif")

# Number of rows and columns
rows_x <- nrow(template.rast)
rows_y <- ncol(template.rast)

# Resolution of the raster
res_x <- res(template.rast)[1]
res_y <- res(template.rast)[2]

# We want to find the lower left center of the X and Y dimension for the header files:
center_x <- xmin(template.rast) + res_x / 2
center_y <- ymin(template.rast) + res_y / 2

# rewrite HDR files!

library(stringr)

# List of scenarios
scenarios <- c("Y2_S1", "Y2_S2", "Y2_S3", "Y3_S1", "Y3_S2", "Y3_S3", "Y4_S1", "Y4_S2", "Y4_S3", "Y5_S1", "Y5_S2", "Y5_S3")

# Loop through scenarios
for (scen in scenarios) {
    # Input folder
    input_folder <- paste0("D:/BP_Layers/U_13N/3PG_flt/6_90m_flt_future/", scen)

    # Get the list of .hdr files in the directory
    hdr_files <- list.files(input_folder, pattern = "\\.hdr$", full.names = TRUE)
    # very important to write this correctly depending on raster size!!
    # Process each .hdr file

    for (hdr_file in hdr_files) {
        # Define the new content for the .hdr file
        new_content <- c(
            paste("NROWS         ", as.character(rows_x)),
            paste("NCOLS         ", as.character(rows_y)),
            paste("xllcenter        ", as.character(center_x)),
            paste("yllcenter        ", as.character(center_y)),
            paste("cellsize          ", as.character(res_x)),
            "nodata_value -9999.000000",
            "byteorder lsbfirst"
        )

        writeLines(new_content, hdr_file)
    }
    cat("Converted HDR for:", scen, "\n")
}


##################################################################################################################################

# random final checks

check.raster5 <- rast("D:/BP_Layers/U_13N/3PG_flt/6_90m_flt_future/Y5_S1/Tmin09.flt")
check.raster6 <- rast("D:/BP_Layers/U_13N/3PG_flt/6_90m_flt/Forest_Age_2019.flt")

compareGeom(check.raster5, check.raster6)



## We need to amend the .hdr files that are produced as outputs from 3PG:


# Set the path to the folder containing .flt and .hdr files
folder_path <- "D:/3PG_Cplusplus/_delete"  # Replace with the path to your folder

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




