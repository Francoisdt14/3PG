library(dplyr)
library(data.table)
library(terra)
library(stringr)

# Generate a list of CSV files
#csv_files <- list.files("D:/climate/Future/M_9S/", pattern = ".csv$", full.names = TRUE, recursive = TRUE)
csv_files <- list.files("D:/climate/Future/M_18S/", pattern = ".csv$", full.names = TRUE, recursive = TRUE)

# Only do it for a few variables if you don't want to include all variables
var.list <- fread("D:/climate/M_9S/climateNA_vars_subset.csv", header = FALSE) %>% pull(V1)

# var.list stays same

# Read and process each CSV file
for (csv_file in csv_files) {
    print(paste("Processing file:", csv_file))

    # Extract the last 5 characters from the CSV filename
    Y_S <- substr(basename(csv_file), nchar(basename(csv_file)) - 8, nchar(basename(csv_file)) - 4)

    # Create the folder for output if it doesn't exist
    output_folder <- file.path("D:/climate/Future/M_18S/tif_1km", Y_S) # change to D:/climate/Future/M_18S/tif_1km
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }

    # Read in CSV
    dt <- fread(csv_file) %>% dplyr::select(Latitude, Longitude, all_of(var.list))
    var.list2 <- names(dt)[4:ncol(dt)] # subset out the lat long, etc

    r <- rast("D:/climate/M_18S/Study_Area_M_18S_1km.tif") # D:/climate/M_9S/Study_Area_M_9S_1km.tif

    for(v in var.list2){
        print(v)
        pts = dplyr::select(dt, Longitude, Latitude, all_of(v)) %>%
            vect(geom = c("Longitude", "Latitude"), crs = "epsg:4326") # double check EPSG ...9S = 32609 18S = epsg:32618

        r2 = rasterize(pts, r, field = v)

        # Output the file
        output_path <- file.path(output_folder, paste(v, ".tif", sep = ""))
        writeRaster(r2, output_path, overwrite = TRUE)
        terra::tmpFiles(remove = TRUE)
    }
}


#######################

# Path to the original 30m DEM
#f.dem30 <- "D:/climate/M_9S/dem_crop_M_9S.tif"
f.dem30 <- "D:/climate/M_18S/dem_crop_M_18S.tif"

# Path to the tif_1km folder containing subfolders
#input_folder <- "D:/climate/Future/M_9S/tif_1km"
input_folder <- "D:/climate/Future/M_18S/tif_1km"

# Path to the output folder for 30m processed tifs
#output_folder <- "D:/climate/Future/M_9S/tif_30m"
output_folder <- "D:/climate/Future/M_18S/tif_30m"

# List all subfolders
subfolders <- list.dirs(input_folder, full.names = FALSE, recursive = FALSE)

# Loop through subfolders
for (subfolder in subfolders) {
    print(paste("Processing subfolder:", subfolder))

    # List tif files in the current subfolder
    tif_files <- list.files(file.path(input_folder, subfolder), full.names = TRUE, pattern = ".tif$")

    # Create corresponding subfolder in the output folder if it doesn't exist
    output_subfolder <- file.path(output_folder, subfolder)
    if (!dir.exists(output_subfolder)) {
        dir.create(output_subfolder, recursive = TRUE)
    }

    num.valid <- rast(f.dem30) %>% global("notNA") %>% as.numeric()

    for (f in tif_files) {
        cat(paste0("\nOn file ", which(tif_files == f), "/", length(tif_files)))

        f.out <- str_replace(f, "tif_1km", "tif_30m")

        if (file.exists(f.out)) {
            cat(" -- file already exists")
            next
        }

        # Only run if you haven't before
        if (!file.exists(f.out)) {
            t1 <- Sys.time()

            blank.r <- rast(f.dem30)

            r2 <- rast(f) %>%
                project(crs(blank.r)) %>%
                focal(w = 5, fun = "mean", na.policy = "only", na.rm = TRUE) %>%
                resample(blank.r, method = "cubicspline") %>%
                mask(blank.r)

            n.current <- global(r2, "notNA") %>% as.numeric()

            if (n.current != num.valid) {
                cat(" -- not the same as target")
                next
            }

            writeRaster(r2, f.out, overwrite = TRUE)
            difftime(Sys.time(), t1, units = "secs") %>% as.numeric() %>% round(2) %>% paste0(" -- done in ", . , " seconds") %>% cat()
        }

        terra::tmpFiles(remove = TRUE)
    }
}


###########################


# Parallel



library(dplyr)
library(data.table)
library(terra)
library(stringr)
library(doParallel)

# Set the number of cores for parallel processing
num_cores <- 2  # Adjust the number of cores as needed

# Set up parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Load required packages within the parallel environment
clusterEvalQ(cl, {library(dplyr);library(data.table);library(terra);library(stringr)})

# Path to the original 30m DEM
f.dem30 <- "D:/climate/M_18S/dem_crop_M_18S.tif"

# Path to the tif_1km folder containing subfolders
input_folder <- "D:/climate/Future/M_18S/tif_1km"

# Path to the output folder for 30m processed tifs
output_folder <- "D:/climate/Future/M_18S/tif_30m"

# List all subfolders
subfolders <- list.dirs(input_folder, full.names = FALSE, recursive = FALSE)

# Function to process a single TIFF file
process_tif <- function(f, f.dem30, num_valid, output_folder) {
    cat(paste0("\nProcessing file: ", f))

    f.out <- str_replace(f, "tif_1km", "tif_30m")

    if (file.exists(f.out)) {
        cat(" -- file already exists")
        return(NULL)
    }

    t1 <- Sys.time()

    blank.r <- rast(f.dem30)

    r2 <- rast(f) %>%
        project(crs(blank.r)) %>%
        focal(w = 5, fun = "mean", na.policy = "only", na.rm = TRUE) %>%
        resample(blank.r, method = "cubicspline") %>%
        mask(blank.r)

    n.current <- global(r2, "notNA") %>% as.numeric()

    if (n.current != num_valid) {
        cat(" -- not the same as target")
        return(NULL)
    }

    writeRaster(r2, f.out, overwrite = TRUE)
    difftime(Sys.time(), t1, units = "secs") %>% as.numeric() %>% round(2) %>%
        paste0(" -- done in ", ., " seconds") %>% cat()

    terra::tmpFiles(remove = TRUE)
}

# Loop through subfolders
for (subfolder in subfolders) {
    print(paste("Processing subfolder:", subfolder))

    # List tif files in the current subfolder
    tif_files <- list.files(file.path(input_folder, subfolder), full.names = TRUE, pattern = ".tif$")

    # Create corresponding subfolder in the output folder if it doesn't exist
    output_subfolder <- file.path(output_folder, subfolder)
    if (!dir.exists(output_subfolder)) {
        dir.create(output_subfolder, recursive = TRUE)
    }

    num_valid <- rast(f.dem30) %>% global("notNA") %>% as.numeric()

    # Apply the process_tif function to each TIFF file in parallel
    tif_results <- foreach(f = tif_files, .combine = 'c') %dopar% {
        process_tif(f, f.dem30, num_valid, output_folder)
    }

    # End of loop iteration
}

# Stop the parallel backend
stopCluster(cl)
