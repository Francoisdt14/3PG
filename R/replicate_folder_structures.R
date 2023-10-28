# Define the source and destination folders
source_folder <- "D:/BP_Layers/M_18S/3PG_flt"
destination_folder <- "D:/BP_Layers/M_11S/3PG_flt"


# Create a function to replicate the folder structure
replicate_folder_structure <- function(src, dest) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    subfolders <- list.dirs(src, full.names = FALSE, recursive = FALSE)
    for (subfolder in subfolders) {
        subfolder_path <- file.path(dest, subfolder)
        dir.create(subfolder_path, showWarnings = FALSE)
    }
}

# Call the function to replicate the folder structure
replicate_folder_structure(source_folder, destination_folder)
