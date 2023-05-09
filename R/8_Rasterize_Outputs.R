# Creating a raster of the outputs


source("R/lib.R")


#df.test <- result_df
#write.csv(df.424.50y, file = "D:/BP_Layers/outputs/crops/npp/df.424.50y.csv")
df.test <- read.csv("D:/BP_Layers/outputs/crops/dataframes/454.csv")
#df.test2 <- read.csv("D:/BP_Layers/outputs/crops/dataframes/516.csv")

lat.test <- read.csv("D:/BP_Layers/outputs/crops/lat/515.csv") %>% na.omit
inputs.test <- read.csv("D:/BP_Layers/outputs/crops/climate/454.csv") %>% na.omit

#colnames(lai.df.033)[1]  <- "LAI"    # change column name for x column

## making a large raster

# mask_crop = rast("D:/BP_Layers/outputs/tree_mask.tif")


# k = number of box we are on (424 here)

k = 454

polygon <- boxes.v[k, ]
mini.r <-  crop(mask_crop, polygon)

n.pix = global(mini.r, "notNA") %>% as.numeric()

r.id = mini.r
r.id[!is.na(r.id)] <- 1:n.pix #npp.df
plot(r.id)

#dt = values(r.id, na.rm = T) %>% as.data.table()

# Read in blank raster
#r = rast()

# How many valid pixels?
num.valid = global(r.id, "notNA") %>% as.numeric()


# Read in output (NPP or whatever) -> change this to the relevant output
vals = df.test %>% pull("npp")

# This should be true
length(vals) == num.valid

# Fill in the blank raster with the output pixels
mini.r[!is.na(mini.r)] <- vals

plot(mini.r)

df.424.50y.rast <- mini.r

#writeRaster(lai.rast, "...crops/lai_034.tif")

plot(df.424.50y.rast)

terra::writeRaster(dbh.488.rast, filename = "D:/BP_Layers/outputs/crops/output_rasters/dbh488.tif")






#################################
# Loop through this to make our rasters

# mask_crop <- rast("D:/BP_Layers/outputs/tree_mask.tif")
# boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")

csv_folder <- "D:/BP_Layers/outputs/crops/dataframes_50yr"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = csv_folder, pattern = "*.csv")


keyword <- "lai"

# where are things saved?
target_folder <- paste0("D:/BP_Layers/outputs/crops/output_rasters_50yr/", keyword)


# Loop through each CSV file
for (file in csv_files) {

    csv_name <- gsub(".csv", "", file)
    filename <- paste0(keyword,csv_name,".tif") # check ----
    filepath <- file.path(target_folder, filename)

    if (file.exists(filepath)) {
        cat("Skipping file", filename, "because it already exists.\n")
        next  # move to next iteration of the loop
    }

    # Read in the CSV file
    df <- read.csv(paste(csv_folder, file, sep = "/"))

    # Extract the numbers from the file name
    num.box <- gsub("[^0-9]", "", file)
    num.box <- as.numeric(num.box)

    # Perform your function using the extracted numbers

    # Do something with the output
    # ...
    polygon <- boxes.v[boxes.v$name == num.box, ]
    mini.r <-  crop(mask_crop, polygon)

    crs(mini.r) <- crs(mask_crop)

    #n.pix = global(mini.r, "notNA") %>% as.numeric()

    #r.id = mini.r
    #r.id[!is.na(r.id)] <- 1:n.pix #npp.df
    #plot(r.id)

    #dt = values(r.id, na.rm = T) %>% as.data.table()

    #r = rast()
    # How many valid pixels?
    #num.valid = global(r.id, "notNA") %>% as.numeric()

    # Read in output (NPP or whatever) -> change this to the relevant output
    vals = df %>% pull(keyword)

    # This should be true
    #length(vals) == num.valid

    # Fill in the blank raster with the output pixels
    mini.r[!is.na(mini.r)] <- vals

    # Add the number to the raster file name
    rast_name <- paste0(target_folder,"/",keyword, num.box, ".tif")

    # Write the raster to disk
    terra::writeRaster(mini.r, filename = rast_name, overwrite=T)
    rm(mini.r)
}


###########################

# combining groups of tiles :
raster_folder <- paste0("D:/BP_Layers/outputs/crops/output_rasters_50yr/", keyword)
# Get a list of all CSV files in the folder
raster_files <- list.files(path = raster_folder, pattern = "*.tif")
# terra::mosaic

group.of.49 <- terra::vrt(paste(raster_folder, raster_files, sep = "/"))

plot(group.of.49, col=viridis(100),  main="Leaf Area Index")

writeRaster(group.of.49, filename = paste0("D:/BP_Layers/outputs/crops/output_rasters_50yr/",keyword, "_","49",".tif"))

##basal454 <- rast("D:/BP_Layers/outputs/crops/output_rasters/basal_area/basal_area454.tif")
plot(basal454)