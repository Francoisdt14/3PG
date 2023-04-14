# Tile the large input rasters into manageable chunks

source("R/lib.R")

# Masking - again, we are using the tree_mask!

mask_crop <- rast("D:/BP_Layers/outputs/tree_mask.tif")

box.size = 10000 # How big do you want the boxes to be? in m

# Going through and finding the center points, starting at the lower left corner
xv = seq(xmin(mask_crop), xmax(mask_crop), by = box.size)
yv = seq(ymin(mask_crop), ymax(mask_crop), by = box.size)

box.ctrs = expand.grid(xv, yv) %>% as.matrix()

boxes = pbapply(box.ctrs, 1, FUN = function(r){
  x = r[1]
  y = r[2]

  n.name = which(box.ctrs[,1] == x & box.ctrs[,2] == y) %>%
    str_pad(width = 3, side = "left", pad = "0") # make longer if you have over 1000

  box.sub <- cbind(c(x, x, x + box.size, x + box.size),
                   c(y, y + box.size, y + box.size, y)) %>%
    vect(type="polygons", crs = crs(mask_crop))

   num.valid = crop(mask_crop, box.sub) %>% global("notNA") %>% as.numeric()

  box.sub$name = n.name

  if(num.valid > 0){return(box.sub)}
})

# Vectorize the boxes
boxes.v = vect(boxes)

#writeVector(boxes.v, "D:/BP_Layers/outputs/boxes.shp")

boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")

#plot it to see what it looks like
plot(mask_crop, ext = ext(boxes.v), col = "red")
plot(boxes.v, add = T)
text(boxes.v, boxes.v$name)

########################################################################################################################
# have a loop where you go through each box and output a cropped raster from mask_crop
# Set file path for the folder containing the input rasters

#folder_path <- "D:/BP_Layers/outputs/inputs"
folder_path <- "D:/BP_Layers/outputs/climate"
#folder_path <- "D:/Radiation/30m_crop_align/M_nineS"

# vector polygons used for cropping = boxes.v from above!

# Get a list of file names in the folder
raster_files <- list.files(folder_path, pattern = "*.tif$")

# Loop through the vector polygons and crop the rasters - here we are cropping just boxes 32:34...
for (i in 44:46) {         #nrow(boxes.v)) {
  polygon <- boxes.v[i, ]

  # Initialize an empty data table to store values for each cropped area
  values_df <- data.table()

  # Loop through the list of rasters and extract values
  for (j in 1:length(raster_files)) {
    file_path <- paste0(folder_path, "/", raster_files[j])
    raster <- rast(file_path)

    # Crop the masked layer
    sub.mask = crop(mask_crop, polygon)

    # Crop the raster to the current polygon
    cropped_raster <- crop(raster, polygon) %>% mask(sub.mask)

    # Extract the values from the cropped raster
    #values <- as.numeric(cropped_raster)

    vals <- values(cropped_raster, xy = TRUE) %>% as.data.table()

    #values <- values(cropped_raster, dataframe = TRUE)

    #column.name <- basename(file_path)
    #column.name <- gsub('.tif','',column.name)
    colnames(vals)[1]  <- gsub(".tif$", "", raster_files[j])

    # Add the values to the data table
    values_df <- cbind(values_df, vals)
    values_df <- values_df %>% mutate_if(is.numeric, round, digits=2)
  }

  # Save the data table as a .csv file
  write.csv(values_df, paste0("D:/BP_Layers/outputs/crops/climate/", polygon$name, ".csv"), row.names = FALSE)
  rm(values_df)
}


# then you have a folder full of smaller tiles that are just the mask values
############################################################

