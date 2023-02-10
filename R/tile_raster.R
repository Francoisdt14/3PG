# Tile the large input rasters into manageable chunks

source("R/lib.R")


# Masking

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
  # box.sub <- cbind(c(x - 30, x - 30, x + box.size + 30, x + box.size + 30),
  #                  c(y - 30, y + box.size + 30, y + box.size + 30, y - 30)) %>%
    vect(type="polygons", crs = crs(mask_crop))

  # test1 = st_as_sf(box.sub) %>% exact_extract(mask_crop, .) %>% bind_rows() %>% pull(value) %>% na.omit()
  num.valid = crop(mask_crop, box.sub) %>% global("notNA") %>% as.numeric()

  box.sub$name = n.name

  if(num.valid > 0){return(box.sub)}
})

boxes.v = vect(boxes)
#writeVector(boxes.v, "D:/BP_Layers/outputs/boxes.shp")



plot(mask_crop, ext = ext(boxes.v), col = "red")
plot(boxes.v, add = T)
text(boxes.v, boxes.v$name)

########################################################################################################################
# have a loop or something where you go through each box and output a cropped raster from mask_crop
# Set file path for the folder containing the input rasters

folder_path <- "D:/BP_Layers/outputs/inputs"

# vector polygons used for cropping = boxes.v

# Get a list of file names in the folder
raster_files <- list.files(folder_path, pattern = "*.tif$")

# Loop through the vector polygons and crop the rasters
for (i in 32:34) {         #nrow(boxes.v)) {
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
  write.csv(values_df, paste0("D:/BP_Layers/outputs/crops/inputs/", polygon$name, ".csv"), row.names = FALSE)
  rm(values_df)
}


####
# Use terra::makeTiles()

############################################################



polygon <- boxes.v[i, ]
lat.mask = crop(mask_crop, polygon)

# For each cell in the raster
lat_long <- project(lat.mask, "+proj=longlat +datum=WGS84")
# Extract y coordinate of each cell center
y_coordinate <- yFromRow(lat_long, 1:nrow(lat_long))
# Save as dataframe
df_y_coordinate <- data.frame(y_coordinate)

# For the entire ratster middle
# Get the extent of the raster
r_extent <- ext(lat.mask)
# Get the CRS of the raster
r_crs <- crs(lat.mask)
# Calculate the center point
center_x <- (r_extent[1] + r_extent[2]) / 2
center_y <- (r_extent[3] + r_extent[4]) / 2
center_point <- cbind(center_x, center_y)


center_spatial <- SpatialPoints(center_point)
proj4string(center_spatial) <- CRS("+init=epsg:32609")
center_spatial_lat <- spTransform(center_spatial, CRS("+init=epsg:4326")) %>% as.data.frame
lat <- center_spatial_lat[1,2]


##########


climate_df <- values_df
inputs_df <- values_df

# then you have a folder full of smaller tiles that are just the mask values



############################################################

prob = list.files("_boxes", pattern = "probability.tif$", full.names = T, recursive = T) %>% str_subset("run", negate = T) %>% lapply(rast) %>% sprc() %>% mosaic(fun = "median")
