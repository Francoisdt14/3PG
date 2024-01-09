# Load required library
library(terra)

# Input folder containing the original large rasters
input_folder <- "D:/BP_Layers/M_9S/3PG_flt/4_30m_inputs_all/"

# Output folder for the divided rasters
output_folder <- "D:/BP_Layers/M_9S/3PG_flt/temp"

######################################################################################################


# Tile the large input rasters into manageable chunks

source("R/lib.R")

# Masking - again, we are using the tree_mask!

mask_crop <- rast("D:/BP_Layers/M_9S/3PG_flt/4_30m_inputs_all/Forest_Age_2019.tif")

#mask_crop <- mask

box.size = 75000 # How big do you want the boxes to be? in m

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

# WRITE THIS TO THE CORRECT LOCATION FOR THE STUDY AREA
#writeVector(boxes.v, "D:/BP_Layers/M_18S/boxes.shp")

#boxes.v <- vect("D:/BP_Layers/M_18S/boxes.shp")

#plot it to see what it looks like
plot(mask_crop, ext = ext(boxes.v), col = "red")
#plot(rast_m, ext = ext(boxes.v))
plot(mask_crop)
plot(boxes.v, add = T)
text(boxes.v, boxes.v$name)

########################################################################################################################
# have a loop where you go through each box and output a cropped raster from mask_crop
# Set file path for the folder containing the input rasters

#folder_path <- "D:/BP_Layers/M_18S/inputs"
folder_path <- "D:/BP_Layers/M_9S/3PG_flt/4_30m_inputs_all"
#folder_path <- "D:/BP_Layers/M_18S/climate"

# vector polygons used for cropping = boxes.v from above!

# Get a list of file names in the folder
raster_files <- list.files(folder_path, pattern = "*.tif$")

# Define which boxes we want to do here:

#vals <- c(296:306, 327:337, 358:368, 389:399, 420:430, 451:461, 482:492, 513:523, 544:554, 575:585, 606:616)
#vals <- c(574:585, 605:617)

polygon <- boxes.v[7, ]


#target_folder <- "D:/BP_Layers/M_18S/crops/inputs2"
target_folder <- "D:/BP_Layers/M_9S/3PG_flt/temp2/tifs/"
#target_folder <- "D:/BP_Layers/M_18S/crops/climate"

# Loop through each raster input
for (i in 1:length(raster_files)) {

    file_path <- paste0(folder_path, "/", raster_files[i])
    rast1 <- rast(file_path)

    # load in big raster
    #r <- terra::rast(raster_files[i])

    ## IS THIS NECESSARY?? - NOT FOR RADIATION
    ###
    # Crop the masked layer
    sub.mask = crop(mask_crop, polygon)

    # Crop the raster to the current polygon
    cropped_raster <- crop(rast1, polygon) %>% mask(sub.mask)

    file.name <- basename(raster_files[i])

    file.name <- gsub('.tif','',file.name)

    # write
    terra::writeRaster(cropped_raster, paste(target_folder, file.name, ".tif", sep = ""), overwrite = T)

}




