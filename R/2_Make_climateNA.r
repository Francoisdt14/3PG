# ClimateNA script
# This script uses a DEM to generate the climate data required for 3PG to run

# Load packages:
library(tidyverse); library(terra); library(data.table); library(silvR21); library(parallel); library(pbapply); library(arrow);

# Load DEM (and study area to crop if necessary)

dem <- rast("Z:/_CanadaLayers/Rasters/DEM/UTM18S_DEM.dat") #Z:\_CanadaLayers\Rasters\DEM

study <- vect("D:/PostDoc/Layers/BP_Maps/Study_Area_U_18S.shp") #D:\PostDoc\Layers\BP_Maps\Study_Area_1_18S

dem_crop <- terra::crop(dem, study)


# terra::writeRaster(dem_crop, "D:/climate/U_18S/dem_crop_U_18S.tif") # D:\climate\dems
# The above raster is a 30 meter raster resolution raster

rm(list = ls())
tmpFiles(remove = T)

####################################################################

# Make 1 km raster and associated CSV
f.dem30 = "D:/climate/M_18S/dem_crop_M_18S.tif" # assume this is a 30m DEM

# load it in
r <- rast(f.dem30) %>% aggregate(33.3) %>% project("epsg:4326") # epsg should be correct for study area

out = str_replace(f.dem30, ".tif", ".csv") # Can remove this second part as it is just folder structure

# Write the 1 km raster - this will be used to assign the values later
writeRaster(r, "D:/climate/U_18S/Study_Area_U_18S_1km.tif")

# make CSV and write it:
r_df <- as.data.frame(r, xy = T, na.rm = T)
r_df$x <- r_df$x * -1
r_df$ID1 <- 1:nrow(r_df)
r_df$ID2 <- 1:nrow(r_df) # A second ID column must be made to produce the climate variables - this is required by ClimateNA

# Restructure to match ClimateNA needs
csv <- r_df[c(4,5,2,1,3)]
names(csv) <- c('ID1','ID2','lat','long','el')
fwrite(csv, out, row.names = FALSE)

####################################################################
####################################################################

# Run climateNA on the csv - this is where we actually run ClimateNA
# ClimateNA information is available here: https://climatena.ca/
f = out

setwd("D:/Climatena_v730")# Need to set the working directory to the correct location otherwise it crashes. We are using the 'silvR21' package here

hist30YClimateNA(f, '1991_2020', 'M', "D:/ClimateNA_v730/ClimateNA_v7.30.exe", outdir = "D:/climate/U_18S/output_monthly") # Change outdir if necessary

# NOTE: radiation values are only available until 2010 (per climateNA) - will get NAs after this. Since they don't change can use RAD values from earlier years

####################################################################
####################################################################

# Generate rasters for each variable in each year
f = list.files("D:/climate/U_18S/output_monthly/", pattern = ".csv$", full.names = T, recursive = T) # CSV output from above

 # Only do it for a few variables if you don't want to include all variables
var.list = fread("D:/climate/U_18S/climateNA_vars_subset.csv", header  =F) %>% pull(V1)

# Read in csv
dt = fread(f[1]) %>% select(Latitude, Longitude, all_of(var.list))

var.list = names(dt)[4:ncol(dt)] # subset out the lat long, etc

for(v in var.list){
  print(v)
  pts = select(dt, Longitude, Latitude, all_of(v)) %>%
        vect(geom = c("Longitude", "Latitude"), crs = "epsg:4326")

  r2 = rasterize(pts, r, field = v)

  # Output the file
  writeRaster(r2, paste0("D:/climate/U_18S/output_tif_1km/", "StudyArea", "_", v, ".tif"), overwrite = T)
  terra::tmpFiles(remove = T)
}

####################################################################
####################################################################

# Re-sample outputs to resolution of NTEMS layers (30m)
# Select the files from the years you want

fl = list.files("D:/climate/M_18S/output_tif_1km", full.names = T, pattern = ".tif$")

num.valid = rast("D:/climate/M_18S/dem_crop_M_18S.tif") %>% global("notNA") %>% as.numeric()

for(f in fl){
  cat(paste0("\nOn ", which(fl == f), "/", length(fl)))

 # f = fl[1]
 f.out = str_replace(f, "output_tif_1km", "output_tif_30m")

  # Only run if you haven't before
  if(!file.exists(f.out)){
      t1 = Sys.time()
    # This should be the original 30m DEM
    blank.r = rast(f.dem30)

    r2 = rast(f) %>%
         project(crs(blank.r)) %>%
         focal(w = 5, fun = "mean", na.policy = "only", na.rm = T) %>%
         resample(blank.r, method = "cubicspline") %>%

         mask(blank.r)

    n.current = global(r2, "notNA") %>% as.numeric()

    if(n.current != num.valid){cat("-- not the same as target");next}

    writeRaster(r2, f.out, overwrite = T)
    difftime(Sys.time(), t1, units = "secs") %>% as.numeric() %>% round(2) %>% paste0(" -- done in ", . , " seconds") %>% cat()
  }

  terra::tmpFiles(remove = T)
}

####

# Make a csv (or parquet) file from the 30m rasters

fl = raster.list # change to the folder output above

r.big = rast(fl)

# r_df <- as.data.frame(r.big, xy = T, na.rm = T)
r_df <- values(r.big) %>% as.data.table()

# write the dataframe to a csv or a parquet file
# csvs are huge compared to parquet files

fwrite(r_df, FILEPATH.csc)
arrow::write_parquet()


# r_df <- as.data.frame(r.big, xy = T, na.rm = T)
r_df <- values(r.big) %>% as.data.table()

# get column names etc correct:
col_names <- lapply(fl, function(x) sub("\\.[^.]*$", "", basename(x)))
col_names <- unlist(col_names)
names(r_df) <- col_names

fwrite(r_df, "D:/climate/testing/rad_test/output_df_30m/Study_Area_M_9S_rad_30m.csv")
library(arrow)
arrow::write_parquet(r_df, "D:/climate/testing/rad_test/output_df_30m/Study_Area_M_9S_rad_30m.parquet") # feelinig spicy?

#############################################################################
