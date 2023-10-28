library(terra)
library(tidyverse)

rm(list = ls())

age.filled <- rast("D:/BP_Layers/M_9S/3PG_flt/4_30m_inputs_all/Forest_Age_2019.tif")

plot(age.filled)

age <- rast("D:/BP_Layers/outputs/inputs/Forest_Age_2019.tif")
age.clamp <- clamp(age, lower = 1869, upper = Inf)

plot(age.clamp)

blank <- age.clamp
values(blank) <- NA



# Convert raster to points
points <- as.points(age.clamp)

####################################################



# Create a raster with values from 10 to 100
r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
values(r) <- seq(10, 100, length.out = ncell(r))

# Introduce some NA values
set.seed(123)  # For reproducibility
random_na_indices <- sample(1:ncell(r), size = ncell(r) * 0.3)  # Introduce 10% NA values
r[random_na_indices] <- NA

plot(r)

xy <- data.frame(xyFromCell(r, 1:ncell(r)))
v <- values(r)
i <- !is.na(v)
xy <- xy[i,]
v <- v[i]


tps <- Tps(xy, v)
p <- r

plot(p)

# use model to predict values at all locations
p2 <- interpolate(p, tps, na.rm = TRUE, cores = 12)

#p <- mask(p, r)
plot(p2)


########################################



blank.r <- r
values(blank.r) <- 1
plot(blank.r)

# Convert raster to points
points <- as.points(r, values = TRUE, na.rm = TRUE)
plot(points)



r2 <- rasterize(points, field = 'lyr.1', blank.r) %>%
      resample(blank.r, method = "cubicspline")

plot(r2)


#################
r <- rast(system.file("ex/elev.tif", package="terra"))
ra <- aggregate(r, 10)
xy <- data.frame(xyFromCell(ra, 1:ncell(ra)))
v <- values(ra)
i <- !is.na(v)
xy <- xy[i,]
v <- v[i]

## Not run:
#library(fields)

tps <- Tps(xy, v)
p <- rast(r)

# use model to predict values at all locations
p <- interpolate(p, tps)
p <- mask(p, r)
plot(p)



############################################################


# Set cells with values less than 10 to NA
raster_data_masked <- mask(raster_data, raster_data >= 10)




m[m < 0.2] <- NA



#######################################################

planted <- rast("D:/3PG_Cplusplus/data_100/forest_age_2019.flt")

age.2019 <- 2019 - planted

writeRaster(age.2019, "D:/3PG_Cplusplus/future_forest_age_input_TEST/age2019.flt")

age.2040 <- age.2019 + 21

writeRaster(age.2040, "D:/3PG_Cplusplus/future_forest_age_input_TEST/age2040.flt", overwrite = TRUE)

age.2060 <- age.2019 + 41
age.2080 <- age.2019 + 61
age.2100 <- age.2019 + 81

writeRaster(age.2100, "D:/3PG_Cplusplus/future_forest_age_input_TEST/age2100.flt")


test <- rast("D:/3PG_Cplusplus/future_forest_age_input_TEST/wf.flt")
test2 <- rast("D:/3PG_Cplusplus/future_forest_age_input_TEST/age2019.flt")

compareGeom(test, test2)




wf <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y2_S2/wf.flt")

crs(wf) <- crs(planted)

wr <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y2_S2/wr.flt")

crs(wr) <- crs(planted)

ws <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y2_S2/ws.flt")

crs(ws) <- crs(planted)

stemno <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y2_S2/stemno.flt")
crs(stemno) <- crs(planted)

asw <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y2_S2/asw.flt")
crs(asw) <- crs(planted)

writeRaster(asw, "D:/3PG_Cplusplus/future_forest_age_input_TEST/Y2_S2/asw.flt")




global(ws, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)



#### M_18S


planted <- rast("D:\\BP_Layers\\M_18S\\3PG_flt\\6_90m_flt\\forest_age_2019.flt")

age.2019 <- 2019 - planted

writeRaster(age.2019, "D:/3PG_Cplusplus/future_forest_age_input_18s/age2019.flt")

age.2040 <- age.2019 + 21

writeRaster(age.2040, "D:/3PG_Cplusplus/future_forest_age_input_18s/age2040.flt", overwrite = TRUE)

age.2060 <- age.2019 + 41
age.2080 <- age.2019 + 61
age.2100 <- age.2019 + 81

writeRaster(age.2080, "D:/3PG_Cplusplus/future_forest_age_input_18s/age2080.flt")


plot(age.2100)


#############################

# create float files


# Loop to create float files

# Set the input and output directories
input_dir <- "Y:/Francois/_Vaughan/732"

output_dir <- "Y:/Francois/_Vaughan/732_FLT"

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

check.rast3 <- rast("Y:/Francois/_Vaughan/732/PPT11.tif")

###
# We want to find the lower left center of the X and Y dimension for the header files:
res_x <- res(check.rast3)[1]
res_y <- res(check.rast3)[2]

center_x <- xmin(raster) + res_x / 2
center_y <- ymin(raster) + res_y / 2
# Print the results
print(paste("Center X:", center_x))
print(paste("Center Y:", center_y))

# rewrite HDR files!

library(stringr)

# Set the directory where the .hdr and .flt files are located

#directory <- "Y:/Francois/flt_test_100_noNA"
directory <- "Y:/Francois/_Vaughan/732_FLT"
# Get the list of .hdr files in the directory
hdr_files <- list.files(directory, pattern = "\\.hdr$", full.names = TRUE)

# very important to write this correctly depending on raster size!!

# Process each .hdr file
for (hdr_file in hdr_files) {
    # Define the new content for the .hdr file
    new_content <- c(
        "NROWS          325", # 90m = 3249 , 30m = 9745
        "NCOLS          325", # 90m 3249  , 30m 9746
        "xllcenter         583618.283481425", # 90m = 403650.448601884 , 30m = 403619.663820003
        "yllcenter         6542691.1043566", # 90m = 6312667.21413766" , 30m = 6312636.42935578"
        "cellsize           30.78478", # 90m = 92.35435 , 30m = 30.78478
        "nodata_value -9999.000000",
        "byteorder lsbfirst"
    )

    # Write the new content to the .hdr file, overwriting the existing contents
    writeLines(new_content, hdr_file)
}

check.rast4 <- rast("Y:/Francois/_Vaughan/732_FLT/PPT11.flt")


###
# random final checks

check.raster5 <- rast("Y:/Francois/_Vaughan/732_FLT/nffd01.flt")
check.raster6 <- rast("Y:/Francois/_Vaughan/732_FLT/Rad12.flt")

compareGeom(check.raster5, check.raster6)



