# Datasets required by r3PG for each tile:
#   site
#   species
#   climate
#   thinning
#   parameters
#   size_dist

source("R/lib.R") # load in all of the libraries required
source("R/funs.R") # load in the functions we want to use


# We need parameter data for each species
# Currently using customized parameters (but can use parameters directly from the vignette)
f_loc <- './data/input/data.input2.xlsx'
parameters <-  read_xlsx(f_loc, 'parameters')

##################################################################################

# Read in the two main data sources - climate data, and remaining inputs

# climate includes: Frost, precipitation, temps, radiation
# Inputs are the remaining inputs required by 3PG that are rasterized: DEM, disturbance date, and species.
# the inputs dataframe was created in 5_tile_raster and just happens to be the non-climate data that was processed at the same time

# You should have access to these pre-prepared rds files


# Use .rds because it saves space
#climate_df <- readRDS("./data/input/climate_033.rds") %>% na.omit()
#inputs_df <- readRDS("./data/input/inputs_033.rds") %>% na.omit()
boxes.v <- vect("D:/BP_Layers/outputs/boxes.shp")

# Make sure we are sending this to the correct folder!
output_folder <- "D:/BP_Layers/outputs/crops/889_test/flt"

# 25 square 5 x 5
#tile.numb <- c(392:396, 423:427, 454:458, 485:489, 516:520)

# 49 square (7 x 7)
#tile.numb <- c(360:366, 391:397, 422:428, 453:459, 484:490, 515:521, 546:552)

# 121 square (11 x 11)
#tile.numb <- c(296:306, 327:337, 358:368, 389:399, 420:430, 451:461, 482:492, 513:523, 544:554, 575:585, 606:616)

tile.numb <- 889

# cl = detectCores()/2 %>% makeCluster()
cl = makeCluster(20) # number of cores
clusterEvalQ(cl, {library(r3PG); library(dplyr); library(readxl); library(data.table); library(tidyr); library(tidyverse); library(lubridate)})  # source also works

for (i in tile.numb) {         #nrow(boxes.v)) {

    #polygon <- boxes.v[boxes.v$name == i,]

    filename <- paste0(i, ".csv")
    filepath <- file.path(output_folder, filename)

    if (file.exists(filepath)) {
        cat("Skipping file", filename, "because it already exists.\n")
        next  # move to next iteration of the loop
    }

    lat_full <- read.csv(paste0("D:/BP_Layers/outputs/crops/lat/",i, ".csv"))
    inputs_full <- read.csv(paste0("D:/BP_Layers/outputs/crops/inputs2/",i,".csv"))

    inputs_full <- select(inputs_full, -leading.species_2019)
    full_comb <- cbind(lat_full, inputs_full)

    # here we read the random raster for fertility and max_asw - need to fix this for the corrext box!!!
    fert_asw <- read.csv("D:/BP_Layers/outputs/crops/889_test/varied/inputs3.csv")

    full_comb <- cbind(full_comb, fert_asw)

    #dim(full_comb)
    #round the forest age?
    full_comb$Forest_Age_2019 <- ifelse(rowSums(is.na(full_comb)) == 1 & is.na(full_comb$Forest_Age_2019), 2019, full_comb$Forest_Age_2019)

    full_comb_clean <- full_comb %>% na.omit()
    full_comb_clean$Forest_Age_2019 <- round(full_comb_clean$Forest_Age_2019)
    full_comb_clean$focal_mean <- round(full_comb_clean$focal_mean, 3)
    full_comb_clean$dem_crop_M_9S <- round(full_comb_clean$dem_crop_M_9S)
    full_comb_clean$Forest_Age_2019 <- ifelse(full_comb_clean$Forest_Age_2019 < 1869, 1869, full_comb_clean$Forest_Age_2019)
    colnames(full_comb_clean) <- c("lat", "dem", "disturbance", "age", "species", "max_asw", "fert")

    climate_df <- read.csv(paste0("D:/BP_Layers/outputs/crops/climate/",i,".csv")) %>% na.omit()
    rad_df <- read.csv(paste0("D:/BP_Layers/outputs/crops/rad/",i,".csv")) %>% na.omit()
    # Need to change radiation names, divide by 10, and then divide by 2, and cbind the radiation data..
    colnames(rad_df) <- c("Rad01", "Rad10", "Rad11", "Rad12", "Rad02", "Rad03", "Rad04", "Rad05", "Rad06", "Rad07", "Rad08", "Rad09")
    # Put the columns in the correct order:
    rad_df <- rad_df[, c("Rad01", "Rad02", "Rad03", "Rad04", "Rad05", "Rad06", "Rad07", "Rad08", "Rad09", "Rad10", "Rad11", "Rad12")]

    # Divide all values by 10 - the scale factor is 0.1
    rad_df2 <- rad_df / 10
    ##### DOUBLE CHECK RADIATION!!!

    # we divided by 20 because we thought we needed PAR - not the case!
    #rad_df2 <- rad_df / 20

    climate_df2 <- cbind(climate_df, rad_df2)

    ####################################
    # Need to make sure all the dataframes are the same size
    # Here we are running things in parallel!


    clusterExport(cl, varlist = c("climate_df2", "full_comb_clean", "f_loc", "parameters"))

    # CHECK THE FUNCTION CALL IS CORRECT HERE!!!

    #result <- Calculate_3PG_80(climate_df2, full_comb_clean, cl = cl)

    result <- Calculate_3PG_flt(climate_df2, full_comb_clean, cl = cl)

    result_df <- bind_rows(result)

    write.csv(result_df, paste0(output_folder,"/",i,".csv"), row.names = FALSE)
    # fwrite

    # print message
    cat(filename, "written.\n")

    rm(result_df)

    # Reset the items in the clusters so that you save memory and also don't accidentally re-do a tile
    climate_df2 <- NA
    full_comb_clean <- NA
    clusterExport(cl, varlist = c("climate_df2", "full_comb_clean", "f_loc", "parameters"))
}

stopCluster(cl)

# 9ish seconds for 1000 rows (20 cores)
# 1:19 for 10 000 rows (20 cores)

#test.df <- read.csv("D:/BP_Layers/outputs/crops/dataframes_2/360.csv")
