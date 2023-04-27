# Datasets required by r3PG for each tile:
#   site
#   species
#   climate
#   thinning
#   parameters
#   size_dist

source("R/lib.R")



# We need parameter data for each species
# Currently using customized parameters (but can use parameters directly from the vignette)
f_loc <- './data/input/data.input2.xlsx'
parameters <-  read_xlsx(f_loc, 'parameters')

#################################################################################
# Main Function


Calculate_3PG_Y <- function(climate.df, inputs.df, cl = NA) {
    # Currently using customized parameters (but can use parameters directly from the vignette)

    test_df = pblapply(cl = cl, 1:nrow(climate.df), FUN = function(i){
        ##########################################################
        # CLIMATE #
        # d_climate requires: month, tmp_min, tmp_max, tmp_ave,  prcp,  srad, frost_days
        # all available from climateNA data

        days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        first_row_data <- climate.df[i,]

        #Select the appropriate columns and rearrange for the correct input:
        first_row_select <- first_row_data %>% #select(Tmax01:PPT12, NFFD01:NFFD12) %>%
            pivot_longer(cols = c(NFFD01:Rad12)) %>%
            separate(name, into = c("Var", "month"), sep = -2) %>%
            pivot_wider(names_from = Var, values_from = value) %>%
            mutate(frost_days = days - NFFD,
                   month = as.numeric(month)) %>%
            select(-NFFD) %>%
            as.data.table() %>% rename(prcp = PPT, srad = Rad, tmp_max = Tmax, tmp_min = Tmin)
        ##########################################################

        site.columns = c("latitude","altitude","soil_class", "asw_i", "asw_min", "asw_max","from","to")
        site = data.frame(matrix(nrow = 1, ncol = length(site.columns)))
        colnames(site) = site.columns


        site$latitude <- inputs.df[i,1] # this is now being pulled for each pixel
        site$altitude <- inputs.df[i,2]
        site$soil_class <- 3 # Site factors will include data from other rasters
        site$asw_i <-999 #
        site$asw_min <- 0
        site$asw_max <- 300

        # simulation from and to!
        #site$from <-paste(inputs.df[i,4],"-6", sep = "")
        site$from <- '1969-12'
        site$to <- '2019-12'

        ##########################################################
        # SPECIES data

        # d_species requires  - species, planted date, fertility, stems_n, biom_stem, biom_root, biom_foliage
        # species from species raster, planted date from disturbance data
        # fertility? stems_n? Biomass?

        sp.columns = c("species","planted","fertility", "stems_n", "biom_stem","biom_root","biom_foliage")
        species = data.frame(matrix(nrow = 1, ncol = length(sp.columns)))
        colnames(species) = sp.columns

        #species$species <- 'White Spruce' # should come from species raster - need to figure out how to do this...

        if (inputs.df[i,5] == 3) {
            species$species <- "Black Spruce" # should be sub-alpine fir
        } else if (inputs.df[i,5] == 23) {
            species$species <-"Lodgepole Pine"
        } else if (inputs.df[i,5] == 18) {
            species$species <-"Black Spruce"
        } else if (inputs.df[i,5] == 22) {
            species$species <-"Jack Pine"
        } else if (inputs.df[i,5] == 29) {
            species$species <-"Black Spruce" # this should be trembling aspen
        } else {
            species$species <-"Blank Tree" # these are inconsequential species
        }


        species$planted <- paste(inputs.df[i,4],"-6", sep = "") # should now come from the AGE raster!

        #### set it to 50 years here
        #species$planted <- '1969-12' ###

        species$fertility <- 0.7 # should come from soil - generally low fertility from literature
        species$stems_n <- 4000 # literature
        species$biom_stem <- 6 # literature
        species$biom_root <- 3 # literature
        species$biom_foliage <- 1 # literature

        ############################################################
        # MAIN FUNCTION

        out_3PG <- run_3PG(
            site        = site,
            species     = species,
            climate     = first_row_select,
            # thinning    = d_thinning,
            parameters  = parameters,
            #size_dist   = d_sizeDist,
            settings    = list(light_model = 1, transp_model = 1, phys_model = 1,
                               height_model = 1, correct_bias = 0, calculate_d13c = 0),
            check_input = TRUE, df_out = TRUE)

        dbh_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'dbh', 5]
        lai_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'lai', 5]
        basal_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'basal_area', 5]


        out_3PG$year <- lubridate::year(out_3PG$date)
        # filter the data for the final year and the 'npp' variable
        final_year_npp <- out_3PG %>% filter(year == 1935 & variable == "npp")
        # then, use the 'sum' function to add up all of the npp values for the final year
        total_npp <- sum(final_year_npp$value)

        return(data.frame("dbh" = dbh_select, "lai" = lai_select, "npp" = total_npp,
                          "basal_area" = basal_select))
    }) %>% bind_rows()

    return(test_df)
}

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

# csvs are too large for github
output_folder <- "D:/BP_Layers/outputs/crops/dataframes"

#tile.numb <- c(360:366, 391:397, 422, 423, 427, 428, 453, 454, 458, 459, 484, 485, 489, 490, 515:521, 546:552)
tile.numb <- c(484:490, 515:521, 546:552)
tile.numb <- (515)

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

    #dim(full_comb)
    #round the forest age?
    full_comb$Forest_Age_2019 <- ifelse(rowSums(is.na(full_comb)) == 1 & is.na(full_comb$Forest_Age_2019), 2019, full_comb$Forest_Age_2019)

    full_comb_clean <- full_comb %>% na.omit()
    full_comb_clean$Forest_Age_2019 <- round(full_comb_clean$Forest_Age_2019)
    full_comb_clean$focal_mean <- round(full_comb_clean$focal_mean, 3)
    full_comb_clean$dem_crop_M_9S <- round(full_comb_clean$dem_crop_M_9S)
    full_comb_clean$Forest_Age_2019 <- ifelse(full_comb_clean$Forest_Age_2019 < 1869, 1869, full_comb_clean$Forest_Age_2019)
    colnames(full_comb_clean) <- c("lat", "dem", "disturbance", "age", "species")

    climate_df <- read.csv(paste0("D:/BP_Layers/outputs/crops/climate/",i,".csv")) %>% na.omit()
    rad_df <- read.csv(paste0("D:/BP_Layers/outputs/crops/rad/",i,".csv")) %>% na.omit()
    # Need to change radiation names, divide by 10, and then divide by 2, and cbind the radiation data..
    colnames(rad_df) <- c("Rad01", "Rad02", "Rad03", "Rad04", "Rad05", "Rad06", "Rad07", "Rad08", "Rad09", "Rad10", "Rad11", "Rad12")
    # Divide all values by 20
    rad_df2 <- rad_df / 20
    climate_df2 <- cbind(climate_df, rad_df2)

    ####################################
    # Need to make sure all the dataframes are the same size
    # Here we are running things in parallel!


    clusterExport(cl, varlist = c("climate_df2", "full_comb_clean", "f_loc", "parameters"))

    result <- Calculate_3PG_Y(climate_df2, full_comb_clean, cl = cl)

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
