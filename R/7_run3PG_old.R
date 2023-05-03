# Datasets required by r3PG for each tile:
#   site
#   species
#   climate
#   thinning
#   parameters
#   size_dist

source("R/lib.R")

# Read in the two main data sources - climate data, and remaining inputs

# climate includes: Frost, precipitation, temps, radiation
# Inputs are the remaining inputs required by 3PG that are rasterized: DEM, disturbance date, and species.
# the inputs dataframe was created in 5_tile_raster and just happens to be the non-climate data that was processed at the same time

# You should have access to these pre-prepared rds files


# Use .rds because it saves space
#climate_df <- readRDS("./data/input/climate_033.rds") %>% na.omit()
#inputs_df <- readRDS("./data/input/inputs_033.rds") %>% na.omit()


# csvs are too large for github

lat_full <- read.csv("D:/BP_Layers/outputs/crops/lat/425.csv")
inputs_full <- read.csv("D:/BP_Layers/outputs/crops/inputs2/425.csv")

inputs_full <- select(inputs_full, -leading.species_2019)

full_comb <- cbind(lat_full, inputs_full)

dim(full_comb)
#round the forest age?

full_comb$Forest_Age_2019 <- ifelse(rowSums(is.na(full_comb)) == 1 & is.na(full_comb$Forest_Age_2019), 2019, full_comb$Forest_Age_2019)

full_comb_clean <- full_comb %>% na.omit()

full_comb_clean$Forest_Age_2019 <- round(full_comb_clean$Forest_Age_2019)
full_comb_clean$focal_mean <- round(full_comb_clean$focal_mean, 3)
full_comb_clean$dem_crop_M_9S <- round(full_comb_clean$dem_crop_M_9S)

full_comb_clean$Forest_Age_2019 <- ifelse(full_comb_clean$Forest_Age_2019 < 1869, 1869, full_comb_clean$Forest_Age_2019)

colnames(full_comb_clean) <- c("lat", "dem", "disturbance", "age", "species")


climate_df <- read.csv("D:/BP_Layers/outputs/crops/climate/425.csv") %>% na.omit()
#inputs_df <- read.csv("D:/BP_Layers/outputs/crops/inputs/044.csv") %>% na.omit()
rad_df <- read.csv("D:/BP_Layers/outputs/crops/rad/425.csv") %>% na.omit()
#lat_df <- read.csv("D:/BP_Layers/outputs/crops/lat/044.csv") %>% na.omit()

# Need to change radiation names, divide by 10, and then divide by 2, and cbind the radiation data..
colnames(rad_df) <- c("Rad01", "Rad02", "Rad03", "Rad04", "Rad05", "Rad06", "Rad07", "Rad08", "Rad09", "Rad10", "Rad11", "Rad12")

# Divide all values by 20
rad_df2 <- rad_df / 20

climate_df2 <- cbind(climate_df, rad_df2)

#climate.df <- climate_df2
#inputs.df <- full_comb_clean
# Join inputs_df

# Currently using customized parameters (but can use parameters directly from the vignette)
f_loc <- './data/input/data.input2.xlsx'
parameters <-  read_xlsx(f_loc, 'parameters')

####################################
# Need to make sure all the dataframes are the same size

# Currently running a loop but will work towards improving this
# Initialize an empty data table to store values for each cropped area

#Calculate_3PG <- function(climate_df, input_df) {

# create an empty dataframe to store results
#result_df <- data.frame(new_value = numeric((nrow(climate_df))))

# THIS IS FASTER - allocate the size of the dataframe ahead of time


result_df <- data.frame(dbh = numeric(20))


# for example now: NPP, leaf area index (lai), and dbh (diameter at breast height) - this is slower
npp.df <- data.table()
lai.df <- data.table()
dbh.df <- data.table()

#for(i in 1:nrow(climate_df)) {

# parameters
# need parameters for every species

# We can pull the parameters data out
# Currently using customized parameters (but can use parameters directly from the vignette)
f_loc <- './data/input/data.input2.xlsx'
parameters <-  read_xlsx(f_loc, 'parameters')


for(i in 1:20) {
    #nrow(climate_df)){

    ##########################################################
    # CLIMATE #
    # d_climate requires: month, tmp_min, tmp_max, tmp_ave,  prcp,  srad, frost_days
    # all available from climateNA data

    days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    first_row_data <- climate_df[i,]

    # Select the appropriate columns and rearrange for the correct input:
    first_row_select <- first_row_data %>% #select(Tmax01:PPT12, NFFD01:NFFD12) %>%
        pivot_longer(cols = c(NFFD01:Tmin12)) %>%
        separate(name, into = c("Var", "month"), sep = -2) %>%
        pivot_wider(names_from = Var, values_from = value) %>%
        mutate(frost_days = days - NFFD,
               month = as.numeric(month)) %>%
        select(-NFFD) %>%
        as.data.table() %>% rename(prcp = PPT, srad = Rad, tmp_max = Tmax, tmp_min = Tmin)

    ##########################################################
    # SITE data

    # d_site requires - lat, elevation, soil class, asw_i, asw_min, asw_max, from, to
    # lat & altitude from climateNA
    # soil class from soil dataset - extract data in raster and assign?
    #asw_i, aws_min, asw_max ??
    # from - to = when we want to run the simulation

        #site <- first_row_data %>% select(c('Latitude', 'Elevation')) %>% rename(latitude = Latitude, altitude = Elevation)

    site <- inputs_df[i,]
    site <- site %>% select(c('dem_crop_M_9S')) %>% rename(altitude = dem_crop_M_9S)

    site$latitude <- 57.08537 # this is currently created in 6_Latitude, but can be done for each pixel

    site$soil_class <- 3 # Site factors will include data from other rasters
    site$asw_i <-999 #
    site$asw_min <- 0
    site$asw_max <- 300


    # simulation from and to!
    site$from <- '2000-01'
    site$to <- '2020-12'

    # We need to organize this and put things in the correct order
    site <- site[, c(2, 1, 3, 4, 5, 6, 7, 8)]

    #prepare_site(site)

    ##########################################################
    # SPECIES data

    # d_species requires  - species, planted date, fertility, stems_n, biom_stem, biom_root, biom_foliage
    # species from species raster, planted date from disturbance data
    # fertility? stems_n? Biomass?

    sp.columns = c("species","planted","fertility", "stems_n", "biom_stem","biom_root","biom_foliage")
    species = data.frame(matrix(nrow = 1, ncol = length(sp.columns)))
    colnames(species) = sp.columns

    species$species <- 'White Spruce' # should come from species raster
    species$planted <- '1985-01' # should come from disturbance raster
    species$fertility <- 0.3 # should come from soil - generally low fertility from literature
    species$stems_n <- 4000 # literature
    species$biom_stem <- 6 # literature
    species$biom_root <- 3 # literature
    species$biom_foliage <- 1 # literature


    ##########################################################
    # thinning
    # any thinning events? Unmanaged - so no

    ##########################################################
    # d_sizedist - currently unused

    ##########################################################

    ############################################################
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

    #head( out_3PG )

    #sel_var <- c('npp', 'height')

    # out_3PG %>%
    #   filter( variable %in% sel_var ) %>%
    #   ggplot( aes(date, value, color = species) ) +
    #   geom_line() +
    #   facet_wrap(~variable, scales = 'free') +
    #   theme_classic()

    # Need to sum GPP over the growth period and add it up
    npp_select <- filter(out_3PG, variable == "npp")
    npp_sum <- sum(npp_select$value)
    npp.df <- rbind(npp.df, npp_sum)

    # for LAI and other variables we only want the last value of the growing period?
    lai_select <- out_3PG[out_3PG$date == '2020-12-31' & out_3PG$variable == 'lai', 5]
    lai.df <- rbind(lai.df, lai_select)

    dbh_select <- out_3PG[out_3PG$date == '2020-12-31' & out_3PG$variable == 'dbh', 5]
    dbh.df <- rbind(dbh.df, dbh_select)

    result_df[i, "dbh"] <- dbh_select

}
return(result_df)
}

#################################################################################


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
        # SITE data
        #site <- first_row_data %>% select(c('Latitude', 'Elevation')) %>% rename(latitude = Latitude, altitude = Elevation)

       # site <- inputs.df[i,]
        # site <- site %>% select(c('dem')) %>% rename(altitude = dem)

        #site$latitude <- inputs.df[i,1] # this is now being pulled for each pixel

        #site$soil_class <- 3 # Site factors will include data from other rasters
        #site$asw_i <-999 #
        #site$asw_min <- 0
        #site$asw_max <- 300


        # simulation from and to!
        #site$from <- '2000-01'
        #site$to <- '2019-12'

        # We need to organize this and put things in the correct order
        site <- site[, c(2, 1, 3, 4, 5, 6, 7, 8)]

        #prepare_site(site)

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


       #species$planted <- paste(inputs.df[i,4],"-6", sep = "") # should now come from the AGE raster!
        #### set it to 50 years here
        species$planted <- '1969-12' ###

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
        final_year_npp <- out_3PG %>% filter(year == 2019 & variable == "npp")
        # then, use the 'sum' function to add up all of the npp values for the final year
        total_npp <- sum(final_year_npp$value)

        return(data.frame("dbh" = dbh_select, "lai" = lai_select, "npp" = total_npp,
                          "basal_area" = basal_select))
    }) %>% bind_rows()

    return(test_df)
}

# cl = detectCores()/2 %>% makeCluster()
cl = makeCluster(20) # number of cores
clusterEvalQ(cl, {library(r3PG); library(dplyr); library(readxl); library(data.table); library(tidyr); library(tidyverse); library(lubridate)})  # source also works
clusterExport(cl, varlist = c("climate_df2", "full_comb_clean", "f_loc", "parameters"))


result <- Calculate_3PG_Y(climate_df2, full_comb_clean, cl = cl)

result_df <- bind_rows(result)
stopCluster(cl)

# 9ish seconds for 1000 rows (20 cores)
# 1:19 for 10 000 rows (20 cores)

###############################################################################################################
test.df <- out_3PG[out_3PG$date == '2019-12-31', ]
