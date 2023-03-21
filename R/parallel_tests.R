# Datasets required by r3PG for each tile:
#   site
#   species
#   climate
#   thinning
#   parameters
#   size_dist

source("R/lib.R")

# TESTING PARALLEL SCRIPTS

# subset data from 7...

climate_df2 <- climate_df[1:500,]
inputs_df2 <- inputs_df[1:500,]

# Here is the current function...
# This is the same function as what is stored in 'test_fun.R'

Calculate_3PG_TEST <- function(climate_df, input_df) {

   test_df <- data.frame(dbh = numeric(nrow(climate_df2)))

   # Currently using customized parameters (but can use parameters directly from the vignette)
    f_loc <- './data/input/data.input2.xlsx'
    parameters <-  read_xlsx(f_loc, 'parameters')

    for(i in 1:nrow(climate_df2)){

        ##########################################################
        # CLIMATE #
        # d_climate requires: month, tmp_min, tmp_max, tmp_ave,  prcp,  srad, frost_days
        # all available from climateNA data

        days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

        first_row_data <- climate_df2[i,]

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
       #site <- first_row_data %>% select(c('Latitude', 'Elevation')) %>% rename(latitude = Latitude, altitude = Elevation)

        site <- inputs_df2[i,]
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

        dbh_select <- out_3PG[out_3PG$date == '2020-12-31' & out_3PG$variable == 'dbh', 5]

        test_df[i, "dbh"] <- dbh_select

    }

    return(test_df)
}

# Here we run the function.. it takes

tic(); test_3PG <- Calculate_3PG_TEST(climate_df2, inputs_df2); toc()

# It takes 39ish seconds to run 500 rows.

################################################################################
################################################################################

# cl = detectCores()/2 %>% makeCluster()
cl = makeCluster(5) # number of cores
clusterEvalQ(cl, {library(r3PG); library(dplyr); library(readxl); library(data.table); library(tidyr); library(tidyverse); var.x = 5})  # source also works
clusterExport(cl, varlist = c("climate_df2", "inputs_df2"))

result_list <- parLapply(cl, 1:nrow(climate_df2), function(i) {
        # Define the Calculate_3PG1 function within the worker process

    Calculate_3PG3 <- function(climate_df, input_df) {

        test_df <- data.frame(dbh = numeric(nrow(climate_df2)))

        # Currently using customized parameters (but can use parameters directly from the vignette)
        f_loc <- './data/input/data.input2.xlsx'
        parameters <-  read_xlsx(f_loc, 'parameters')

        for(i in 1:nrow(climate_df2)){

            ##########################################################
            # CLIMATE #
            # d_climate requires: month, tmp_min, tmp_max, tmp_ave,  prcp,  srad, frost_days
            # all available from climateNA data

            days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

            first_row_data <- climate_df2[i,]

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
            #site <- first_row_data %>% select(c('Latitude', 'Elevation')) %>% rename(latitude = Latitude, altitude = Elevation)

            site <- inputs_df2[i,]
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

            dbh_select <- out_3PG[out_3PG$date == '2020-12-31' & out_3PG$variable == 'dbh', 5]

            test_df[i, "dbh"] <- dbh_select

        }

        return(test_df)
    }

    # Call the Calculate_3PG function with the current row of the dataframes
    result <- Calculate_3PG3(climate_df2[i, ], inputs_d2f[i, ])

    return(result)
})

result_df <- do.call(rbind, result_list)
stopCluster(cl)






################################################################################


################################################################################
################################################################################


# THIS DOESN"T WORK

# climate_df2 <- climate_df[1:500,]
# inputs_df2 <- inputs_df[1:500,]


library(parallel)

# define the number of cores to use
num_cores <- 6

# split the input dataframes into chunks based on the number of cores
df_A_chunks <- split(climate_df2, 1:num_cores)
df_B_chunks <- split(inputs_df2, 1:num_cores)

# create a cluster object to use for parallelization
cl <- makeCluster(num_cores)

# load the script containing the custom function
source("R/test_fun.R")


# register the custom function with the cluster
clusterExport(cl, "Calculate_3PG_TEST")

# parallelize the function across the chunks of data
df_Output_chunks <- parLapply(cl, 1:num_cores, function(i) {
    Calculate_3PG_TEST(df_A_chunks[[i]], df_B_chunks[[i]])
})

# stop the cluster
stopCluster(cl)

# combine the output chunks into a single dataframe
df_Output <- do.call(rbind, df_Output_chunks)





################################################################################
################################################################################




# Use a list?


library(parallel)

# Define function to be parallelized
Calculate_3PG3 <- function(input_list) {
    # Unpack input list into separate data frames
    climate_df2 <- input_list[[1]]
    inputs_df2 <- input_list[[2]]

    # Initialize empty data frame to store results
    result_df <- data.frame(dbh = numeric(nrow(climate_df2)))

    # Loop over rows in climate_df and inputs_df
    for (i in 1:nrow(climate_df2)) {

        days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

        first_row_data <- climate_df2[i,]
        #Select the appropriate columns and rearrange for the correct input:
        first_row_select <- first_row_data %>% #select(Tmax01:PPT12, NFFD01:NFFD12) %>%
            pivot_longer(cols = c(NFFD01:Tmin12)) %>%
            separate(name, into = c("Var", "month"), sep = -2) %>%
            pivot_wider(names_from = Var, values_from = value) %>%
            mutate(frost_days = days - NFFD,
                   month = as.numeric(month)) %>%
            select(-NFFD) %>%
            as.data.table() %>% rename(prcp = PPT, srad = Rad, tmp_max = Tmax, tmp_min = Tmin)

        site <- inputs_df2[i,]
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

        # Species
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

        ############################################################
        # MAIN FUNCTION

        out_3PG <- run_3PG(
            site        = site,
            species     = species,
            climate     = first_row_select,
            parameters  = parameters,
            settings    = list(light_model = 1, transp_model = 1, phys_model = 1,
                               height_model = 1, correct_bias = 0, calculate_d13c = 0),
            check_input = TRUE, df_out = TRUE)

        dbh_select <- out_3PG[out_3PG$date == '2020-12-31' & out_3PG$variable == 'dbh', 5]

        result_df[i, "dbh"] <- dbh_select

    }

    return(result_df)
}

# Generate sample data
climate_df2 <- data.frame(temperature = rnorm(500), precipitation = rnorm(500))
inputs_df2 <- data.frame(var1 = rnorm(500), var2 = rnorm(500))
# climate_df2 <- climate_df[1:500,]
# inputs_df2 <- inputs_df[1:500,]

# Combine data frames into a list
input_list <- list(climate_df2, inputs_df2)

# Parallelize function using parLapply
cl <- makeCluster(5)
clusterExport(cl, list("Calculate_3PG3", "input_list"))
results <- parLapply(cl, input_list, Calculate_3PG3)
stopCluster(cl)

# Combine results into a single data frame
result_df <- do.call(rbind, results)


}

#####################################################################################################################
# CHRIS EXAMPLE

# set up parallel cluster
library(parallel)
# cl = detectCores()/2 %>% makeCluster()
cl = makeCluster(5) # number of cores

clusterEvalQ(cl, {library(r3PG); library(tidyverse); library(readxl); var.x = 5})  # source also works
clusterExport(cl, varlist = c("climate_df2", "inputs_df2"))

output.list = pbapply::pblapply(1:nrow(climate_df2), FUN = function(i){

    Calculate_3PG3 <- function(climate_df2, input_df2) {

        test_df <- data.frame(dbh = numeric(nrow(climate_df2)))

        # Currently using customized parameters (but can use parameters directly from the vignette)
        f_loc <- './data/input/data.input2.xlsx'
        parameters <-  read_xlsx(f_loc, 'parameters')

        for(i in 1:nrow(climate_df2)){

            ##########################################################
            # CLIMATE #
            # d_climate requires: month, tmp_min, tmp_max, tmp_ave,  prcp,  srad, frost_days
            # all available from climateNA data

            days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

            first_row_data <- climate_df2[i,]

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
            #site <- first_row_data %>% select(c('Latitude', 'Elevation')) %>% rename(latitude = Latitude, altitude = Elevation)

            site <- inputs_df2[i,]
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

            dbh_select <- out_3PG[out_3PG$date == '2020-12-31' & out_3PG$variable == 'dbh', 5]

            test_df[i, "dbh"] <- dbh_select

        }

        return(test_df) # Return a row instead of r-binding to a current thing
    }, cl = cl) %>%

    rbindlist() # bind all elements in list


stopCluster(cl)










#################################################################################
Calculate_3PG_X <- function(climate_df, inputs_df, cl = NA) {
    days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    clim.reshape = climate_df %>% #select(Tmax01:PPT12, NFFD01:NFFD12) %>%
        mutate(., id = 1:nrow(.)) %>%
        pivot_longer(cols = c(NFFD01:Tmin12)) %>%
        separate(name, into = c("Var", "month"), sep = -2) %>%
        pivot_wider(names_from = Var, values_from = value, id_expand = T) %>%
        mutate(frost_days = days - NFFD,
               month = as.numeric(month)) %>%
        select(-NFFD) %>%
        as.data.table() %>%
        rename(prcp = PPT, srad = Rad, tmp_max = Tmax, tmp_min = Tmin)

    # Currently using customized parameters (but can use parameters directly from the vignette)

    test_df = pblapply(cl = cl, 1:7000, FUN = function(i){
        ##########################################################
        # CLIMATE #
        # d_climate requires: month, tmp_min, tmp_max, tmp_ave,  prcp,  srad, frost_days
        # all available from climateNA data
        # tic()
        first_row_select = filter(clim.reshape, id == i) %>% select(-id)

        ##########################################################
        # SITE data
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
        # toc()
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

        dbh_select <- out_3PG[out_3PG$date == '2020-12-31' & out_3PG$variable == 'dbh', 5]

        return(data.frame("dbh" = dbh_select))
    }) %>% bind_rows()

    return(test_df)
}
# cl = detectCores()/2 %>% makeCluster()
cl = makeCluster(20) # number of cores
clusterEvalQ(cl, {library(r3PG); library(dplyr); library(readxl); library(data.table); library(tidyr); library(tidyverse)})  # source also works
clusterExport(cl, varlist = c("climate_df", "inputs_df", "f_loc", "parameters"))

result <- Calculate_3PG_X(climate_df, inputs_df, cl = cl)

result_df <- bind_rows(result)
stopCluster(cl)


#############################################################





#################################################################################
Calculate_3PG_Y <- function(climate_df, inputs_df, cl = NA) {
    # Currently using customized parameters (but can use parameters directly from the vignette)

    test_df = pblapply(cl = cl, 1:7000, FUN = function(i){
        ##########################################################
        # CLIMATE #
        # d_climate requires: month, tmp_min, tmp_max, tmp_ave,  prcp,  srad, frost_days
        # all available from climateNA data

        days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        first_row_data <- climate_df[i,]

        #Select the appropriate columns and rearrange for the correct input:
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

        dbh_select <- out_3PG[out_3PG$date == '2020-12-31' & out_3PG$variable == 'dbh', 5]

        return(data.frame("dbh" = dbh_select))
    }) %>% bind_rows()

    return(test_df)
}
# cl = detectCores()/2 %>% makeCluster()
cl = makeCluster(20) # number of cores
clusterEvalQ(cl, {library(r3PG); library(dplyr); library(readxl); library(data.table); library(tidyr); library(tidyverse)})  # source also works
clusterExport(cl, varlist = c("climate_df", "inputs_df", "f_loc", "parameters"))

result <- Calculate_3PG_Y(climate_df, inputs_df, cl = cl)

result_df <- bind_rows(result)
stopCluster(cl)
