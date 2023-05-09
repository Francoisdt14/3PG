######################
#### TEST FUNCTION ###
######################
# First iteration of the run3PG function including reshaping some of the dataframes

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
