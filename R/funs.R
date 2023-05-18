###############
## functions ##
###############


Calculate_3PG <- function(climate.df, inputs.df, cl = NA) {
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
        site$from <-paste(inputs.df[i,4],"-6", sep = "")
        #site$from <- '1969-12'
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

        biom_stem_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'biom_stem', 5]
        biom_foliage_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'biom_foliage', 5]
        biom_root_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'biom_root', 5]

        volume_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'volume', 5]

        out_3PG$year <- lubridate::year(out_3PG$date)
        # filter the data for the final year and the 'npp' variable
        final_year_npp <- out_3PG %>% filter(year == 2019 & variable == "npp")
        # then, use the 'sum' function to add up all of the npp values for the final year
        total_npp <- sum(final_year_npp$value)

        return(data.frame("dbh" = dbh_select,
                          "lai" = lai_select,
                          "basal_area" = basal_select,
                          "biom_stem" = biom_stem_select,
                          "biom_foliage" = biom_foliage_select,
                          "biom_root" = biom_root_select,
                          "volume" = volume_select,
                          "npp" = total_npp))
    }) %>% bind_rows()

    return(test_df)
}


######################################################################

Calculate_3PG_50 <- function(climate.df, inputs.df, cl = NA) {
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
        site$from <- '1969-06'
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
        species$planted <- '1969-06' ###

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

        biom_stem_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'biom_stem', 5]
        biom_foliage_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'biom_foliage', 5]
        biom_root_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'biom_root', 5]
        volume_select <- out_3PG[out_3PG$date == '2019-12-31' & out_3PG$variable == 'volume', 5]

        out_3PG$year <- lubridate::year(out_3PG$date)
        # filter the data for the final year and the 'npp' variable
        final_year_npp <- out_3PG %>% filter(year == 2019 & variable == "npp")
        # then, use the 'sum' function to add up all of the npp values for the final year
        total_npp <- sum(final_year_npp$value)

        return(data.frame("dbh" = dbh_select,
                          "lai" = lai_select,
                          "basal_area" = basal_select,
                          "biom_stem" = biom_stem_select,
                          "biom_foliage" = biom_foliage_select,
                          "biom_root" = biom_root_select,
                          "volume" = volume_select,
                          "npp" = total_npp))

    }) #%>% bind_rows()

    return(test_df)
}
