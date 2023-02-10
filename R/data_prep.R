# Data Preparation
# Prepare datasets required by r3PG for each tile:
#   site
#   species
#   climate
#   thinning
#   parameters
#   size_dist

source("R/lib.R")

climate_df2 <- read.csv("D:/BP_Layers/outputs/crops/climate/033.csv") %>% na.omit()
inputs_df2 <- read.csv("D:/BP_Layers/outputs/crops/inputs/033.csv") %>% na.omit()

###  UPDATE LAT FOR EACH RASTER!!!

# Initialize an empty data table to store values for each cropped area

npp.df <- data.table()


tic('total')
for(i in 1:nrow(climate_df2)) {

    #nrow(climate_df)){
    ##########################################################
    # CLIMATE #
    # d_climate requires: month, tmp_min, tmp_max, tmp_ave,  prcp,  srad, frost_days
    # all available from climateNA data
    days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    first_row_data <- climate_df2[i,]

    # Select the appropriate columns:
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

    site <- inputs_df2[i,]
    site <- site %>% select(c('dem_crop_M_9S')) %>% rename(altitude = dem_crop_M_9S)

    site$latitude <- lat

    site$soil_class <- 3 #
    site$asw_i <-999
    site$asw_min <- 0
    site$asw_max <- 300


    # simulation from!
    site$from <- '2000-01'
    site$to <- '2020-12'

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
    species$fertility <- 0.5 # should come from.... soil?
    species$stems_n <- 4000 # arbitrary
    species$biom_stem <- 6 # arbitrary
    species$biom_root <- 3 # arbitrary
    species$biom_foliage <- 1 # arbitrary


    ##########################################################
    # thinning
    # any thinning events? Unmanaged so no?

    ##########################################################
    # d_sizedist
    # ??

    ##########################################################
    # parameters
    # need parameters for every species

    f_loc <- 'D:/r3PG/vignette_data/data.input2.xlsx'
    parameters <-  read_xlsx(f_loc, 'parameters')

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

}
toc()

####################
# append npp.df to an empty raster?

npp.df.033 <- npp.df

colnames(npp.df.033)[1]  <- "NPP"    # change column name for x column

write.csv(npp.df.033, "D:/BP_Layers/outputs/crops/npp/npp_033.csv")
