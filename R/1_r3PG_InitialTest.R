# r3PG Test Script - testing out r3PG with default parameters and functions
# Get an idea of what is needed for first run of 3PG

library(tidyverse)
library(r3PG)
library(dplyr)
library(ggplot2)
library(readxl) # for working with excel datasheets
library(multidplyr)

# Using basic examples with internal data
# Important to double check which settings are used

# check data input
prepare_input( site = d_site, species = d_species, climate = d_climate, d_thinning)

# main function
out_3PG <- run_3PG(
  site        = d_site,
  species     = d_species,
  climate     = d_climate,
  thinning    = d_thinning,
  parameters  = d_parameters,
  size_dist   = d_sizeDist,
  settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                     height_model = 1, correct_bias = 0, calculate_d13c = 0),
  check_input = TRUE, df_out = TRUE)

head( out_3PG )


# Plotting the data different parameters

sel_var <- c('biom_stem', 'biom_foliage', 'biom_root', 'height')

out_3PG %>%
  filter( variable %in% sel_var ) %>%
  ggplot( aes(date, value, color = species) ) +
  geom_line() +
  facet_wrap(~variable, scales = 'free') +
  theme_classic()


i_var <- c('stems_n',  'dbh', 'height', 'biom_stem', 'biom_root', 'biom_foliage')
i_lab <- c('Stem density', 'DBH', 'Height', 'Stem biomass', 'Root biomass', 'Foliage biomass')

out_3PG %>%
  filter(variable %in% i_var) %>%
  mutate(variable = factor(variable, levels = i_var)) %>%
  ggplot( aes(date, value))+
  geom_line( aes(color = species), size = 0.5)+
  facet_wrap( ~ variable, scales = 'free_y', ncol = 3,
              labeller = labeller(variable = setNames(i_lab, i_var) )) +
  scale_color_brewer('', palette = 'Dark2') +
  theme_classic()+
  theme(legend.position="bottom")+
  xlab("Calendar date") + ylab('Value')

#######################################################################################

#### Using excel sheets - example data that matches above can be found at:
# https://github.com/trotsiuk/r3PG/tree/master/pkg/data-raw

f_loc <- # 'filepath.../vignette_data/data.input.xlsx'

out_3PG2 <- run_3PG(
  site        = read_xlsx(f_loc, 'site'),
  species     = read_xlsx(f_loc, 'spruce'),
  climate     = read_xlsx(f_loc, 'climate2'),
  #thinning    = read_xlsx(f_loc, 'thinning'),
  parameters  = read_xlsx(f_loc, 'parameters'),
  #size_dist   = read_xlsx(f_loc, 'sizeDist'),
  settings    = list(light_model = 1, transp_model = 1, phys_model = 1,
                     height_model = 1, correct_bias = 0, calculate_d13c = 0),
  check_input = TRUE, df_out = TRUE)


# Plotting the data

sel_var <- c('height', 'gpp', 'height_rel', 'dbh' )

out_3PG2 %>%
  filter( variable %in% sel_var ) %>%
  ggplot( aes(date, value, color = species) ) +
  geom_line() +
  facet_wrap(~variable, scales = 'free') +
  theme_classic()

height_output <- out_3PG2 %>%
  filter( variable %in% sel_var )

height_output <- out_3PG2 %>%
  filter( variable %in% sel_var )

#######################################################################################
