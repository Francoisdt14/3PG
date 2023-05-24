###############
## libraries ##
###############

#--- define libraries ---#
packages <- c('tidyverse','sf', 'sp','terra','sgsR','rmdformats','rmarkdown', 'r3PG', "data.table", "silvR21", "parallel",
              "pbapply", "tictoc", "readxl", "arrow", "furrr", "lubridate", "viridis", "sgsR")

installed_packages <- packages %in% rownames(installed.packages())

#--- install if needed ---#
if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
}

#--- load libs ---#
lapply(packages, library, character.only = TRUE) |>
    invisible()
