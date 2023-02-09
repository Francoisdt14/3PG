###############
## libraries ##
###############

#--- define libraries ---#
packages <- c('tidyverse','sf','terra','sgsR','rmdformats','rmarkdown')

installed_packages <- packages %in% rownames(installed.packages())

#--- install if needed ---#
if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
}

#--- load libs ---#
lapply(packages, library, character.only = TRUE) |>
    invisible()
