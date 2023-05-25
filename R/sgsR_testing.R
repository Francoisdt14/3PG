
dem <- rast("D:/BP_Layers/outputs/inputs/dem_crop_M_9S.tif")

dem_crop <- crop(dem, volume_3pg)

species <- rast("D:/BP_Layers/outputs/inputs/leading-species_2019_2.tif")

species_crop <- crop(species, volume_3pg)

age <- rast("D:/BP_Layers/outputs/inputs/Forest_Age_2019.tif")

age_crop <- crop(age, volume_3pg)

tmax08 <- rast("D:/BP_Layers/outputs/climate/Tmax08.tif")

tmax08_crop <- crop(tmax08, volume_3pg)

layer_stack <- c(dem_crop, species_crop, age_crop, tmax08_crop)

# Set new names for the raster layers
new_names <- c("dem", "species", "age", "tmax08")

# Change the names of the raster layers in the stack
names(layer_stack) <- new_names

##### We have the following rasters for 3PG and NTEMS:

basal_3pg
biomas_3pg
volume_3pg

basal_ntems
biomas_ntems
volume_ntems

basal_3pg_NORM
biomass_3pg_NORM
volume_3pg_NORM

basal_ntems_NORM
biomass_ntems_NORM
volume_ntems_NORM


##############################
# sgsR

#--- Load mraster from internal data ---#
r <- system.file("extdata", "mraster.tif", package = "sgsR")

#--- load mraster using the terra package ---#
mraster <- terra::rast(r)


#--- apply kmeans algorithm to metrics raster ---#
sraster <- strat_quantiles(
    mraster = mraster$zq90, # use mraster as input for sampling
    nStrata = 4, # algorithm will produce 4 strata
    plot = TRUE
) # algorithm will plot output












ls <- layer_stack[[c(1,4)]]

sp <- layer_stack[[2]]


qt_strat <- strat_quantiles(layer_stack[[1]], nStrata = 5, plot = TRUE)

qt_breaks <- strat_breaks(layer_stack[[1]], breaks = c(700,1000,1500,2000), plot = TRUE)

#qt_sp <- c(qt_strat,sp)

#mapped <- strat_map(qt_sp, stack = TRUE, plot = TRUE)

unique(values(mapped))

s <- sample_strat(sraster = mapped$strata, nSamp = 200, plot= TRUE)

sample_strat(sraster = qt_breaks, nSamp = 100, plot=TRUE, allocation = "equal", mindist = 100)

sample_systematic(raster = ls[[1]], cellsize = 1000, square = FALSE, plot = TRUE)


s <- extract_metrics(mraster = bio, existing = s)

s %>%
    group_by(strata) %>%
    summarise(mean = mean(ntems_bio))


bio <- c(biomas_ntems, biomas_3pg)
names(bio) <- c("ntems_bio","3pg_bio")







