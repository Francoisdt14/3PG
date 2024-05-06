# Load the required packages
library(terra)

# Load the raster data
raster1 <- rast("path/to/raster1.tif")  # Biomass values for species A
raster2 <- rast("path/to/raster2.tif")  # Biomass values for species B

raster3 <- rast("path/to/raster3.tif")  # Wetness index

# Create a new raster to store the combined values
new_raster <- rast(raster1)

# Iterate through the pixels and assign values based on the wetness index
for (i in 1:ncell(new_raster)) {
    if (raster3[i] > 0.5) {
        new_raster[i] <- raster1[i]
    } else {
        new_raster[i] <- raster2[i]
    }
}

# Save the new raster
writeRaster(new_raster, "path/to/new_raster.tif", overwrite = TRUE)


######################################################################################################
# TEST
#######################################################################################################
# PREPROCESS

#######################################################################################
# TESTING
#######################################################################################
library(tidyverse)
library(terra)

# Load the masks
Sh_mask <- rast("D:/BP_Layers/U_13N/landcover/Sh_90m.tif")

# Load in the stem and foliage values at 2080 for each scenario
dec.ws <- rast("I:/data_2024_05_02_deciduous/U_13N/S2_dec/Y4_output_test/ws.flt")
dec.wf <- rast("I:/data_2024_05_02_deciduous/U_13N/S2_dec/Y4_output_test/wf.flt")

# calculate agb for each scenario in 2080
dec1 <- dec.ws + dec.wf

# Get the EPSG code from fao.mask
#epsg_code <- crs(fao_mask, describe = T)$code
# Set the CRS of s1 using the extracted EPSG code
#crs(s1) <- paste0("EPSG:", epsg_code)
crs(dec1) <- crs(Sh_mask)

dec1_mask <- terra::mask(dec1, Sh_mask)

con.ws <- rast("D:/BP_Layers/U_13N/biomass_3PG/S2/Y4_output_test/ws208007.flt")
con.wf <- rast("D:/BP_Layers/U_13N/biomass_3PG/S2/Y4_output_test/wf208007.flt")

con1 <- con.ws + con.wf
crs(con1) <- crs(Sh_mask)

con1_mask <- terra::mask(con1, Sh_mask)

hist(con1_mask)
hist(dec1_mask)


# Load the raster data
raster1 <- con1_mask  # Biomass values for SPRUCE
raster2 <- dec1_mask #rast("path/to/raster2.tif")  # Biomass values for DECID

raster3 <- rast("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all/scaled_TWI_noNA.tif")  # Wetness index

raster3.mask <- mask(raster3, Sh_mask)

# Scale the wetness index raster to 0-1 range
raster3.sc <- (raster3.mask - minmax(raster3.mask)[1]) / (minmax(raster3.mask)[2] - minmax(raster3.mask)[1])

# Create a new raster to store the combined values
new_raster <- raster1

# Define the wetness thresholds
wet_threshold <- 0.67
medium_threshold <- 0.33

#### this doesn't work ...

# Scenario 1: Species A in wet, Species B in rest
for (i in 1:ncell(new_raster)) {
    if (raster3.sc[i] > wet_threshold) {
        new_raster[i] <- raster1[i]
    } else {
        new_raster[i] <- raster2[i]
    }
}

plot(new_raster)


tic()
# Create a new raster using conditional assignment with ifel()
new_raster <- ifel(raster3.sc <= wet_threshold, raster1,
                   raster2)
toc()



# Scenario 2: Species A in wet and medium, Species B in rest
for (i in 1:ncell(new_raster)) {
    if (raster3[i] > medium_threshold) {
        new_raster[i] <- raster1[i]
    } else {
        new_raster[i] <- raster2[i]
    }
}

tic()
# Create a new raster using conditional assignment with ifel()
new_raster <- ifel(raster3.sc <= medium_threshold, raster1,
                   raster2)
toc()


###############################################################################

# Define the wetness thresholds
wet_threshold <- 0.67
medium_threshold <- 0.33

# Scenario 3: Species A in wet, Species B in dry, medium randomly assigned
for (i in 1:ncell(new_raster)) {
    if (raster3[i] > wet_threshold) {
        new_raster[i] <- raster1[i]
    } else if (raster3[i] < (1 - wet_threshold)) {
        new_raster[i] <- raster2[i]
    } else {
        new_raster[i] <- sample(c(raster1[i], raster2[i]), 1)
    }
}


new_raster3 <- raster1

# Assign values based on conditions (avoiding ifelse)
new_raster3[] <- raster2[raster3.sc > wet_threshold]  # Wet gets decid
new_raster3[] <- raster1[raster3.sc < medium_threshold]  # Dry gets spruce
#new_raster3[] <- sample(c(raster1[], raster2[]), size=length(raster3.sc) - sum(!is.na(new_raster[])), replace=TRUE)  # Random in medium

# Assuming you have raster1 and raster2 already loaded
r1 <- raster1
r2 <- raster2

# Combine the two rasters into a single stack
r_stack <- c(r1, r2)
# Get the number of cells in the rasters
n_cells <- ncell(r_stack)
# Extract the values from the stack, skipping NAs
r_values <- values(r_stack, na.rm = TRUE)
# Randomly sample the non-NA cell indices
sample_idx <- sample(1:length(r_values), length(r_values), replace = TRUE)
# Create the random raster
raster.rand <- rast(r1)
raster.rand[!is.na(raster.rand)] <- r_values[sample_idx]

# Fill the values between the thresholds with the random raster
new_raster3[] <- raster.rand[raster3.sc >= medium_threshold & raster3.sc <= wet_threshold]







###########################################################################
# Assuming you have raster1 and raster2 already loaded
r1 <- raster1
r2 <- raster2
r3 <- raster3.sc
# Combine the two rasters into a single stack

r_stack <- c(r1, r2, r3)

# Extract the values from the stack, skipping NAs
r_values <- values(r_stack, na.rm = TRUE)

test = r_values %>% as.data.frame()


test$random = apply(test[, 1:2], MARGIN = 1, FUN = function(rw){sample(rw, 1)})

head(test)

# Create the new column
test$select <- ifelse(test$`TWI_U_13N` > 0.8, test$ws,
                          ifelse(test$`TWI_U_13N` < 0.2, test$`ws207007`,
                                 test$random))


# FISX THIS
# Create the new column
test$wet <- if(test$`TWI_U_13N` > 0.8, test$ws,
                   else(test$`ws207007`))


head(test)

raster.rand <- raster1

raster.rand[!is.na(raster.rand)] <- test$select





