library(terra)
library(tidyverse)

# year trees were planted
planted <- rast("D:/BP_Layers/M_9S/3PG_flt/5_90m_inputs_all/Forest_Age_2019_2025.tif")

# age in 2019
age.2019 <- 2019 - planted



# Ages in 2041, 2061, 2081, 2101
age.2041 <- age.2019 + 22
age.2061 <- age.2019 + 42
age.2081 <- age.2019 + 62
age.2101 <- age.2019 + 82

# writeRaster(age.2041, "D:/3PG_Cplusplus/future_forest_age_input_TEST/future_ages_2/age2041.flt", datatype = "FLT4S", overwrite = TRUE)

# Ages in 2040, 2060, 2080 for input

age.2040 <- age.2019 + 21
age.2060 <- age.2019 + 41
age.2080 <- age.2019 + 61

writeRaster(age.2080, "D:/3PG_Cplusplus/future_forest_age_input_TEST/future_ages_3/age2080.flt", datatype = "FLT4S", overwrite = TRUE)


# biomass inputs into the future model
wf.input <- rast('D:/3PG_Cplusplus/future_forest_age_input_TEST/wf.flt')
ws.input <- rast('D:/3PG_Cplusplus/future_forest_age_input_TEST/ws.flt')
wr.input <- rast('D:/3PG_Cplusplus/future_forest_age_input_TEST/wr.flt')

global(ws.input, c("max", "min", "mean"), na.rm = T)

# first output from the future model
wf.2020 <- rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/wf202007.flt")
ws.2020  <-  rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/ws202007.flt")
wr.2020 <-  rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/wr202007.flt")

global(ws.2020, c("max", "min", "mean"), na.rm = T)

# 2030 output from the future model
wf.2030 <- rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/wf203007.flt")
ws.2030  <-  rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/ws203007.flt")
wr.2030 <-  rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/wr203007.flt")

global(ws.2030, c("max", "min", "mean"), na.rm = T)

par(mfrow = c(3, 1))
plot(ws.input, main = "stem biomass input")
plot(ws.2020, main = "stem biomass 2020")
plot(ws.2030, main = "stem biomass 2030")

par(mfrow = c(1, 1))

##########

planted.df <- as.data.frame(planted)

planted.df$focal_mean <- as.integer(round(planted.df$focal_mean))

unique_values <- unique(planted.df$focal_mean)

num_unique_values <- length(unique_values)




test <- rast("D:/3PG_Cplusplus/future_forest_age_input_TEST/ws202007.flt")
plot(test)
#global(test, c("max", "min", "mean"), na.rm = T)
crs(test) <- crs(planted)

#writeRaster(test, "D:/3PG_Cplusplus/future_forest_age_input_TEST/stemno202007.flt", datatype = "FLT4S")


test <- rast("D:/3PG_Cplusplus/future_forest_age_input_TEST/ws202007.flt")

test2 <- rast("D:/3PG_Cplusplus/output_M_9S_lp_futureTEST/ws202007.flt")


plot(test2)

global(test, c("max", "min", "mean"), na.rm = T)
global(test2, c("max", "min", "mean"), na.rm = T)




#############################################


wf <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/wf.flt")

crs(wf) <- crs(planted)

wr <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/wr.flt")

crs(wr) <- crs(planted)

ws <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/ws.flt")

crs(ws) <- crs(planted)

stemno <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/stemno.flt")
crs(stemno) <- crs(planted)

asw <- rast("D:/3PG_Cplusplus/output_M_9S_lp_Y4_S2/asw.flt")
crs(asw) <- crs(planted)

writeRaster(ws, "D:/3PG_Cplusplus/future_forest_age_input_TEST/Y4_S2/ws.flt")

######################################


library(stringr)

# Set the directory where the .hdr and .flt files are located

#directory <- "Y:/Francois/flt_test_100_noNA"
directory <- "D:/3PG_Cplusplus/future_forest_age_input_TEST/Y4_S2"
# Get the list of .hdr files in the directory
hdr_files <- list.files(directory, pattern = "\\.hdr$", full.names = TRUE)

# very important to write this correctly depending on raster size!!

# Process each .hdr file
for (hdr_file in hdr_files) {
  # Define the new content for the .hdr file
  new_content <- c(
    "NROWS          3249", # 90m = 3249 , 30m = 9745
    "NCOLS          3249", # 90m 3249  , 30m 9746
    "xllcenter         403650.448601884", # 90m = 403650.448601884 , 30m = 403619.663820003
    "yllcenter         6312667.21413766", # 90m = 6312667.21413766" , 30m = 6312636.42935578"
    "cellsize           92.35435", # 90m = 92.35435 , 30m = 30.78478
    "nodata_value -9999.000000",
    "byteorder lsbfirst"
  )

  # Write the new content to the .hdr file, overwriting the existing contents
  writeLines(new_content, hdr_file)
}




global(ws, c("mean", "max", "min", "sd", "rms"), na.rm=TRUE)


###############################################################################
wf <- rast("D:/3PG_Cplusplus/_delete/new/wf202007.flt")
ws <- rast("D:/3PG_Cplusplus/_delete/new/ws202007.flt")
tot <- wf + ws

crs(tot) <- crs(planted)

planted_NA <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/Forest_Age_2019_withNA.flt")

tot_mask <- mask(tot, planted_NA)

#writeRaster(tot_mask, "D:/3PG_Cplusplus/_delete/new/biom_2020.tif")


current <- rast("D:/3PG_Cplusplus/_delete/new/biom_2100_currentT.tif")
S2 <- rast("D:/3PG_Cplusplus/_delete/new/biom_2100_s2.tif")

diff <- S2 - current

diff[diff<10] <- NA

#writeRaster(diff, "D:/3PG_Cplusplus/_delete/new/S2_current_DIFF_10plus.tif")

landcover <- rast("D:/BP_Layers/M_9S/landcover/landcover_90m.tif")

x <- droplevels(landcover) # get rid of un-necessary data
xnum <- as.numeric(x) # convert the raster to numeric - need to keep an eye on which codes we want
xnum[!xnum %in% c(81, 210, 220, 230, 40, 50, 100)] <- NA # here we want to keep the forested landcover types, so set everything that isn't to NA
xnum[!is.na(xnum)] <- 1 # set the forested landcovers to 1

#plot(xnum, col = "darkgreen")
mask_crop <- terra::crop(xnum, landcover)

#writeRaster(mask_crop, "D:/BP_Layers/M_9S/landcover/tree_plus_40_50_100_90m.tif", datatype = "INT1U", overwrite = T) # write this mask as it will be used moving forward!


landcover_1 <- droplevels(landcover)
landcover_1 <- as.numeric(landcover_1)
landcover_1[!landcover_1 %in% c(81, 210, 220, 230)] <- NA # here we want to keep the forested landcover types, so set everything that isn't to NA
landcover_1[!is.na(landcover_1)] <- 1

plot(landcover_1)
#writeRaster(landcover_1, "D:/BP_Layers/M_9S/landcover/tree_mask_90m.tif", datatype = "INT1U", overwrite = T) # write this mask as it will be used moving forward!



no.lim <- rast("D:/3PG_Cplusplus/_delete/new/biom_2100_currentT_unmasked.tif")

no.lim.shrub <- mask(no.lim, mask_crop)

plot(no.lim.shrub)

#writeRaster(no.lim.shrub, "D:/3PG_Cplusplus/_delete/new/biom_2100_shrub.tif")

tot_mask <- rast('D:/3PG_Cplusplus/_delete/new/biom_2020.tif')


# Load your raster data (replace with actual file paths)
raster_base <- rast("D:/3PG_Cplusplus/data_100/hillshade.flt")

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(tot_mask, add = TRUE, alpha = 0.9, col = viridis(100), title = "Treed Areas Only")

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(no.lim.shrub, add = TRUE, alpha = 0.9, col = viridis(100), title = "Treed and Shrubland Areas")

# Add a title to the plot
title("Treed, Shrubland, Bryoid, and Herb Areas")

# Create a legend
#legend("topright", legend = "tDM ha-1", bty = "n")

# Extract all pixel values as a vector
values <- as.data.frame(tot_mask)
column_sums <- colSums(values, na.rm = TRUE)


values2 <- as.data.frame(no.lim.shrub)
column_sums2 <- colSums(values2, na.rm = TRUE)

#########################




#######################

# Initialize vectors to store the results
co2_values <- seq(350, 700, by = 5)
fCalpha_values <- numeric(length(co2_values))
fCg_values <- numeric(length(co2_values))

fcalpha700 <- 1.4
fcg700 <- 0.7

for (i in 1:length(co2_values)) {
    co2 <- co2_values[i]

    fcalphax <- (fcalpha700 / (2 - fcalpha700))
    fcg0 <- (fcg700 / (2 * (fcg700 - 1)))

    fCalpha <- fcalphax * co2 / (350 * (fcalphax - 1) + co2)
    fCg <- fcg0 / (1 + (fcg0 - 1) * co2 / 350)

    # Store the results in vectors
    fCalpha_values[i] <- fCalpha
    fCg_values[i] <- fCg
}

##################################################################################

alphaCx = 0.047 #canopy quantum efficiency (PARAMETER)
fNutr = 0.88
fT = 0.980882468
fFrost = 0.966666666666667
#fCalpha = fCalpha_values[1]
PhysMod = 0.741921238213433


# alphaC = alphaCx * fNutr * fT * fFrost * fCalpha * PhysMod
# alphaC goes directly into NPP calculation

# Initialize a vector to store alphaC values
alphaC_values <- numeric(length(co2_values))

# Calculate alphaC for each fCalpha value
for (i in 1:length(co2_values)) {
    alphaC_values[i] <- alphaCx * fNutr * fT * fFrost * fCalpha_values[i] * PhysMod
}


# Create separate plots for fCalpha and fCg
par(mfrow = c(3, 1))  # Set the layout to have 2 rows and 1 column of plots

# Plot for fCalpha
plot(co2_values, fCalpha_values, type = "l", col = "blue", xlab = "CO2", ylab = "fCalpha")
title("fCalpha vs. CO2")

# Plot for fCg
plot(co2_values, fCg_values, type = "l", col = "red", xlab = "CO2", ylab = "fCg")
title("fCg vs. CO2")

# Create a plot
plot(co2_values, alphaC_values, type = "l", col = "forestgreen", xlab = "CO2", ylab = "alphaC")
title("alphaC vs. CO2 for Varying fCalpha")

par(mfrow = c(1, 1))  # Reset the layout to the default

####

fCalpha <- 1  # Set fCalpha to a constant value

# Create a dataframe to store the results
result_df <- data.frame(CO2 = co2_values, Required_alphaCx = numeric(length(co2_values)))

# Calculate required alphaCx values
for (i in 1:length(co2_values)) {
    #alphaC <- alphaCx * fNutr * fT * fFrost * fCalpha * PhysMod
    Required_alphaCx <- alphaC_values[i] / (fNutr * fT * fFrost * fCalpha * PhysMod)

    result_df$Required_alphaCx[i] <- Required_alphaCx
}

# Print the dataframe
print(result_df)

# Calculate alphaC_new using Required_alphaCx values
alphaC_new <- result_df$Required_alphaCx * fNutr * fT * fFrost * PhysMod

# Create a plot with alphaC and alphaC_new
plot(co2_values, alphaC_values, type = "n", xlab = "CO2", ylab = "alphaC")
lines(co2_values, alphaC_values, type = "l", col = "forestgreen")
lines(co2_values, alphaC_new, type = "l", col = "blue", lty = 4)

legend("topright", legend = c("alphaC", "alphaC_new"), col = c("forestgreen", "blue"), lty = c(1, 2))
title("alphaC and alphaC_new vs. CO2")


# Create a plot with alphaC and alphaC_new
plot(co2_values, result_df$Required_alphaCx, type = "n", xlab = "CO2", ylab = "Required alphaCx")
lines(co2_values, result_df$Required_alphaCx, type = "l", col = "forestgreen")
title("Required alphaCx to curve match vs. CO2")

#######################################################################################

# BUT - we know that PhysMod and AlphaC change over time...
# Trying to deal with that:
# Constants
co2_levels <- c(350, 500, 700)  # Include all relevant CO2 levels
times <- c(1, 2, 3)  # Example time points in years
fCalpha <- 1  # Constant for fCalpha
fNutr <- 0.88
fT <- 0.980882468
fFrost <- 0.966666667

# Observed data for alphaC and PhysMod over time and for different CO2 levels
# Define observed_alphaC and observed_PhysMod matrices or data frames
# The dimensions should match the number of CO2 levels and times.

# Example:
observed_alphaC <- matrix(c(
    0.029095917, 0.021712018, 0.02343216, # CO2 = 350, time = 1, 2, 3
    0.035115762, 0.034594885, 0.028280193, # CO2 = 500, time = 1, 2, 3
    0.040734284, 0.040130067, 0.032805024 # CO2 = 700, time = 1, 2, 3
), nrow = length(co2_levels), byrow = TRUE)

observed_PhysMod <- matrix(c(
    0.741921238, 0.553638062, 0.597500232,  # CO2 = 350, time = 1, 2, 3
    0.741921238, 0.730916229, 0.597500232,  # CO2 = 500, time = 1, 2, ...
    0.741921238, 0.730916229, 0.597500232   # CO2 = 700, time = 1, 2, ...
), nrow = length(co2_levels), byrow = TRUE)

# Function to calculate Required_alphaCx
calculate_required_alphaCx <- function(alphaC, PhysMod) {
    Required_alphaCx <- alphaC / (fNutr * fT * fFrost * fCalpha * PhysMod)
    return(Required_alphaCx)
}

# Calculate Required_alphaCx for different CO2 levels and times
result_df2 <- data.frame(CO2 = numeric(0), Time = numeric(0), Required_alphaCx = numeric(0))

for (i in 1:length(co2_levels)) {
    co2 <- co2_levels[i]
    for (j in 1:length(times)) {
        time <- times[j]
        alphaC <- observed_alphaC[i, j]
        PhysMod <- observed_PhysMod[i, j]

        # Calculate Required_alphaCx using observed values
        Required_alphaCx <- calculate_required_alphaCx(alphaC, PhysMod)

        # Store the results in a dataframe
        result_df2 <- rbind(result_df2, data.frame(CO2 = co2, Time = time, Required_alphaCx = Required_alphaCx))
    }
}

# Print the dataframe
print(result_df2)

# Does this show it doesn't matter whether it changes with time?
