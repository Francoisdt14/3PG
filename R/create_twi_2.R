library(terra)

# Load TWI from all 6 study areas at 30m resolution:

M_9S <- rast("D:/BP_Layers/M_9S/3PG_flt/1_other_inputs/TWI_M_9S.tif")
M_11S <- rast("D:/BP_Layers/M_11S/large_rasters/TWI_M_11s.tif")
M_18S <- rast("D:/BP_Layers/M_18S/large_rasters/TWI_M_18s.tif")
U_13N <- rast("D:/BP_Layers/U_13N/large_rasters/TWI_U_13N.tif")
U_18S <- rast("D:/BP_Layers/U_18S/large_rasters/TWI_U_18S.tif")
U_15S <- rast("D:/BP_Layers/U_15S/large_rasters/TWI_U_15S.tif")


par(mfrow=c(3,2))
hist(M_9S)
hist(M_11S)
hist(M_18S)
hist(U_13N)
hist(U_18S)
hist(U_15S)


#################################################################
# List of raster objects
raster_files <- list(M_9S, M_18S, M_11S, U_13N, U_18S, U_15S)

# Function to extract 10% of values randomly from a raster
extract_random_values <- function(raster) {
  # Extract 10% of values randomly, excluding NA values
  sampled_values <- sample(values(raster), size = round(0.1 * ncell(raster)))
  
  # Create a data frame with the sampled values
  df <- data.frame(RasterName = rep(raster@file@name, length(sampled_values)),
                   SampledValue = sampled_values)
  
  return(df)
}

# Apply the function to each raster
result_df_list <- lapply(raster_files, function(file) {
  raster <- rast(file)
  extract_random_values(raster)
})

# Combine the results into a single data frame
result_df <- do.call(rbind, result_df_list)

# View the resulting data frame
print(result_df)

#################################################################
# List of raster objects
raster_files <- list(M_9S, M_18S, M_11S, U_13N, U_18S, U_15S)

# Function to extract 10% of values randomly from a raster
extract_random_values <- function(raster) {
  # Extract values excluding NAs
  raster_values <- values(raster)
  non_na_values <- na.omit(raster_values)
  
  # Sample 10% of non-NA values randomly
  sampled_values <- sample(non_na_values, size = round(0.1 * length(non_na_values)))
  
  # Create a data frame with the sampled values
  df <- data.frame(RasterName = rep(names(raster), length(sampled_values)),
                   SampledValue = sampled_values)
  
  return(df)
}

# Apply the function to each raster
result_df_list <- lapply(raster_files, function(raster) {
  extract_random_values(raster)
})

# Combine the results into a single data frame
result_df <- do.call(rbind, result_df_list)

any(is.na(result_df))

####
#TWI.df <- as.data.frame(TWI)
TWI.df <- result_df


#######
# Can do this multiple ways:
# Calculate the 5th and 95th percentiles of the original data
percentile_5 <- quantile(TWI.df$SampledValue, probs = 0.05)
percentile_95 <- quantile(TWI.df$SampledValue, probs = 0.95)

# Define the original and scaled percentile values
original_percentiles <- c(percentile_5, percentile_95)
scaled_percentiles <- c(100, 300)

# Calculate the slope and intercept of the linear equation
slope <- (scaled_percentiles[2] - scaled_percentiles[1]) / (original_percentiles[2] - original_percentiles[1])
intercept <- scaled_percentiles[1] - slope * original_percentiles[1]
####
# OR:
# Create a linear regression model
#model <- lm(scaled_values ~ original_values)
#summary(model)
# formula to follow:
#scaled_values = intercept + slope * original_values


# Set the output folder
output_folder <- "D:/BP_Layers/TWI_Outputs/"

# Loop through each raster
for (i in seq_along(raster_files)) {
  # Get the current raster
  current_raster <- raster_files[[i]]
    # Apply the equation
  scaled_raster <- intercept + slope * current_raster
    # Clamp the data
  TWI.clamp <- clamp(scaled_raster, lower = 0, upper = 500)
    # Construct the output file name
  output_name <- paste0(output_folder, names(current_raster), ".tif")
    # Write the clamped raster
  writeRaster(TWI.clamp, filename = output_name, overwrite = TRUE)
    # Find the mean of the raster
  mean_TWI <- global(TWI.clamp, fun = mean, na.rm = TRUE)
    # Create a new raster
  TWI_clamp_noNA <- TWI.clamp
    # Fill the new raster with mean where values are NA
  TWI_clamp_noNA[is.na(TWI_clamp_noNA)] <- mean_TWI
    # Construct the output file name for the noNA raster
  output_name_noNA <- paste0(output_folder, names(current_raster), "_noNA.tif")
    # Save the noNA raster
  writeRaster(TWI_clamp_noNA, filename = output_name_noNA, overwrite = TRUE)
    # Aggregate the raster to 90 m from 30m
    TWI_clamp_noNA_90m <- terra::aggregate(TWI_clamp_noNA, 100/res(TWI.clamp.noNA)[1], cores = 12)
    # construct the output file name 
  output_name_noNA_90m <- paste0(output_folder, names(current_raster), "_noNA_90m.tif")
    # Write the raster 
  writeRaster(TWI_clamp_noNA_90m, filename = output_name_noNA_90m, overwrite = TRUE)
}


test <- rast("D:/BP_Layers/TWI_Outputs/TWI_M_11S.tif")

#test2 <- rast("D:/BP_Layers/M_11S/3PG_flt/6_90m_flt/NFFD01.flt")

compareGeom(test,test2)









