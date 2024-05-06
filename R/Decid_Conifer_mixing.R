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


names(test) <- c("conif", "decid", "wetness", "random")
head(test)

# Create the new column
test$scenario1 <- ifelse(test$wetness > 0.7, test$decid,
                          ifelse(test$wetness < 0.3, test$conif,
                                 test$random))
# Create the new columns
test$scenario2 <- ifelse(test$wetness > 0.8, test$decid, test$conif)
test$scenario3 <- ifelse(test$wetness > 0.5, test$decid, test$conif)

head(test)

raster.scen1 <- raster1
raster.scen1[!is.na(raster.scen1)] <- test$scenario1

global(raster.scen1, fun="mean",na.rm=TRUE)
mean(test$scenario1)

biomass.df <- test %>% select(!c(wetness, random))
biomass.df$clim_scen <- 'S2'
biomass.df$study_area <- 'U_13N'

#############################
library(reshape2)

library(reshape2)

# Reshape data (explicitly specify scenario columns)
biomass.molten <- melt(biomass.df, id.vars = c("clim_scen", "study_area"),
                       measure.vars = c("conif", "decid", "scenario1", "scenario2", "scenario3"))

# Group by variable (actual column name after melt)
biomass.summary <- biomass.molten %>%
    group_by(variable) %>%  # Group by "variable" which holds scenario information
    summarize(mean_biomass = mean(value))


library(stats)
library(FSA)

# Kruskal-Wallis test
kruskal_results <- kruskal.test(value ~ variable, data = biomass.molten)

# Dunn's test with Bonferroni correction for pairwise comparisons
dunn_results <- dunnTest(value ~ variable, data = biomass.molten, method = "bonferroni")




# Print Kruskal-Wallis test results
print(kruskal_results)

# Print Dunn's test results
print(dunn_results)

# Extract pairwise comparison results from Dunn's test
pairwise_results <- dunn_results$res

# Plot pairwise comparisons
ggplot(pairwise_results, aes(x = Comparison, y = P.adj)) +
    geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
    coord_flip() +
    labs(title = "Pairwise Comparisons",
         x = "Comparison",
         y = "Adjusted p-value") +
    theme_minimal()



library(ggplot2)
library(rcompanion)

# Perform Dunn's test
dunn_test <- dunnTest(biomass.molten$value, biomass.molten$variable, p.adjust.method = "bonferroni")

# Create a data frame for plotting
plot_data <- data.frame(
    comparison = dunn_test$comparison,
    z = dunn_test$Z,
    p.value = dunn_test$P
)

# Create the plot
ggplot(plot_data, aes(x = comparison, y = z)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = sprintf("p = %.3f", p.value)), vjust = -0.5, size = 3) +
    coord_flip() +
    labs(
        title = "Dunn's Test for Pairwise Comparisons",
        x = "Comparison",
        y = "Z-score"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))

#####
library(emmeans)

# Calculate estimated marginal means
em_means <- emmeans(biomass.molten, ~ variable)

# Extract pairwise comparisons
pairwise_comparisons <- pairs(em_means)

# Plot means with error bars and significance letters
ggplot(biomass.molten, aes(x = variable, y = value)) +
    geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
    geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.2) +
    geom_text(data = pairwise_comparisons, aes(x = x, y = y, label = as.character(p.adj)), vjust = -1, size = 4) +
    labs(title = "Mean Biomass by Planting Scenario",
         x = "Planting Scenario",
         y = "Mean Biomass") +
    theme_minimal()
