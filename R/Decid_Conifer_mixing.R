# Load the required packages
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

#writeRaster(con1_mask, "D:/BP_Layers/U_13N/analysis/paper_2/con1.tif")
#writeRaster(dec1_mask, "D:/BP_Layers/U_13N/analysis/paper_2/dec1.tif")

# Load the raster data
raster1 <- con1_mask  # Biomass values for SPRUCE
raster2 <- dec1_mask #rast("path/to/raster2.tif")  # Biomass values for DECID

raster3 <- rast("D:/BP_Layers/U_13N/3PG_flt/5_90m_inputs_all/scaled_TWI_noNA.tif")  # Wetness index

raster3.mask <- mask(raster3, Sh_mask)

# Scale the wetness index raster to 0-1 range
raster3.sc <- (raster3.mask - minmax(raster3.mask)[1]) / (minmax(raster3.mask)[2] - minmax(raster3.mask)[1])

# Create a new raster to store the combined values
new_raster <- raster1

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
test$scen3 <- ifelse(test$wetness > 0.67, test$decid,
                          ifelse(test$wetness < 0.33, test$conif,
                                 test$random))
# Create the new columns
test$scen2 <- ifelse(test$wetness > 0.67, test$decid, test$conif)
#test$scenario3 <- ifelse(test$wetness > 0.5, test$decid, test$conif)

head(test)

raster.scen1 <- raster1
raster.scen1[!is.na(raster.scen1)] <- test$scen3

#writeRaster(raster.scen1, "D:/BP_Layers/U_13N/analysis/paper_2/scen3.tif")

global(raster.scen1, fun="mean",na.rm=TRUE)
mean(test$scenario1)

biomass.df <- test %>% dplyr::select(!c(wetness, random))
biomass.df$clim_scen <- 'S2'
biomass.df$study_area <- 'U_13N'

#############################
library(reshape2)


# Reshape data (explicitly specify scenario columns)
biomass.molten <- melt(biomass.df, id.vars = c("clim_scen", "study_area"),
                       measure.vars = c("conif", "decid", "scen3", "scen2"))

# Group by variable (actual column name after melt)
biomass.summary <- biomass.molten %>%
    group_by(variable) %>%  # Group by "variable" which holds scenario information
    summarize(mean_biomass = mean(value))


##################
# Define labels for the columns
col_labels <- c("coniferous", "deciduous", "scenario 3", "scenario 2")

# Select the first 4 columns for plotting
biomass_df_subset <- biomass.df[, 1:4]  # Select columns 1 to 4 (inclusive)

# Melt the dataframe to long format for easier plotting
biomass_df_melted <- reshape2::melt(biomass_df_subset)

# Plot violin plots for each variable
ggplot(biomass_df_melted, aes(x = variable, y = value)) +
    geom_violin(fill = "skyblue", color = "black") +
    #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) + # Add boxplots for visualizing quartiles and outliers
    theme_minimal() +
    labs(title = "Distribution of Biomass Variables",
         x = "Variable",
         y = "Biomass Value")

## or

# Reshape the dataframe using gather()
biomass_df_long <- biomass_df_subset %>%
    gather(key = "variable", value = "value")

# Create the plot
ggplot(biomass_df_long, aes(x = variable, y = value)) +
    geom_violin() +
    geom_boxplot(width = 0.1, fill = "white") +
    labs(x = "Variable", y = "Value") +
    theme_minimal()

###################



##########

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

# Create a grouped bar plot
ggplot(pairwise_results, aes(x = Comparison, y = Z, fill = P.adj < 0.05)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = ifelse(P.adj < 0.05, "Significant", "")),
              vjust = -0.5, position = position_dodge(width = 0.9)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Pairwise Dunn Test Comparisons",
         x = "Comparison",
         y = "Z Value") +
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey"),
                      guide = FALSE)








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
dunn_test <- dunnTest(biomass.molten$value, biomass.molten$variable, method = "bonferroni")


# Create a data frame for plotting
plot_data <- data.frame(
    comparison = dunn_test$res[1],
    z = dunn_test$res[2],
    p.value = dunn_test$res[4]
)

# Create the plot
ggplot(plot_data, aes(x = Comparison, y = Z)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = sprintf("p = %.3f", P.adj)), vjust = -0.5, size = 3) +
    coord_flip() +
    labs(
        title = "Dunn's Test for Pairwise Comparisons",
        x = "Comparison",
        y = "Z-score"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))

######


dunn_pvals <- dunn_test$res[, "P.adj"]

assign_letter <- function(pval.matrix) {
    # Set a significance threshold (e.g., 0.05)
    alpha <- 0.05

    # Initialize an empty vector for letters
    letters <- rep(NA, nrow(pval.matrix))

    # Loop through rows (each comparison)
    for (i in 1:nrow(pval.matrix)) {
        # Find non-significant comparisons (p-value > alpha)
        if (all(pval.matrix[i, ] > alpha)) {
            # Assign same letter for non-significant comparisons
            letters[i] <- letters[which.max(letters)]
        } else {
            # Assign a new letter for significant comparisons
            letters[i] <- paste0("A", length(unique(letters)) + 1)
        }
    }
    return(letters)
}


# Assuming 'variable' is the column for scenario names
biomass.molten.2 <- biomass.molten %>%
    left_join(data.frame(variable = unique(biomass.molten$variable), pval = dunn_pvals), by = "variable") %>%
    mutate(letter_group = assign_letter(pval))



######

biomass.test <- kruskal.test(value ~ variable, data = biomass.molten)


#####

# Calculate means for each level of the variable factor
mean_values <- biomass.molten %>%
    group_by(variable) %>%
    summarise(mean_value = mean(value))

# Perform pairwise comparisons using the Wilcoxon rank sum test
pairwise_results <- pairwise.wilcox.test(biomass.molten$value, biomass.molten$variable, p.adjust.method = "bonferroni")

# Extract adjusted p-values from pairwise comparison results
p_adj <- pairwise_results$P.adj

significance <- ifelse(p_adj < 0.05, "*", "")

# Create a data frame for plotting
plot_data <- data.frame(variable = unique(biomass.molten$variable),
                        mean_value = mean_values$mean_value,
                        significance = significance)

# Plot means with error bars and significance indicators
ggplot(plot_data, aes(x = variable, y = mean_value)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_errorbar(stat = "identity", aes(ymin = mean_value - sd(value), ymax = mean_value + sd(value)), width = 0.2) +
    geom_text(aes(label = significance), vjust = -0.5, size = 6) +
    labs(title = "Mean Biomass by Planting Scenario",
         x = "Planting Scenario",
         y = "Mean Biomass") +
    theme_minimal()







# Combine mean values with adjusted p-values
pairwise_comparison_data <- cbind(mean_values, p_adj)

# Plot means with error bars and significance letters
ggplot(pairwise_comparison_data, aes(x = variable, y = mean_value)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_errorbar(stat = "identity", aes(ymin = mean_value - sd(value), ymax = mean_value + sd(value)), width = 0.2) +
    geom_text(aes(label = ifelse(p_adj < 0.05, "*", "")), vjust = -0.5, size = 6) +
    labs(title = "Mean Biomass by Planting Scenario",
         x = "Planting Scenario",
         y = "Mean Biomass") +
    theme_minimal()





