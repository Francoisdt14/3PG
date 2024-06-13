# Load the required packages

library(tidyverse)
library(terra)
library(arrow)
library(FSA)
library(tictoc)
library(viridis)
library(ggridges)
library(dplyr)
library(tidyr)
library(gridExtra)
library(FSA)

####
#Custom ggplot theme
library(extrafont)
font_import()
loadfonts(quiet = T)
theme_cwm <- function(){
    theme_bw() %+replace%    #replace elements we want to change
        theme(

            #grid elements
            #panel.grid = element_blank(),    #strip major gridlines
            axis.ticks = element_line(color = "black"),          #strip axis ticks

            #text elements
            text = element_text(family = "Open Sans", color = "black"),
            axis.text = element_text(family = "Open Sans", color = "black"),

            # other
            legend.position = "bottom",
            strip.background = element_blank()

        )
}

####



# First, some TESTING

#######################################################################################
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

test$mix <- ifelse(test$wetness > 0.67, test$conif,
                    ifelse(test$wetness < 0.33, test$decid,
                           test$random))

############################################################################

# DO IT IN A LOOP
# Define the study areas and scenarios
#study_areas <- c("U_13N", "M_9S", "M_18S", "U_18S")
# U_13N = different!
study_areas <- c("M_9S", "M_18S", "U_18S", "M_11S", "U_15S")
study_areas <- c("U_13N")
scenarios <- c("S1", "S2", "S3")

# Loop through each study area and scenario
for (study_area in study_areas) {
    for (scenario in scenarios) {

        output_file <- paste0("D:/BP_Layers/analysis/paper_2/", study_area, "_", scenario, ".csv")
        if (!file.exists(output_file)) {
        # Load the masks
        Sh_mask <- rast(paste0("D:/BP_Layers/", study_area, "/landcover/Sh_90m.tif"))

        # Load in the stem and foliage values at 2080 for each scenario
        # dec.ws <- rast(paste0("I:/data_2024_05_02_deciduous/", study_area, "/", scenario, "_dec/Y4_output/ws.flt"))
        # dec.wf <- rast(paste0("I:/data_2024_05_02_deciduous/", study_area, "/", scenario, "_dec/Y4_output/wf.flt"))

        dec.ws <- rast(paste0("I:/data_2024_05_02_deciduous/", study_area, "/", scenario, "_dec/Y4_output/ws208007.flt")) # U_13N
        dec.wf <- rast(paste0("I:/data_2024_05_02_deciduous/", study_area, "/", scenario, "_dec/Y4_output/wf208007.flt")) # U_13N

        # Calculate agb for each scenario in 2080
        dec1 <- dec.ws + dec.wf
        crs(dec1) <- crs(Sh_mask)
        dec1_mask <- terra::mask(dec1, Sh_mask)

        con.ws <- rast(paste0("D:/BP_Layers/", study_area, "/biomass_3PG/", scenario, "/Y4_output/ws208007.flt")) # U_13N
        con.wf <- rast(paste0("D:/BP_Layers/", study_area, "/biomass_3PG/", scenario, "/Y4_output/wf208007.flt")) # U_13N

        #con.ws <- rast(paste0("D:/BP_Layers/", study_area, "/biomass_3PG/", scenario, "/Y4_output/ws.flt"))
        #con.wf <- rast(paste0("D:/BP_Layers/", study_area, "/biomass_3PG/", scenario, "/Y4_output/wf.flt"))

        con1 <- con.ws + con.wf
        crs(con1) <- crs(Sh_mask)
        con1_mask <- terra::mask(con1, Sh_mask)

        # Load the raster data
        raster1 <- con1_mask # Biomass values for SPRUCE
        raster2 <- dec1_mask # Biomass values for DECID
        raster3 <- rast(paste0("D:/BP_Layers/", study_area, "/3PG_flt/5_90m_inputs_all/scaled_TWI_noNA.tif")) # Wetness index
        raster3.mask <- mask(raster3, Sh_mask)

        # Scale the wetness index raster to 0-1 range
        raster3.sc <- (raster3.mask - minmax(raster3.mask)[1]) / (minmax(raster3.mask)[2] - minmax(raster3.mask)[1])

        # Create a new raster to store the combined values
        new_raster <- raster1

        # Combine the rasters into a single stack
        r1 <- raster1
        r2 <- raster2
        r3 <- raster3.sc
        r_stack <- c(r1, r2, r3)

        # Extract the values from the stack, skipping NAs
        r_values <- values(r_stack, na.rm = TRUE)
        test <- r_values %>% as.data.frame()

        # Add random column and assign column names
        test$random <- apply(test[, 1:2], MARGIN = 1, FUN = function(rw) {sample(rw, 1)})
        names(test) <- c("conif", "decid", "wetness", "random")

        # Create the new column
        test$mix <- ifelse(test$wetness > 0.67, test$conif,
                             ifelse(test$wetness < 0.33, test$decid,
                                    test$random))
        # Create the new columns
        #test$scen2 <- ifelse(test$wetness > 0.67, test$decid, test$conif)
        #test$scenario3 <- ifelse(test$wetness > 0.5, test$decid, test$conif)

        # Add scenario and study area columns
        test$clim_scen <- scenario
        test$study_area <- study_area


        # Save the table as a CSV file
        output_file <- paste0("D:/BP_Layers/analysis/paper_2/", study_area, "_", scenario, ".csv")
        write.csv(test, output_file, row.names = FALSE)

        # Print a message to notify when the CSV file has been written
        print(paste0("CSV file written: ", output_file))
        } else {
            # Optional: Print a message if the file already exists
            print(paste0("Skipping existing file: ", output_file))
        }
    }
}


##################################
# This is just an example of what things might look like if we want to plot
u18S.s3 <- read.csv("D:/BP_Layers/analysis/paper_2/U_18S_S3.csv")

rast.conif <- rast("D:/BP_Layers/U_18S/landcover/Sh_90m.tif")
rast.decid <- rast("D:/BP_Layers/U_18S/landcover/Sh_90m.tif")
rast.mix <- rast("D:/BP_Layers/U_18S/landcover/Sh_90m.tif")
# rast.s1 <- mask

# values(rast.s1)[!is.na(values(rast.s1))] <- update.value[!is.na(values(rast.s1))]
rast.conif[!is.na(rast.conif)] <- u18S.s3$conif
rast.decid[!is.na(rast.decid)] <- u18S.s3$decid
rast.mix[!is.na(rast.mix)] <- u18S.s3$mix

names(rast.conif) <- "conif"
names(rast.decid) <- "decid"
names(rast.mix) <- "mix"

# Set the consistent scale limits
min_val <- 150
max_val <- 500

par(mfrow=c(1,3))
plot(rast.conif, main = names(rast.conif), range=c(min_val, max_val))
plot(rast.mix,main = names(rast.mix),range=c(min_val, max_val))
plot(rast.decid, main = names(rast.decid), range=c(min_val, max_val))

par(mfrow=c(1,1))

writeRaster(rast.conif,"D:/BP_Layers/U_18S/analysis/paper_2/conif_s3.tif")
writeRaster(rast.decid,"D:/BP_Layers/U_18S/analysis/paper_2/decid_s3.tif")
writeRaster(rast.mix,"D:/BP_Layers/U_18S/analysis/paper_2/mix_s3.tif")

##################################

scenarios <- c("S1", "S2", "S3")

for (scenario in scenarios) {
    file_paths <- list.files("D:/BP_Layers/analysis/paper_2/", pattern = paste0("_", scenario, ".csv"), full.names = TRUE)
    if (length(file_paths) > 0) {
        # Combine data frames using lapply and rbind
        combined_df <- do.call(rbind, lapply(file_paths, read.csv))
                # Define output Parquet file path
        output_file <- paste0("D:/BP_Layers/analysis/paper_2/combined_", scenario, ".parquet")
                # Write the combined data frame as Parquet file
        write_parquet(combined_df, output_file)
        print(paste0("Combined dataframe written to: ", output_file))
    } else {
        print(paste0("No files found for scenario: ", scenario))
    }
}

##############

## read in combined dataframes
df.s1 <- read_parquet("D:/BP_Layers/analysis/paper_2/combined_S1.parquet")
df.s2 <- read_parquet("D:/BP_Layers/analysis/paper_2/combined_S2.parquet")
df.s3 <- read_parquet("D:/BP_Layers/analysis/paper_2/combined_S3.parquet")

df.all <- rbind(df.s1, df.s2)
df.all <- rbind(df.all, df.s3)

# quick box plot to look at coniferous
ggplot(df.all, aes(x = clim_scen, y = conif)) +
    geom_boxplot() +
    labs(title = "Comparison of conif Biomass Across Climate Scenarios",
         x = "Climate Scenario",
         y = "conif Biomass")



ggplot(df.all, aes(x = conif, color = clim_scen, fill = clim_scen, y = clim_scen)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Distribution of Conif Biomass Across Climate Scenarios",
         x = "Conif Biomass", y = "Density") +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw()




# Filter the data for climate scenario S1
df_s1 <- df.all %>% filter(clim_scen == "S1")

# Reshape the data from wide to long format
df_s1 <- df_s1 %>%
    pivot_longer(cols = c(conif, decid, scen2), names_to = "biomass_type", values_to = "biomass_value")


ggplot(df_s1, aes(x = biomass_value, y = biomass_type, fill = biomass_type)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Distribution of Biomass Values for Climate Scenario S1",
         x = "Biomass Value", y = NULL) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_bw()


####################

# Create a new data frame with only the necessary columns
df_biomass <- df.all %>%
    select(conif, decid, mix, clim_scen)

# Pivot the data to a long format
df_long <- df_biomass %>%
    pivot_longer(cols = c(conif, decid, mix), names_to = "biomass_type", values_to = "biomass_value")


# Plots with ridgelines for climate scenarios, separated by biomass type
p1 <- ggplot(df_biomass, aes(x = conif, y = clim_scen, fill = clim_scen)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Distribution of Conif Biomass Across Climate Scenarios",
         x = "Conif Biomass", y = "Density") +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw()

p2 <- ggplot(df_biomass, aes(x = decid, y = clim_scen, fill = clim_scen)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Distribution of Decid Biomass Across Climate Scenarios",
         x = "Decid Biomass", y = "Density") +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw()

p3 <- ggplot(df_biomass, aes(x = mix, y = clim_scen, fill = clim_scen)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Distribution of Scen2 Biomass Across Climate Scenarios",
         x = "Scen2 Biomass", y = "Density") +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw()


# Plots with ridgelines for biomass types, separated by climate scenario
p4 <- ggplot(df_long %>% filter(clim_scen == "S1"), aes(x = biomass_value, y = biomass_type, fill = biomass_type)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Distribution of Biomass Values for Climate Scenario S1",
         x = "Biomass Value", y = NULL) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_bw()

p5 <- ggplot(df_long %>% filter(clim_scen == "S2"), aes(x = biomass_value, y = biomass_type, fill = biomass_type)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Distribution of Biomass Values for Climate Scenario S2",
         x = "Biomass Value", y = NULL) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_bw()

p6 <- ggplot(df_long %>% filter(clim_scen == "S3"), aes(x = biomass_value, y = biomass_type, fill = biomass_type)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Distribution of Biomass Values for Climate Scenario S3",
         x = "Biomass Value", y = NULL) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_bw()

# Print the plots vertically
print(p4)
print(p5)
print(p6)

# Arrange the plots into a single plot with 2 columns and 3 rows
combined_plot <- grid.arrange(p1, p4, p2, p5, p3, p6, ncol = 2)

# Print the combined plot
print(combined_plot)

################################



# Plots with separate panels for each biomass type, with fixed x-axis
faceted_plot_1 <- ggplot(df_long, aes(x = biomass_value, y = clim_scen, fill = clim_scen)) +
    geom_density_ridges(alpha = 0.5) +
    facet_wrap(~biomass_type, scales = "free_y", ncol = 1) +
    labs(title = "Distribution of Biomass Values Across Climate Scenarios",
         x = "Biomass Value", y = "Density") +
    scale_fill_viridis(discrete = TRUE) +
    theme_cwm() #theme_bw()

# Plots with separate panels for each climate scenario, with fixed x-axis
faceted_plot_2 <- ggplot(df_long, aes(x = biomass_value, y = biomass_type, fill = biomass_type)) +
    geom_density_ridges(alpha = 0.5) +
    facet_wrap(~clim_scen, scales = "free_y", ncol = 1) +
    labs(title = "Distribution of Biomass Values by Climate Scenario",
         x = "Biomass Value", y = "Density") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_cwm() #theme_bw()


# Arrange the faceted plots side by side
combined_plot <- grid.arrange(faceted_plot_1, faceted_plot_2, ncol = 2)
# Print the combined plot
print(combined_plot)

###
# problem with theme_cwm

# Export the combined plot to a PDF (optional)
ggsave("G:/Sync/PostDoc/Figures/Paper_2/ridge_plot_test2_new.pdf", combined_plot, width = 8.5, height = 11, units = "in", device = cairo_pdf)

# Summarize statistics by clim_scen
summary_df <- df_biomass %>%
    group_by(clim_scen) %>%
    summarise(
        conif_mean = mean(conif, na.rm = TRUE),
        conif_median = median(conif, na.rm = TRUE),
        conif_min = min(conif, na.rm = TRUE),
        conif_max = max(conif, na.rm = TRUE),
        conif_sd = sd(conif, na.rm = TRUE),
        decid_mean = mean(decid, na.rm = TRUE),
        decid_median = median(decid, na.rm = TRUE),
        decid_min = min(decid, na.rm = TRUE),
        decid_max = max(decid, na.rm = TRUE),
        decid_sd = sd(decid, na.rm = TRUE),
        mix_mean = mean(mix, na.rm = TRUE),
        mix_median = median(mix, na.rm = TRUE),
        mix_min = min(mix, na.rm = TRUE),
        mix_max = max(mix, na.rm = TRUE),
        mix_sd = sd(mix, na.rm = TRUE)
    )

# View the summary data frame
print(summary_df)

summary_df_transposed <- t(summary_df)




write.csv(summary_df, "D:/BP_Layers/analysis/paper_2/summary_df.csv")

########################################################################


df_biomass$clim_scen <- as.factor(df_biomass$clim_scen)

kruskal_conif <- kruskal.test(conif ~ clim_scen, data = df_biomass)
kruskal_decid <- kruskal.test(decid ~ clim_scen, data = df_biomass)
kruskal_mix <- kruskal.test(mix ~ clim_scen, data = df_biomass)


# Subset the data
subset_data <- df_biomass[sample(nrow(df_biomass), 1000000), ]


tic();dunn_conif <- dunnTest(conif ~ clim_scen, data = subset_data, method = "bonferroni");toc()
tic();dunn_decid <- dunnTest(decid ~ clim_scen, data = subset_data, method = "bonferroni");toc()
tic();dunn_mix <- dunnTest(mix ~ clim_scen, data = subset_data, method = "bonferroni");toc()

subset_data2 <- df_long[sample(nrow(df_long), 1000000), ]

kruskal_S1 <- kruskal.test(biomass_value ~ biomass_type, data = subset(subset_data2, clim_scen == "S1"))
dunn_S1 <- dunnTest(biomass_value ~ biomass_type, data = subset(subset_data2, clim_scen == "S1"), method = "bonferroni")

kruskal_S2 <- kruskal.test(biomass_value ~ biomass_type, data = subset(subset_data2, clim_scen == "S2"))
dunn_S2 <- dunnTest(biomass_value ~ biomass_type, data = subset(subset_data2, clim_scen == "S2"), method = "bonferroni")

kruskal_S3 <- kruskal.test(biomass_value ~ biomass_type, data = subset(subset_data2, clim_scen == "S3"))
dunn_S3 <- dunnTest(biomass_value ~ biomass_type, data = subset(subset_data2, clim_scen == "S3"), method = "bonferroni")


###################################
# Looking at NORTH vs SOUTH

df.loc <- df.all
df.loc$clim_scen <- as.factor(df.loc$clim_scen)
df.loc$study_area <- as.factor(df.loc$study_area)
# Extract the first letter from the study_area column
df.loc$managed <- substr(df.loc$study_area, 1, 1)
df.loc$managed <- as.factor(df.loc$managed)




kruskal.test(conif ~ managed, data = subset(df.loc, clim_scen == "S1"))


df_long.loc <- df.loc %>%
    pivot_longer(cols = c(conif, decid, mix),
                 names_to = "biomass_type",
                 values_to = "biomass_value")

####


#############

# Filter the data for 'conif' only
df_conif <- df.loc %>%
    select(conif, clim_scen, managed)
# Convert to long format
df_conif_long <- df_conif %>%
    pivot_longer(cols = conif, names_to = "biomass_type", values_to = "biomass_value")


# Filter the data for 'decid' only
df_decid <- df.loc %>%
    select(decid, clim_scen, managed)
# Convert to long format
df_decid_long <- df_decid %>%
    pivot_longer(cols = decid, names_to = "biomass_type", values_to = "biomass_value")

# Filter the data for 'mix' only
df_mix <- df.loc %>%
    select(mix, clim_scen, managed)
# Convert to long format
df_mix_long <- df_mix %>%
    pivot_longer(cols = mix, names_to = "biomass_type", values_to = "biomass_value")



management_plot <- ggplot(df_conif_long, aes(x = biomass_value, y = managed, fill = managed)) +
    geom_density_ridges(alpha = 0.5) +
    facet_wrap(~clim_scen, scales = "free_y", ncol = 1) +
    labs(title = "Distribution of Biomass Values by Climate Scenario",
         x = "Biomass Value", y = "Density") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_cwm() + #theme_bw()
    xlim(50, 600)  # Set x-axis limits from 50 to 600

#ggsave("G:/Sync/PostDoc/Figures/Paper_2/management_decid_2.pdf", management_plot, width = 8.5, height = 11, units = "in", device = cairo_pdf)


# What if we look at it by study area???
# Filter the data for 'decid' only
df_decid_study <- df.loc %>%
    select(decid, clim_scen, study_area)
# Convert to long format
df_decid__study_long <- df_decid_study %>%
    pivot_longer(cols = decid, names_to = "biomass_type", values_to = "biomass_value")


# Create the ridgeplot

ggplot(df_decid__study_long, aes(x = biomass_value, y = study_area, fill = study_area)) +
    geom_density_ridges(alpha = 0.5) +
    facet_wrap(~clim_scen, scales = "free_y", ncol = 1) +
    labs(title = "Distribution of Biomass Values by Climate Scenario",
         x = "Biomass Value", y = "Density") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_cwm() #theme_bw()


##################
# I want to compare the biomass values for 1 climate scenario, facet wrap the study areas?

#df_long.loc_S2 <- df_long.loc %>% subset(clim_scen == 'S2')

df_long.loc_S2 <- df_long.loc %>%
    filter(clim_scen == 'S2' & biomass_type %in% c('conif', 'decid'))

clim2_plot <- ggplot(df_long.loc_S2, aes(x = biomass_value, y = biomass_type, fill = biomass_type)) +
    geom_density_ridges(alpha = 0.5) +
    facet_wrap(~study_area, scales = "free_y", ncol = 1) +
    labs(title = "Distribution of Biomass Values by Climate Scenario",
         x = "Biomass Value", y = "Density") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_cwm() #theme_bw()

#ggsave("G:/Sync/PostDoc/Figures/Paper_2/ridge_plot_S2.pdf", clim2_plot, width = 8.5, height = 11, units = "in", device = cairo_pdf)


#################

means_management <- df_long.loc %>%
    group_by(biomass_type, managed, clim_scen) %>%
    summarise(mean_value = mean(biomass_value, na.rm = TRUE))

print(means_management)
#########

means_study <- df_long.loc %>%
    group_by(biomass_type, study_area, clim_scen) %>%
    summarise(mean_value = mean(biomass_value, na.rm = TRUE))

print(means_study)



# Filter data for 'conif' biomass type
df_filtered <- df_long.loc %>%
    filter(biomass_type == "conif")  # Select rows where biomass_type is "conif"

#############################################


# Calculate mean biomass value for each level of study_area
mean_biomass <- df_decid__study_long %>%
    group_by(study_area) %>%
    summarize(mean_biomass = mean(biomass_value, na.rm = TRUE))

# Reorder study_area factor levels based on mean biomass value
df_decid__study_long$study_area <- factor(df_decid__study_long$study_area, levels = mean_biomass$study_area[order(mean_biomass$mean_biomass)])

# Create the ggplot with reordered study_area levels
decid_plot <- ggplot(df_decid__study_long, aes(x = biomass_value, y = study_area, fill = study_area)) +
    geom_density_ridges(alpha = 0.5) +
    facet_wrap(~clim_scen, scales = "free_y", ncol = 1) +
    labs(title = "Distribution of Biomass Values by Climate Scenario",
         x = "Biomass Value", y = "Density") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_cwm() + #theme_bw()
    xlim(50, 550)  # Set x-axis limits from 50 to 600

#ggsave("G:/Sync/PostDoc/Figures/Paper_2/ridge_plot_decid3.pdf", decid_plot, width = 7, height = 11, units = "in", device = cairo_pdf)

############################################################
# Filter the data for 'decid' only
df_conif_study <- df.loc %>%
    select(conif, clim_scen, study_area)
# Convert to long format
df_conif_study_long <- df_conif_study %>%
    pivot_longer(cols = conif, names_to = "biomass_type", values_to = "biomass_value")

# Calculate mean biomass value for each level of study_area
mean_biomass <- df_conif_study_long %>%
    group_by(study_area) %>%
    summarize(mean_biomass = mean(biomass_value, na.rm = TRUE))

# Reorder study_area factor levels based on mean biomass value
df_conif_study_long$study_area <- factor(df_conif_study_long$study_area, levels = mean_biomass$study_area[order(mean_biomass$mean_biomass)])

# Create the ggplot with reordered study_area levels
conif_plot <- ggplot(df_conif_study_long, aes(x = biomass_value, y = study_area, fill = study_area)) +
    geom_density_ridges(alpha = 0.5) +
    facet_wrap(~clim_scen, scales = "free_y", ncol = 1) +
    labs(title = "Distribution of Biomass Values by Climate Scenario",
         x = "Biomass Value", y = "Density") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_cwm()  + #theme_bw()
    xlim(50, 550)  # Set x-axis limits from 50 to 600

#ggsave("G:/Sync/PostDoc/Figures/Paper_2/ridge_plot_conif2.pdf", conif_plot, width = 7, height = 11, units = "in", device = cairo_pdf)


