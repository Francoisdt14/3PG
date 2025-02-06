library(terra)


M_18S_spec <- rast("D:/Landcover/francois2/Shapefiles/Study_Area_M_eighteenS/18S/species/leading_species_201918S.dat")
M_11S_spec <- rast("")

shp_test <- vect('D:/Landcover/francois2/Shapefiles/Study_Area_M_eighteenS.shp')

M_11S_1 <- rast('D:/Landcover/francois2/Shapefiles/Study_Area_M_elevenS/mosaiced/species/leading_species_201910S.dat')
M_11S_2 <- rast('D:/Landcover/francois2/Shapefiles/Study_Area_M_elevenS/mosaiced/species/leading_species_201911S.dat')

M_11S_spec <- merge(M_11S_1, M_11S_2)
M_11S_spec_2 <- mosaic(M_11S_1, M_11S_2)


M_11_spec <- rast("Y:/Muise/francois5/Study_Area_M_elevenS/mosaiced/species/leading-species_2019.dat")
M_11_age <- rast("Y:/Muise/francois5/Study_Area_M_elevenS/mosaiced/age/Forest_Age_2019.dat")

mask <- rast("D:/BP_Layers/M_11S/tree_mask.tif")

M_11_spec_p <- project(M_11_spec, mask, method = 'near', mask = TRUE,threads = TRUE)

compareGeom(mask, M_11_spec_p)

############################################################################################
spec <- raster_list[[6]]

spec.df <- values(spec, na.rm = TRUE) %>% as.data.frame()

head(spec.df)

# Get the category names and their corresponding numeric codes
cat_data <- levels(spec)[[1]]
#print(cat_data)
names(cat_data) <- c("ID", "Category")

# Count the occurrences of each category
spec_counts <- table(spec.df$category)
spec_counts_df <- as.data.frame(spec_counts)
names(spec_counts_df) <- c("ID", "Count")

# Merge the category names with the counts
result <- merge(cat_data, spec_counts_df, by = "ID", all = TRUE)

result$Count[is.na(result$Count)] <- 0

# Calculate percentages
total_pixels <- sum(result$Count)
result$Percentage <- (result$Count / total_pixels) * 100

# Sort the results by percentage in descending order
result <- result[order(-result$Percentage), ]

# If you want to exclude the "Non-Treed" category from your calculations
result_treed <- result[result$Category != "Non-Treed", ]

total_treed_pixels <- sum(result_treed$Count)

result_treed$Percentage <- (result_treed$Count / total_treed_pixels) * 100
result_treed <- result_treed[order(-result_treed$Percentage), ]
print(result_treed)

tree_species <- read.table("D:/Landcover/species_codes.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

result_treed_with_type <- merge(result_treed, tree_species[c("Code", "Type")], by.x = "Category", by.y = "Code", all.x = TRUE)
result_treed_with_type_o <- result_treed_with_type[order(-result_treed_with_type$Percentage), ]

print(result_treed_with_type_o)

total_percentage <- sum(result_treed_with_type$Percentage)
coniferous_percentage <- sum(result_treed_with_type$Percentage[result_treed_with_type$Type == "Coniferous"])
deciduous_percentage <- sum(result_treed_with_type$Percentage[result_treed_with_type$Type == "Deciduous"])

# Normalize to ensure they sum to 100%
coniferous_percentage_normalized <- (coniferous_percentage / total_percentage) * 100
deciduous_percentage_normalized <- (deciduous_percentage / total_percentage) * 100



#########################################################
raster_list <- list(
    M_18S = rast("Y:/Muise/francois5/Study_Area_M_eighteenS/18S/species/leading-species_2019_18S.dat"),
    M_11S = rast("Y:/Muise/francois5/Study_Area_M_elevenS/mosaiced/species/leading-species_2019.dat"),
    M_9S = rast("Y:/Muise/francois5/Study_Area_M_nineS/mosaiced/species/leading-species_2019.dat"),
    U_18S = rast("Y:/Muise/francois5/Study_Area_U_eighteenS/mosaiced/species/leading-species_2019.dat"),
    U_15S = rast("Y:/Muise/francois5/Study_Area_U_fifteenS/mosaiced/species/leading-species_2019.dat"),
    U_13N = rast("Y:/Muise/francois5/Study_Area_U_thirteenN/mosaiced/species/leading-species_2019.dat")
)


process_raster <- function(raster, tree_species) {
    spec.df <- values(raster, na.rm = TRUE) %>% as.data.frame()

    cat_data <- levels(raster)[[1]]
    names(cat_data) <- c("ID", "Category")

    spec_counts <- table(spec.df$category)
    spec_counts_df <- as.data.frame(spec_counts)
    names(spec_counts_df) <- c("ID", "Count")

    result <- merge(cat_data, spec_counts_df, by = "ID", all = TRUE)
    result$Count[is.na(result$Count)] <- 0

    #total_pixels <- sum(result$Count)
    #result$Percentage <- (result$Count / total_pixels) * 100

    result_treed <- result[result$Category != "Non-Treed", ]
    total_treed_pixels <- sum(result_treed$Count)
    result_treed$Percentage <- (result_treed$Count / total_treed_pixels) * 100

    result_treed_with_type <- merge(result_treed, tree_species[c("Code", "Type")], by.x = "Category", by.y = "Code", all.x = TRUE)

    total_percentage <- sum(result_treed_with_type$Percentage)
    coniferous_percentage <- sum(result_treed_with_type$Percentage[result_treed_with_type$Type == "Coniferous"])
    deciduous_percentage <- sum(result_treed_with_type$Percentage[result_treed_with_type$Type == "Deciduous"])

    coniferous_percentage_normalized <- (coniferous_percentage / total_percentage) * 100
    deciduous_percentage_normalized <- (deciduous_percentage / total_percentage) * 100

    return(c(coniferous = coniferous_percentage_normalized, deciduous = deciduous_percentage_normalized))
}


# Read the tree species data
tree_species <- read.table("D:/Landcover/species_codes.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Process all rasters
results <- lapply(names(raster_list), function(study_area) {
    percentages <- process_raster(raster_list[[study_area]], tree_species)
    c(study_area = study_area, t(percentages))
})

# Convert to a data frame
final_table <- do.call(rbind, results) %>% as.data.frame()

# Rename columns
names(final_table) <- c("study_area", "coniferous_percentage", "deciduous_percentage")

# Print the final table
print(final_table)

write.csv(final_table, "D:/BP_Layers/analysis/paper_2/deciduous_percentages.csv")
#####################################

library(terra)
library(tidyverse)
library(ggridges)
library(purrr)

# Define base path
base_path <- "D:/BP_Layers/" # for coniferous
base_path <- "I:/data_2024_05_02_deciduous/" # for deciduous

# Define study areas, scenarios, and years
study_areas <- c("M_9S")
#Scenarios <- c("S1", "S2", "S3")
scenarios <- c("S1_dec", "S2_dec", "S3_dec") # for deciduous
years <- c(2030, 2040, 2050, 2060, 2070, 2080)

# Function to create and save rasters
create_raster <- function(study_area, scenario, year, base_path) {
    Sh_mask <- rast(paste0("D:/BP_Layers/", study_area, "/landcover/Sh_90m.tif"))

    # Construct file names for ws and wf - double check for deciduous
    #ws_file <- paste0(base_path, study_area, "/biomass_3PG/", scenario, "/combined_years/ws", year, "07.flt")
    #wf_file <- paste0(base_path, study_area, "/biomass_3PG/", scenario, "/combined_years/wf", year, "07.flt")

    ws_file <- paste0(base_path, study_area,"/", scenario, "/combined_years/ws", year, "07.flt")
    wf_file <- paste0(base_path, study_area,"/", scenario, "/combined_years/wf", year, "07.flt")

    con.ws <- rast(ws_file)
    con.wf <- rast(wf_file)

    con1 <- con.ws + con.wf
    crs(con1) <- crs(Sh_mask)

    con1_mask <- terra::mask(con1, Sh_mask)

    # Create output directory if it doesn't exist
    output_dir <- paste0(base_path, study_area, "/biomass_3PG/", scenario, "/year_masked_rasters")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Save the raster as a TIF file
    output_file <- paste0(output_dir, "/", year, ".tif")
    writeRaster(con1_mask, output_file, overwrite = TRUE)

    return(output_file)
}

# Create rasters for all combinations
raster_files <- expand.grid(study_area = study_areas, scenario = scenarios, year = years) %>%
    pmap_chr(~create_raster(..1, ..2, ..3, base_path))

####################

# Define scenarios and years
#scenarios <- c("S1", "S2", "S3")
scenarios <- c("S1_dec", "S2_dec", "S3_dec") # for deciduous
years <- c(2030, 2040, 2050, 2060, 2070, 2080)

# Function to read and sample raster data
read_and_sample_raster <- function(scenario, year) {
    #raster_path <- paste0("D:/BP_Layers/M_9S/biomass_3PG/", scenario, "/year_masked_rasters/", year, ".tif")
    raster_path <- paste0("I:/data_2024_05_02_deciduous/M_9S/biomass_3PG/", scenario, "/year_masked_rasters/", year, ".tif")
    rast_data <- rast(raster_path)
    values <- values(rast_data, na.rm = TRUE)

    # Sample 100,000 points or all points if less than 100,000
    sample_size <- min(100000, length(values))
    sampled_values <- sample(values, size = sample_size)

    return(data.frame(biomass = sampled_values, year = as.factor(year), scenario = scenario))
}

# Read and sample all rasters
all_data <- map2_dfr(
    rep(scenarios, each = length(years)),
    rep(years, times = length(scenarios)),
    read_and_sample_raster
)

# Create ridge plot function
create_ridge_plot <- function(data, scenario) {
    ggplot(data %>% filter(scenario == !!scenario),
           aes(x = biomass, y = year, fill = year)) +
        geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.8) +
        scale_fill_viridis_d() +
        labs(title = paste("Biomass Distribution Over Time -", scenario),
             x = "Biomass", y = "Year") +
        theme_ridges() +
        theme(legend.position = "none") +
    scale_x_continuous(limits = c(0, 400))  # Set x-axis limits here
}

# Create ridge plots for each scenario
plots <- map(scenarios, ~create_ridge_plot(all_data, .x))

# Define the output folder for PDF files
figure_folder <- "G:/Sync/PostDoc/Figures/Paper_2/paper_figs"

# Save plots as PDFs
walk2(plots, scenarios, ~ggsave(
    filename = file.path(figure_folder, paste0("decid_ridge_M-9S_", .y, ".pdf")),
    plot = .x,
    width = 10,
    height = 8,
    dpi = 300
))


#################################



# Load necessary libraries
library(terra)
library(dplyr)
library(purrr)
library(ggplot2)

# Define the function to read and sample raster data
read_and_sample_raster <- function(scenario, year, species) {
    base_path <- ifelse(species == "deciduous",
                        #"I:/data_2024_05_02_deciduous/M_9S/biomass_3PG/",
                        "D:/BP_Layers/M_9S/biomass_3PG_decid/",
                        "D:/BP_Layers/M_9S/biomass_3PG/")
    raster_path <- paste0(base_path, scenario, "/year_masked_rasters/", year, ".tif")
    rast_data <- rast(raster_path)
    values <- values(rast_data, na.rm = TRUE)

    # Sample 100,000 points or all points if less than 100,000
    sample_size <- min(100000, length(values))
    sampled_values <- sample(values, size = sample_size)

    return(data.frame(biomass = sampled_values, year = as.factor(year), scenario = scenario, species = species))
}

# Define scenarios and years
scenarios <- "S2"
years <- c(2030, 2040, 2050, 2060, 2070, 2080)

# Read and sample all rasters for deciduous species
deciduous_data <- map2_dfr(
    rep(paste0(scenarios, "_dec"), each = length(years)),
    rep(years, times = length(scenarios)),
    ~read_and_sample_raster(.x, .y, "deciduous")
)

# Read and sample all rasters for coniferous species
coniferous_data <- map2_dfr(
    rep(scenarios, each = length(years)),
    rep(years, times = length(scenarios)),
    ~read_and_sample_raster(.x, .y, "coniferous")
)

# Combine the dataframes
all_data <- rbind(deciduous_data, coniferous_data)



# Create the plot
accum_plot <- ggplot(all_data, aes(x = year, y = biomass, fill = species)) +
    geom_boxplot(position = position_dodge(width = 0.6), width = 0.3, outlier.shape = NA) +
    scale_fill_manual(values = c("deciduous" = "#fe9f6d", "coniferous" = "#b73779")) + # Purple and dark grey from magma color scale
    labs(title = "Biomass Comparison: Deciduous vs Coniferous (S2 Scenario)",
         x = "Year", y = "Biomass", fill = "Species") +
    theme_cwm() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = quantile(all_data$biomass, c(0.01, 0.99))) # Adjust y-axis to remove extreme outliers


#ggsave("G:/Sync/PostDoc/Figures/Paper_2/biomass_accumulation.pdf", accum_plot, width = 11, height = 9, units = "in", device = cairo_pdf)


#######
# Create the plot
accum_plot <- ggplot(all_data, aes(x = year, y = biomass, fill = species)) +
    geom_boxplot(position = position_dodge(width = 0.6), outlier.shape = NA) +
    scale_fill_manual(values = c("deciduous" = "#fe9f6d", "coniferous" = "#b73779")) + # Purple and dark grey from magma color scale
    labs(title = "Biomass Comparison: Deciduous vs Coniferous (CS2)",
         x = "Year", y = "Biomass", fill = "Species") +
    theme_cwm() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = quantile(all_data$biomass, c(0.01, 0.99)))+

    # Make vertical lines for years dashed and dark grey
    geom_vline(xintercept=1:6 - 0.5, linetype="dashed", color="lightgrey", size=0.5) # Adjust y-axis to remove extreme outliers

ggsave("G:/Sync/PostDoc/Figures/Paper_2/biomass_accumulation_update.pdf", accum_plot, width = 11, height = 9, units = "in", device = cairo_pdf)

#######


#######

# Create box plots for each planting scenario across the years
box_plot <- ggplot(data2, aes(x = factor(Year), y = Total_Biomass, fill = Planting_Condition)) +
    geom_boxplot(position = position_dodge(width = 0.8), color = "black") +
    labs(title = "Total Biomass by Planting Condition (Climate Scenario 2)",
         x = "Year",
         y = "Total Biomass") +
    theme_minimal() +
    scale_fill_manual(values = c("Coniferous" = "lightgreen", "Deciduous" = "lightblue", "Mixed" = "lightcoral")) +
    theme(legend.title = element_blank()) +

    # Make vertical lines for years dashed and dark grey
    geom_vline(xintercept=1:6 - 0.5, linetype="dashed", color="darkgrey", size=1)

# Display the box plot
print(box_plot)



test.rast <- rast("D:/BP_Layers/M_9S/biomass_3PG_decid/S2_dec/year_masked_rasters/2030.tif")
plot(test.rast)
test.rast
