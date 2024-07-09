# Distribution plots for different 3-PG executables

library(terra)
library(tidyverse)
library(ggridges)
library(patchwork)


# When looking at a single year
# load in rasters of each .exe
flt_ncc <- rast("D:/3PG_EXE_TESTS/output_10yr_FLT_NCC/ws190907.flt")
flt_vz <- rast("D:/3PG_EXE_TESTS/output_10yr_FLT_VZ/ws190907.flt")
flt_fdt <- rast("D:/3PG_EXE_TESTS/output_10yr_FLT_FDT/ws190907.flt")

# stack and turn into dataframe
r_stack <- c(flt_ncc, flt_vz, flt_fdt)
r_values <- values(r_stack, na.rm = TRUE)
test <- r_values %>% as.data.frame()
colnames(test) <- c("flt_ncc","flt_vz", "flt_fdt")

# Reshape the data from wide to long format
test_long <- test %>%
    pivot_longer(cols = c(flt_ncc, flt_vz, flt_fdt), names_to = "model_type", values_to = "biomass_value")

# ridge plot the dataframes
ggplot(test_long, aes(x = biomass_value, y = model_type, fill = model_type)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Biomass Values for 3-PG Models",
         x = "Biomass Value", y = NULL) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_bw()

######################
# Looking at plotting multiple years for every .exe (thank you chatgpt)

# Function to read and process files for a given year
process_year <- function(year, base_paths) {
    year_str <- sprintf("%02d", year)  # Convert year to two-digit string
    r_list <- map(base_paths, ~rast(paste0(.x, "ws19", year_str, "07.flt")))

    r_values <- map(r_list, ~values(.x, na.rm = TRUE)) %>%
        do.call(cbind, .)

    df <- as.data.frame(r_values)
    colnames(df) <- names(base_paths)
    df$year <- as.factor(1900 + year)
    return(df)
}

# Base paths - you can add as many as you want
base_paths <- list(
    flt_vz = "D:/3PG_EXE_TESTS/output_10yr_FLT_VZ/",
    flt_fdt = "D:/3PG_EXE_TESTS/output_10yr_FLT_FDT/",
    flt_ncc = "D:/3PG_EXE_TESTS/output_10yr_FLT_NCC/"
)

# Process data for years 1901 to 1909
years <- 1:9
all_data <- map_dfr(years, ~process_year(.x, base_paths))

# Reshape the data from wide to long format
all_data_long <- all_data %>%
    pivot_longer(cols = all_of(names(base_paths)), names_to = "model_type", values_to = "biomass_value")

# Sample the data because there is a lot of it
set.seed(123)  # for reproducibility
all_data_long_sampled <- all_data_long %>% sample_n(100000)

# Function to create ridge plot
create_ridge_plot <- function(data, model_type, title) {
    data %>%
        filter(model_type == !!model_type) %>%
        ggplot(aes(x = biomass_value, y = year, fill = year)) +
        geom_density_ridges(alpha = 0.7, scale = 3, rel_min_height = 0.01) +
        scale_x_continuous(limits = c(4, 20)) +
        labs(title = title, x = "Biomass Value", y = "Year") +
        scale_fill_viridis_d(option = "magma") +
        theme_ridges() +
        theme(legend.position = "none")
}

# Create ridge plots
plots <- map2(names(base_paths), names(base_paths),
              ~create_ridge_plot(all_data_long_sampled, .x, .y))

# Combine plots side by side
combined_plot <- reduce(plots, `+`)

# Display the combined plot
print(combined_plot)
