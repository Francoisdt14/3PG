
df <- data.frame(matrix(sample(1:10, 5*20, replace=TRUE), nrow=20))
colnames(df) <- paste0("col", 1:5)

# Set rows 5 and 6 to NA
df[5:6, ] <- NA

# Set some values to NA
df[1, 5] <- NA
df[2, 5] <- NA
df[9, 5] <- NA

df <- as.data.frame(df)


# Replace NA values in column 5 with 0 if they are the only NA value in the row
df$col5 <- ifelse(rowSums(is.na(df)) == 1 & is.na(df$col5), 20, df$col5)



lat.44 <- read.csv("D:/BP_Layers/outputs/crops/lat/044.csv")
lat.44.test <- read.csv("D:/BP_Layers/outputs/crops/lat/044_test.csv")

######################################################################################################

test.df <- out_3PG[out_3PG$date == '2019-12-31', ]
# out_3PG -> contains all the data


i_var <- c("npp", "gpp", "tmax", "lai", "dbh", "basal_area", "age", "frost_days", "prcp")
i_lab <- c("npp", "gpp", "tmax", "lai", "dbh", "basal_area", "age", "frost_days", "prcp")

plot <- out_3PG %>%
    filter(variable %in% i_var) %>%
    mutate(variable = factor(variable, levels = i_var)) %>%
    ggplot( aes(date, value))+
    geom_line( aes(color = species), linewidth = 0.5)+
    facet_wrap( ~ variable, scales = 'free_y', ncol = 3,
                labeller = labeller(variable = setNames(i_lab, i_var) )) +
    scale_color_brewer('', palette = 'Dark2') +
    theme_classic()+
    theme(legend.position="bottom")+
    xlab("Calendar date") + ylab('Value')


library(lubridate)

# assume your data frame is named 'my_data'
# extract the year from the date column
out_3PG$year <- year(out_3PG$date)

# filter the data for the final year and the 'npp' variable
final_year_data <- out_3PG %>% filter(year == 1935 & variable == "npp")

# then, use the 'sum' function to add up all of the npp values for the final year
total_npp <- sum(final_year_data$value)

table(inputs_df$leading.species_2019)


sort(table(inputs_df$leading.species_2019), decreasing = TRUE)

species_rast <- rast("D:/BP_Layers/outputs/inputs/leading-species_2019.tif")
plot(species_rast)

#r_unique <- unique(round(species_rast))

r_rounded <- round(species_rast)

freq <- table(as.vector(r_rounded))

freq_ordered <- sort(freq, decreasing = TRUE)

head(freq_ordered)

species_rast2 <- rast("D:/BP_Layers/M_9S/species/leading-species_2019.dat")
plot(species_rast2)

freq2 <- table(as.vector(species_rast2))

freq_ordered2 <- sort(freq2, decreasing = TRUE)

head(freq_ordered2)


######################################################################################
# PLoitting species info
library(tidyverse)
library(readxl)
# Currently using customized parameters (but can use parameters directly from the vignette)
datasheet <- 'D:/Sync/PostDoc/species_tests/Spruce_Jack_Lodge_Parameters.xlsx'

Comb <-  read_xlsx(datasheet, 'Comb')


# Subset the dataframe to only include the Basal Area parameter
df_subset <- subset(Comb, Parameter == "Basal Area")
df_subset2 <- subset(Comb, Parameter == "DBH")
# Create the plot with ggplot2
p1 <- ggplot(df_subset, aes(x = Reference, y = `3-PG`, color = Species)) +
    geom_point(size = 2.5) +
    scale_x_continuous(name = expression(paste("Reference Data (m"^"2"*" ha"^"-1"*")"))) + # use expression function to format the axis label with superscripts
    scale_y_continuous(name = expression(paste("3-PG Data (m"^"2"*" ha"^"-1"*")"))) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.7) +
    theme(aspect.ratio = 1) + theme_classic() +
    scale_color_manual(values = c("magenta2", "gold2", "turquoise")) +
    coord_equal()


p2 <- ggplot(df_subset2, aes(x = Reference, y = `3-PG`, color = Species)) +
    geom_point(size = 2.5) +
    scale_x_continuous(name = "Reference Data (cm)") +
    scale_y_continuous(name = "3-PG Data (cm)") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.7) +
    theme(aspect.ratio = 1) + theme_classic() +
    scale_color_manual(values = c("magenta2", "gold2", "turquoise")) +
    coord_equal()


ggsave("C:/Users/fdutoit.stu/Sync/PostDoc/species_tests/basal_area.png", p1, width = 6, height = 6, dpi = 300)

#######################################

# Load the ggplot2 and ggmap packages
library(ggplot2)
library(ggmap)

# Read the CSV file into a data frame
data <- read.csv("D:/Sync/PostDoc/species_tests/study_locations.csv")

# Define a vector of species names
species_names <- unique(data$Species)

# Define a vector of colors to map to the species names
species_colors <- c("magenta2", "turquoise", "gold2") # Add more colors as needed

# Define a named vector that maps each species name to a color
species_color_map <- setNames(species_colors[1:length(species_names)], species_names)

# Add a new column to the data frame that maps each species name to a color
data$SpeciesColor <- species_color_map[data$Species]

# Plot the sample sites and color them by the 'SpeciesColor' column
points(data$Longitude, data$Latitude, pch = 19, cex = 0.5, col = data$SpeciesColor)


############################################################################

library(maps)
library(mapdata)
library(maptools)

provinces <- readOGR(dsn = "Z:/_CanadaLayers/Vectors/txomin/ca_prov.shp")

shapefile <- readOGR(dsn ="D:/PostDoc/Layers/BP_Maps/ManagedForests.shp")

points(data$Longitude, data$Latitude, pch=19, cex=1)  #plot my sample sites



plot(provinces)
plot(shapefile, add = T)
#addShapefile(shapefile, col = "blue", border = "black")

maps::map(database = "world", regions = "Canada",ylim=c(40,75),col="gray90", fill=TRUE)
points(data$Longitude, data$Latitude, pch=19, col=data$SpeciesColor, cex=1)  #plot my sample sites
text(data$Longitude, data$Latitude, labels = data$Study.Area, pos = 3, offset = 1)
