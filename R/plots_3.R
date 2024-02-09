library(terra)
library(rasterVis)
library(viridis)
library(grid)
library(gridExtra)
#################################################
# Creating plots

########################################################################################################
area = "U_18S"

s1.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S1/Y3_Output/ws205007.flt"))
s1.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S1/Y3_Output/wf205007.flt"))

s2.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S2/Y3_Output/ws205007.flt"))
s2.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S2/Y3_Output/wf205007.flt"))

s3.ws <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S3/Y3_Output/ws205007.flt"))
s3.wf <- rast(file.path("D:/BP_Layers", area, "biomass_3PG/S3/Y3_Output/wf205007.flt"))

# calculate agb for each scenario in 2080
s1 <- s1.ws + s1.wf
s2 <- s2.ws + s2.wf
s3 <- s3.ws + s3.wf

# Load the masks
fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/fao_forest_90m.tif"))
Sh_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_90m.tif"))
ShBy_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_Bry_90m.tif"))
Sh_fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_fao_90m.tif"))
ShBy_fao_mask <- rast(file.path("D:/BP_Layers", area, "landcover/Sh_Bry_fao_90m.tif"))

# Get the EPSG code from fao.mask
epsg_code <- crs(fao_mask, describe = T)$code
# Set the CRS of s1 using the extracted EPSG code
crs(s1) <- paste0("EPSG:", epsg_code)
crs(s2) <- paste0("EPSG:", epsg_code)
crs(s3) <- paste0("EPSG:", epsg_code)


######

fao.11S <- rast("D:/BP_Layers/M_11S/analysis/s1_fao_mask.tif")
shrub.11S <- rast("D:/BP_Layers/M_11S/analysis/s1_Sh_mask.tif")
shby.11S <- rast("D:/BP_Layers/M_11S/analysis/s1_ShBy_mask.tif")


fao.18S <- rast("D:/BP_Layers/U_18S/analysis/s1_fao_mask.tif")
shrub.18S <- rast("D:/BP_Layers/U_18S/analysis/s1_Sh_mask.tif")
shby.18S <- rast("D:/BP_Layers/U_18S/analysis/s1_ShBy_mask.tif")


###########################################


# Create the plot
terra::plot(fao.18S, col = viridis(100), main = "Study Area U 18S, Scenario 1, FAO Mask", legend = T, mar = c(2, 2, 2, 2), cex.main = 0.8)  # Using the viridis color palette and setting the title
# Add a scale bar in kilometers
sbar(50000, xy = "bottomright", divs = 3, cex = 0.8, ticks = TRUE, labels = c(0, 25, 50), col = 'deeppink2', below="kilometers")
# Add a North arrow
north(xy = 'topleft', label = 'N', type = 2, col = "deeppink2")
legend("bottomright", legend = "Biomass (tDM/ha)", xpd = NA, bty = "n", , x.intersp = 0)


# Create the plot
terra::plot(shrub.18S, col = viridis(100), main = "Study Area U 18S, Scenario 1, Shrub Mask", legend = T, mar = c(2, 2, 2, 2), cex.main = 0.8)  # Using the viridis color palette and setting the title
# Add a scale bar in kilometers
sbar(50000, xy = "bottomright", divs = 3, cex = 0.8, ticks = TRUE, labels = c(0, 25, 50), col = 'deeppink2', below="kilometers")
# Add a North arrow
north(xy = 'topleft', label = 'N', type = 2, col = "deeppink2")
legend("bottomright", legend = "Biomass (tDM/ha)", xpd = NA, bty = "n")


# Create the plot
terra::plot(shby.18S, col = viridis(100), main = "Study Area U 18S, Scenario 1, Shrub and Bryoid Mask", legend = T, mar = c(2, 2, 2, 2), cex.main = 0.8,)  # Using the viridis color palette and setting the title
# Add a scale bar in kilometers
sbar(50000, xy = "bottomright", divs = 3, cex = 0.8, ticks = TRUE, labels = c(0, 25, 50), col = 'deeppink2', below="kilometers")
# Add a North arrow
north(xy = 'topleft', label = 'N', type = 2, col = "deeppink2")
legend("bottomright", legend = "Biomass (tDM/ha)", xpd = NA, bty = "n")



# Create the plot
terra::plot(fao_mask, main = "Study Area U 18S\nFAO Forest Mask", legend = F, mar = c(2, 2, 2, 2), cex.main = 0.8, col = "#3CBB75FF")  # Using the viridis color palette and setting the title
# Add a scale bar in kilometers
sbar(50000, xy = "bottomright", divs = 3, cex = 0.8, ticks = TRUE, labels = c(0, 25, 50), col = 'deeppink2', below="kilometers")
# Add a North arrow
north(xy = 'topleft', label = 'N', type = 2, col = "deeppink2")


# Create the plot
terra::plot(Sh_mask, main = "Study Area U 18S\nShrub Mask", legend = F, mar = c(2, 2, 2, 2), cex.main = 0.8, col = "#440154FF")  # Using the viridis color palette and setting the title
# Add a scale bar in kilometers
sbar(50000, xy = "bottomright", divs = 3, cex = 0.8, ticks = TRUE, labels = c(0, 25, 50), col = 'deeppink2', below="kilometers")
# Add a North arrow
north(xy = 'topleft', label = 'N', type = 2, col = "deeppink2")


# Create the plot
terra::plot(ShBy_mask, main = "Study Area U 18S\nShrub and Bryoid Mask", legend = F, mar = c(2, 2, 2, 2), cex.main = 0.8, col = "#238A8DFF")  # Using the viridis color palette and setting the title
# Add a scale bar in kilometers
sbar(50000, xy = "bottomright", divs = 3, cex = 0.8, ticks = TRUE, labels = c(0, 25, 50), col = 'deeppink2', below="kilometers")
# Add a North arrow
north(xy = 'topleft', label = 'N', type = 2, col = "deeppink2")


# Create the plot
terra::plot(Sh_mask, legend = F, mar = c(2, 2, 2, 2), cex.main = 0.8, col = "#440154FF", add = T)  # Using the viridis color palette and setting the title
