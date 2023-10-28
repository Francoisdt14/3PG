library(terra)
library(viridis)
library(rasterVis)

mask <- rast("D:/BP_Layers/M_9S/3PG_flt/6_90m_flt_other_inputs/Forest_Age_2019_withNA.flt")

# Load your raster data (replace with actual file paths)
raster_base <- rast("D:/3PG_Cplusplus/data_100/hillshade.flt")

# Plot the base raster
plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)  # Use "white" to set the background color


# Plot the overlay raster with 70% opacity- this is based
#plot(r3, add = TRUE, alpha = 0.9, legend = TRUE)

#plot(r4, add = TRUE, alpha = 0.9, col = viridis(100), breaks = seq(0, 280, length.out = 11), legend = FALSE)

r1s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws191007.flt")
r1f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf191007.flt")
r1 <- r1s+r1f
r1[r1 < 10] <- NA

r2s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws192007.flt")
r2f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf192007.flt")
r2 <- r2s+r2f
r2[r2 < 10] <- NA

r3s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws193007.flt")
r3f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf193007.flt")
r3 <- r3s+r3f
r3[r3 < 10] <- NA

r4s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws194007.flt")
r4f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf194007.flt")
r4 <- r4s+r4f
r4[r4 < 10] <- NA

r5s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws195007.flt")
r5f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf195007.flt")
r5 <- r5s+r5f
r5[r5 < 10] <- NA

r6s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws196007.flt")
r6f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf196007.flt")
r6 <- r6s+r6f
r6[r6 < 10] <- NA

r7s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws197007.flt")
r7f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf197007.flt")
r7 <- r7s+r7f
r7[r7 < 10] <- NA

r8s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws198007.flt")
r8f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf198007.flt")
r8 <- r8s+r8f
r8[r8 < 10] <- NA

r9s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws199007.flt")
r9f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf199007.flt")
r9 <- r9s+r9f
r9[r9 < 10] <- NA

r10s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws200007.flt")
r10f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf200007.flt")
r10 <- r10s+r10f
r10[r10 < 10] <- NA

r11s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws201007.flt")
r11f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf201007.flt")
r11 <- r11s+r11f
r11[r11 < 10] <- NA
crs(r11) <- crs(mask)
r11.1 <- terra::mask(r11, mask)


r12s <- rast("D:/3PG_Cplusplus/output_M_9S_lp/ws.flt")
r12f <- rast("D:/3PG_Cplusplus/output_M_9S_lp/wf.flt")
r12 <- r12s+r12f
#r12[r10 < 10] <- NA
crs(r12) <- crs(mask)
compareGeom(r12,mask)
r12.1 <- terra::mask(r12, mask)


# Set the maximum value for comparison
max_value <- 280

# Define the number of color classes (adjust as needed)
num_classes <- 10

# Calculate breaks for the continuous color scale
breaks <- seq(0, max_value, length.out = num_classes + 1)


# Plot the raster with a continuous color scale
# PNG device

png("G:/Sync/PostDoc/Figures/lp_current/r1.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r1, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1910", col = "red", cex = 1.5)
dev.off()


png("G:/Sync/PostDoc/Figures/lp_current/r2.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r2, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1920", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r3.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r3, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1930", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r4.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r4, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1940", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r5.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r5, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1950", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r6.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r6, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1960", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r7.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r7, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1970", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r8.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r8, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1980", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r9.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r9, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1990", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r10.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r10, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "2000", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r11.png", width = 8.2, height = 8, units = "in", res = 300)
plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r11.1, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "2010", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/lp_current/r12.png", width = 8.2, height = 8, units = "in", res = 300)
plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r12.1, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "2019", col = "red", cex = 1.5)
dev.off()

#########

# REGULAR TEMPS


#########




#plot(r12.1, alpha = 0.9, col = viridis(num_classes), legend = TRUE)




#plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
#plot(r6, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)

#plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
#plot(r7, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)

#######################################################################################################################################

# 18S

mask <- rast("D:/BP_Layers/M_18S/3PG_flt/6_90m_flt/Forest_Age_2019.flt")
# fix this - we need a version with NAs to mask out non-forest

# Load your raster data (replace with actual file paths)
raster_base <- rast("D:/BP_Layers/M_18S/inputs/hillshade.tif")

# Plot the base raster
plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)  # Use "white" to set the background color


# Plot the overlay raster with 70% opacity- this is based
#plot(r3, add = TRUE, alpha = 0.9, legend = TRUE)

#plot(r4, add = TRUE, alpha = 0.9, col = viridis(100), breaks = seq(0, 280, length.out = 11), legend = FALSE)

r1s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws191007.flt")
r1f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf191007.flt")
r1 <- r1s+r1f
r1[r1 < 10] <- NA

r2s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws192007.flt")
r2f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf192007.flt")
r2 <- r2s+r2f
r2[r2 < 10] <- NA

r3s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws193007.flt")
r3f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf193007.flt")
r3 <- r3s+r3f
r3[r3 < 10] <- NA

r4s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws194007.flt")
r4f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf194007.flt")
r4 <- r4s+r4f
r4[r4 < 10] <- NA

r5s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws195007.flt")
r5f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf195007.flt")
r5 <- r5s+r5f
r5[r5 < 10] <- NA

r6s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws196007.flt")
r6f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf196007.flt")
r6 <- r6s+r6f
r6[r6 < 10] <- NA

r7s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws197007.flt")
r7f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf197007.flt")
r7 <- r7s+r7f
r7[r7 < 10] <- NA

r8s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws198007.flt")
r8f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf198007.flt")
r8 <- r8s+r8f
r8[r8 < 10] <- NA

r9s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws199007.flt")
r9f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf199007.flt")
r9 <- r9s+r9f
r9[r9 < 10] <- NA

r10s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws200007.flt")
r10f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf200007.flt")
r10 <- r10s+r10f
r10[r10 < 10] <- NA

r11s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws201007.flt")
r11f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf201007.flt")
r11 <- r11s+r11f
r11[r11 < 10] <- NA
crs(r11) <- crs(mask)
r11.1 <- terra::mask(r11, mask)


r12s <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/ws202007.flt")
r12f <- rast("D:/3PG_Cplusplus/output_M_18S_bs_old/wf202007.flt")
r12 <- r12s+r12f
r12[r10 < 10] <- NA
crs(r12) <- crs(mask)
compareGeom(r12,mask)
r12.1 <- terra::mask(r12, mask)


# Set the maximum value for comparison
max_value <- 430

# Define the number of color classes (adjust as needed)
num_classes <- 10

# Calculate breaks for the continuous color scale
breaks <- seq(0, max_value, length.out = num_classes + 1)


# Plot the raster with a continuous color scale
# PNG device

png("G:/Sync/PostDoc/Figures/18S_bs/r1.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r1, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1910", col = "red", cex = 1.5)
dev.off()


png("G:/Sync/PostDoc/Figures/18S_bs/r2.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r2, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1920", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r3.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r3, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1930", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r4.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r4, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1940", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r5.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r5, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1950", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r6.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r6, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1960", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r7.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r7, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1970", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r8.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r8, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1980", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r9.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r9, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "1990", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r10.png", width = 8.2, height = 8, units = "in", res = 300)

plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r10, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "2000", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r11.png", width = 8.2, height = 8, units = "in", res = 300)
plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r11.1, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "2010", col = "red", cex = 1.5)
dev.off()

png("G:/Sync/PostDoc/Figures/18S_bs/r12.png", width = 8.2, height = 8, units = "in", res = 300)
plot(raster_base, col = gray.colors(256), legend = FALSE, alpha = 0.8)
plot(r12.1, add = TRUE, alpha = 0.9, col = viridis(num_classes), breaks = breaks, legend = FALSE)
text(x = (xmin(r11)+15000), y = (ymin(r11)+8000), labels = "2019", col = "red", cex = 1.5)
dev.off()


plot(r12.1, add = TRUE, alpha = 0.9, col = viridis(100), legend = TRUE)

