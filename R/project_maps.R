library(terra)

# Project study layers


shp <- vect("D:\\PostDoc_Maps_Original\\Layers\\BP_Maps\\Forested_ecozones\\Forested_ecozones_cliped.shp")

shp.orig <- vect("D:/PostDoc_Maps_Original/Layers/BP_Maps/ManagedForests.shp")
shp.orig

shp.proj <- project(shp.orig, shp)

shp3 <- vect("D:/Landcover/francois5/Study_Area_M_eighteenS.shp")
shp3.proj <- project(shp3, shp)

shp4 <- vect("D:/Landcover/francois5/Study_Area_M_nineS.shp")
shp4.proj <- project(shp4, shp)

