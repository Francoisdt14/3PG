###################################################################
# Run climateNA for this strip
library(tidyverse); library(terra); library(data.table); library(silvR21); library(parallel); library(pbapply)
library(fs); library(arrow)
rm(list = ls())
tmpFiles(remove = T)

# Get DEMS from ClimateNA
fl = list.files("E:/climate/dem_csv", recursive = T, full.names = T, pattern = ".csv$") %>% str_subset("UTM18S")
yrs = c("Y2", "Y3", "Y4", "Y5")
ssps = c("S1", "S2", "S3")

fy.df = expand.grid(fl, yrs, ssps, stringsAsFactors = F) %>% as.data.table()

cl = makeCluster(25)
clusterEvalQ(cl, {library(silvR21); library(tidyverse); setwd("D:/ClimateNA_v730")})

pbapply(fy.df, MARGIN = 1, FUN = function(rw){
  # pbapply(fy.df, MARGIN = 1, FUN = function(rw){
  f = rw[1]
  yr = rw[2]
  ssp.c = rw[3]

  ssp.names = data.frame(s = c("S1", "S2", "S3"), n = c("ssp126", "ssp245", "ssp370"))
  ssp.c.nm = ssp.names$n[which(ssp.names$s == ssp.c)]

  yr.names = data.frame(y = c("Y2", "Y3", "Y4", "Y5"), n = c("2021-2040", "2041-2060", "2061-2080", "2081-2100"))
  yr.c.nm = yr.names$n[which(yr.names$y == yr)]

  # Copy the file to the climateNA directory
  file.to = str_split(f, "/", simplify = T) %>% .[,ncol(.)]
  f.chk = str_replace(file.to, ".csv", paste0("_", yr, "_", ssp.c, ".csv")) %>%
    paste0("E:/climate/future/a1km/", .)
  file.copy(f, file.to)

  if(!file.exists(f.chk)){
    projClimateNA20Y(file.to, 'Y', "ClimateNA_v7.30.exe", scen = "8GCM", ssp = ssp.c, years = yr)
  }

  # rename the file
  a.f.out = list.files(str_remove(file.to, ".csv"), full.names = T) %>%
    str_subset(paste0(ssp.c.nm, "_", yr.c.nm))
  a.f.to = str_replace(a.f.out, "/8GCMs", "/annual_8GCMs")
  file.rename(a.f.out, a.f.to)

  f.chk2 = str_replace(file.to, ".csv", paste0("_", yr, "_", ssp.c, ".csv")) %>%
    paste0("E:/climate/future/s1km/", .)
  if(!file.exists(f.chk2)){
    file.copy(f, file.to)
    projClimateNA20Y(file.to, 'S', "ClimateNA_v7.30.exe", scen = "8GCM", ssp = ssp.c, years = yr)
  }
  # rename the file
  s.f.out = list.files(str_remove(file.to, ".csv"), full.names = T) %>%
    str_subset(paste0(ssp.c.nm, "_", yr.c.nm)) %>% str_subset("annual", negate = T)
  s.f.to = str_replace(s.f.out, "/8GCMs", "/seasonal_8GCMs")
  file.rename(s.f.out, s.f.to)

  # Copy to the other directory
  file.copy(a.f.to, f.chk)
  file.copy(s.f.to, f.chk2)

  # delete the files made during this step
  file.remove(c(a.f.to, s.f.to))

}, cl = cl)
stopCluster(cl)
