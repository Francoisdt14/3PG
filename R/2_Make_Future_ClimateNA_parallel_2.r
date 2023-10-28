###################################################################
# Run climateNA for this strip
library(tidyverse); library(terra); library(data.table); library(silvR21); library(parallel); library(pbapply)
library(fs); library(arrow)
rm(list = ls())
tmpFiles(remove = T)

# Get DEMS from ClimateNA
#fl = list.files("D:/climate/Future/dem_csv", recursive = T, full.names = T, pattern = ".csv$") %>% str_subset("M_9S")

#fl <- "D:/climate/M_9S/dem_crop_M_9S.csv"
fl <- "D:/climate/M_11S/dem_crop_M_11S.csv"
yrs = c("Y2", "Y3", "Y4", "Y5")
ssps = c("S1", "S2", "S3")

fy.df = expand.grid(fl, yrs, ssps, stringsAsFactors = F) %>% as.data.table()

# is this the issue?!
#file.to <- "D:/ClimateNA_v730/dem_crop_M_18S.csv"

cl = makeCluster(12)

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
    #file.to = str_split(f, "/", simplify = T) %>% .[,ncol(.)] # this is within the climateNA directory

    file.to <- "D:/ClimateNA_v730/dem_crop_M_11S.csv"

    file.simplify = str_split(f, "/", simplify = T) %>% .[,ncol(.)] # this is within the climateNA directory

    f.chk = str_replace(file.simplify, ".csv", paste0("_", yr, "_", ssp.c, ".csv")) %>%
        paste0("D:/climate/Future/M_11S/", .) # this is where we eventually want it to go

    #file.copy(f, file.to) # copy to climateNA wd

    # run cna
    if(!file.exists(f.chk)){
        projClimateNA(file.to, "M", "ClimateNA_v7.30.exe", scen = "13GCM", ssp = ssp.c, years = yr)
    }

    # rename the file

    a.f.out = list.files(str_remove(file.simplify, ".csv"), full.names = T) %>%
        str_subset(paste0(ssp.c.nm, "_", yr.c.nm)) # this is the name of the new file that climateNA makes

    # Copy to the other directory
    file.copy(a.f.out, f.chk)

    # delete the files made during this step
    file.remove(a.f.out)

}, cl = cl)
stopCluster(cl)



########################
# RANDOM TESTING

############################

unique_data <- test[!duplicated(test), ]

# Combine ID1 and ID2 columns into a single column
test.dem_combined <- test.dem %>%
    mutate(combined_ID = paste(ID1, ID2, sep = "_"))

unique_data_combined <- unique_data %>%
    mutate(combined_ID = paste(ID1, ID2, sep = "_"))

unique_data_combined <- unique_data_combined[, 1:5]
colnames(unique_data_combined) <- c("ID1", "ID2", "lat", "long", "el")
unique_data_combined$long <- unique_data_combined$long*-1


# Find rows with differences in any column
different_rows <- unique_data_combined[!(unique_data_combined %in% test.dem), ]

# Print out rows with differences
cat("Rows with differences:\n")
print(different_rows)
