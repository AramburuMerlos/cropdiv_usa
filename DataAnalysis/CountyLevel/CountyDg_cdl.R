library(raster)
library(data.table)

# Functions --------
source('Functions/CrossTab.R')
source('Functions/Diversity.R')
source('Functions/DoubleCrops_DT.R')


# upload files -------
m <- read.csv("DblCropMatrix.csv")
GEOID <- raster("D:/USrasterized/CountiesGEOID_30m.tif")

# All years ===================================================================
year <- 2008:2017

# get crop frequencies for each county and in each year
dtl <- vector(mode = 'list', length = 10)

for(i in 1:length(year)){
  cropID <- raster(paste0("D:/ReclMaskedData/CDL", year[i], "RCM.tif"))
  dtl[[i]] <- CrossTab(x = GEOID, y = cropID)
}

# add the ten years and save .csv
dt <- rbindlist(dtl)
dt <- dt[, .(freq = sum(freq, na.rm = TRUE)), by = .(GEOID, cropID)]
fwrite(dt, "Results/CDL_CountyCropFreq.csv")

dt <- fread("Results/CDL_CountyCropFreq.csv")
# reassign double crop frequencies to their corresponding components
dt <- dc_dt(dt, m)
# calculate diversity and save
Dg_CDL <- dt[, .(Dg = diver(freq)), by = .(GEOID)]
fwrite(Dg_CDL, "Results/CountyDg_cdl.csv")

# Only 2012 ===================================================================
# (to be compared against census data)
cropID <- raster(paste0("D:/ReclMaskedData/CDL2012RCM.tif"))
dt2012 <- CrossTab(x = GEOID, y = cropID)
fwrite(dt2012, "Results/CDL_CountyCropFreq_2012.csv")

dt2012 <- fdc(dt2012, m)

Dg_CDL_2012 <- dt2012[, .(Dg = f(freq)), by = .(GEOID)]
fwrite(Dg_CDL_2012, "Results/CountyDg_2012cdl.csv")
