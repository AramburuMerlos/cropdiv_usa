library(raster)
library(data.table)
source("Functions/CrossTab.R")

Dt <- raster("D:/TmpDivData/TmpDiv.tif")
rnames <- paste0("D:/ReclMaskedData/CDL", 
                 2008:2017, 
                 "RCM.tif")
rlist <- lapply(rnames, raster)
nr <- length(rlist)
dlist <- vector(mode = "list", length = nr)

for(i in 1:nr){
  dlist[[i]] <- CrossTab(rlist[[i]], Dt, long = FALSE) 
}

d <- rbinlist(dlist)

d[, sum(.SD), by = Dt]
