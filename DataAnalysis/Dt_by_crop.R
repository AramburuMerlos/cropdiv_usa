library(raster)
library(data.table)
source("Functions/CrossTab.R")

# Dt freqs by crop #########
Dt <- raster("D:/TmpDivData/TmpDiv.tif")
rnames <- paste0("D:/ReclMaskedData/CDL", 
                 2008:2017, 
                 "RCM.tif")
rlist <- lapply(rnames, raster)
nr <- length(rlist)
dlist <- vector(mode = "list", length = nr)

for(i in 1:nr){
  dlist[[i]] <- CrossTab(rlist[[i]], Dt) 
}

# sum all years
d <- rbindlist(dlist)
setnames(d, "rlist[[i]]", "ID")
d <- d[, .(freq = sum(freq, na.rm = TRUE)), by = .(ID, Dt)]
d <- d[complete.cases(d),]

# distribute/reclassify double crops
d_dc <- d[ID >200,]
d_dc <- d_dc[, ID:= m[match(ID,m[,1]), 3]]
d[, ID:= ifelse(ID > 200, m[match(ID,m[,1]), 2], ID)]
d <- rbind(d, d_dc)
d <-  d[, .(freq = sum(freq)), by = .(ID, Dt)]
# save results
fwrite(d, "Results/Dt_by_crop.csv")


# table to be published ##########
d <- fread("Results/Dt_by_crop.csv")
brks <- c(0,1,2,3,4,5,6,11)
d[,bin:= cut.default(Dt, breaks = brks)]
d <- d[, .(freq = sum(freq)), by = .(ID, bin)]

cnames <- fread("cropnames.csv")
d <- merge(d, cnames, by = 'ID')
d <- d[A_N == 'A',]
d[,c("ID","A_N"):= NULL]

d[, perc:= round(freq/sum(freq),2)*100, by = Name]
dw <- dcast(d, Name ~ bin, value.var = 'perc')
darea <- d[, .(area_ha = sum(freq) * 900 / 10000 / 10), by = Name]
darea[, Area:= ifelse(area_ha > 1e6, paste0(round(area_ha/1e6,1), "M"),
                      ifelse(area_ha > 1e3, 
                             paste0(round(area_ha/1e3,1), "k"),
                             round(area_ha)))]
dw <- merge(dw, darea)
setorderv(dw, "area_ha", order = -1L)
setnames(dw, c("Name", "(0,1]"), c("Crop", "= 1"))
dw[, area_ha:= NULL]
fwrite(dw, "Results/Dt_by_Crop_Table.csv")
