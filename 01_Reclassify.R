# CDL reclassification -------
# fuse same species classes, set all double crops >200, and non-ag classes to 0
library(raster)
library(snow)

# reclassificaiton matrix
t <- read.csv("CropCode.csv")
m <- cbind(t$VALUE, t$ID) 

# paths and file names
CDLpath <- "D:/CDL"
Rpath <- "D:/cropdiv_usa/ReclData"
dir.create(Rpath)

CDLnames <- paste0(2008:2017, "_30m_cdls.img")
outnames <- paste0("CDL", 2008:2017, "RC.tif")

# run in parallel
beginCluster()
for(i in 1:length(CDLnames)){
  r <- raster(file.path(CDLpath, CDLnames[i]))
  clusterR(r, reclassify, args = list(rcl = m), 
           filename = file.path(Rpath,outnames[i]),
           options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
           datatype="INT1U", NAflag = 0, overwrite = TRUE, progress = "text")
}
endCluster()

