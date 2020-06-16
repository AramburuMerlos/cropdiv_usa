# Run Temporal Diversity at each raster cell (30m)
library(raster)
library(Rcpp)
library(snow)

# upload C++ diversity function
source('functions/Dcpp.R')

# upload Double Crop Matrix
b <- read.csv("DblCropMatrix.csv")

# paths
TmpDivPath <- "D:/TmpDivData"
RcMDpath <-"D:/ReclMaskedData"

# reclassified and masked (6y) CDL
CDLfs <- Sys.glob(path = file.path(RcMDpath, '*.tif'))
CDLs <- stack(lapply(CDLfs, raster))

beginCluster()
clusterR(CDLs, calc, args = list(fun = Dcpp), export = 'b', 
         filename = file.path(TmpDivPath,"TmpDiv.tif"), 
         options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
         overwrite = TRUE, progress = 'text')
endCluster()
