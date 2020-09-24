# Run Temporal Diversity at each raster cell (30m)
library(raster)
library(snow)

# paths
TmpDivPath <- "D:/cropdiv_usa/TmpDivData"
dir.create(TmpDivPath)
RcMDpath <-"D:/cropdiv_usa/ReclMaskedData"

# reclassified and masked (6y) CDL
CDLfs <- Sys.glob(path = file.path(RcMDpath, '*.tif'))
CDLs <- stack(lapply(CDLfs, raster))

# Double Crop Matrix
m <- read.csv("DblCropMatrix.csv")

# Temporal Richness -----
rich <- function(x){
  if(all(is.na(x))) return(NA)
  x <- x[!is.na(x)]
  if(max(x)>200){
    dci <- match(x,m[,1])
    dci <- dci[!is.na(dci)]
    x <- c(x[x < 200], m$First[dci], m$Second[dci])
  } 
  return(length(unique(x)))
}

beginCluster()
clusterR(CDLs, calc, args = list(fun = rich), export = 'm',
         filename = file.path(TmpDivPath,"TmpRich.tif"), 
         options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
         datatype = "INT2U", overwrite = TRUE, progress = 'text')
endCluster()


# Temporal Diversity -----
div <- function(x){
  if(all(is.na(x))) return(NA)
  x <- x[!is.na(x)]
  if(max(x)>200){
    dci <- match(x,m[,1])
    dci <- dci[!is.na(dci)]
    x <- c(x[x < 200], m$First[dci], m$Second[dci])
  } 
  v <- tapply(x,x,length)  
  pi <- v/sum(v)
  return(exp(-sum(pi*log(pi))))
}

beginCluster()
clusterR(CDLs, calc, args = list(fun = div), export = 'm',
         filename = file.path(TmpDivPath,"TmpDiv.tif"), 
         options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
         overwrite = TRUE, progress = 'text')
endCluster()
