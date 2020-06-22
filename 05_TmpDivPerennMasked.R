library(raster)
library(snow)

Rpath <-"D:/ReclMaskedData"
Tpath <- "D:/TmpDivData"
PMpath <- "D:/PerennialMask"
dir.create(PMpath)

# Create mask ----

CDLfs <- Sys.glob(path = file.path(Rpath, '*.tif'))
CDLs <- stack(lapply(CDLfs, raster))

# perennials crops (to mask)
cropcode <- read.csv('CropCode.csv')
pc <- cropcode$ID[cropcode$A_N == "P" | cropcode$A_N == "M"]
pc <- unique(pc)

f <- function(x){
  if(all(is.na(x))) return(NA)
  sum(x %in% pc, na.rm = T) >= 4
} 

beginCluster(n = 3)
clusterR(CDLs, calc, args = list(fun = f), export = 'pc', 
         filename = file.path(PMpath, 'PerennialMask.tif'), 
         options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
         datatype = 'INT1U', overwrite = TRUE, progress = 'text')
endCluster()

pmask <- raster(file.path(PMpath, 'PerennialMask.tif'))


# Mask TmpDiv by perennials ----
tmp = raster(file.path(Tpath, "TmpDiv.tif"))
mask(tmp, pmask, maskvalue = T, 
     filename = file.path(PMpath,"TmpDivPM.tif"), 
     options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
     overwrite = TRUE, progress = 'text')
