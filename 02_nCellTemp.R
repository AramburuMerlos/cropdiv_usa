#  Number of years of each cell classified as a crop ------
library(raster)
library(snow)

Rpath <- "D:/ReclData"
fn <- Sys.glob(file.path(Rpath,'*.tif'))
rl <- lapply(fn,raster)
s <- stack(rl)

beginCluster()
clusterR(s, fun = function(x) sum(!is.na(x)), 
         filename = 'D:/nCellTemp/nCellTemp.tif', 
         options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
         datatype="INT1U", NAflag = 0, overwrite = TRUE, progress = "text"
         )
endCluster()
