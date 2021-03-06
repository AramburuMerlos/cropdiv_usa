#  Number of years of each cell classified as a crop ------
library(raster)
library(snow)

Rpath <- "D:/cropdiv_usa/ReclData"
ntpath <- "D:/cropdiv_usa/nCellTemp"
dir.create(ntpath)
fn <- Sys.glob(file.path(Rpath,'*.tif'))
rl <- lapply(fn,raster)
s <- stack(rl)

beginCluster()
clusterR(s, fun = function(x) sum(!is.na(x)), 
         filename = file.path(ntpath, 'nCellTemp.tif'), 
         options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
         datatype="INT1U", NAflag = 0, overwrite = TRUE, progress = "text"
         )
endCluster()
