# mask cells with less than 6 years of agriculture ----------
library(raster)

Rpath <- "D:/cropdiv_usa/ReclData"
rnames <- Sys.glob(file.path(Rpath,'*.tif'))
rlist <- lapply(rnames, FUN = raster)

ncellpath <- "D:/cropdiv_usa/nCellTemp"
ncell <- raster(file.path(ncellpath, "nCellTemp.tif"))

# function to mask cells with less than 6 years classified as crop
f <- function(x,y){
  x[y<6]<- NA
  return(x)
}

# output file names and folder
mname <- paste0("CDL", 2008:2017, 'RCM.tif')
mpath <- "D:/cropdiv_usa/ReclMaskedData/"
dir.create(mpath)

# run in parallel
beginCluster()
for(i in 1:length(mname)){
  s <- stack(rlist[[i]],ncell)
  clusterR(s, overlay, args = list(fun = f), 
           filename = file.path(mpath, mname[i]),
           options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
           datatype="INT1U", NAflag = 0, overwrite = TRUE, progress = "text"
           )
}
endCluster()