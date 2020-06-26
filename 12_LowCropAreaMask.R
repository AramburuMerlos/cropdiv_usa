library(raster)

npath = 'D:/nCellSpatial'
nfn <- Sys.glob(file.path(npath, "*Mean.tif"))

ncell <- lapply(nfn, raster)
res_m <- gsub(paste0(npath,"/n_"), "", gsub("_Mean.tif", "", nfn))

th = read.csv('PorcAreaThreshold.csv', stringsAsFactors = FALSE)
# check if resolutions in th and ncell match
all.equal(res_m, th$res)

mpath <- 'D:/LowCropAreaMask'
dir.create(mpath)

mname <- paste0('lcam_', res_m, '.tif')

for(i in 1:length(ncell)){
  calc(ncell[[i]], fun = function(n) n > th$Threshold_n[i],
       filename = file.path(mpath, mname[i]), datatype = 'INT1U',
       options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
       progress = 'text', NAflag = 0, overwrite = TRUE)
}

