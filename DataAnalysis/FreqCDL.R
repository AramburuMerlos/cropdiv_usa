library(raster)

CDLpath <- "D:/CDL"
CDLnames <- paste0(2008:2017, "_30m_cdls.img")
CDLs <- lapply(CDLnames, function(fn)raster(file.path(CDLpath, fn)))

outpath <- "Results/CDLfrequencies"
dir.create(outpath)
outnames <- paste0("CDLfreq", 2008:2017, ".csv")
# estimating frequencies (this takes up to ? hs)
for(i in 1:length(CDLs)){
  frq <- freq(CDLs[[i]], progress = "text")
  write.csv(frq, file.path(outpath, outnames), row.names = F)  
}

