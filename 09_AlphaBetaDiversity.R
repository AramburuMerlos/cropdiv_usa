library(raster)

source('functions/AggrAlpha.R')

# resolutions (as character with equal width)
facts <- c(11 * 1:6, 33 * 2^(2:11))
res_m <- facts * 30
ndig <- max(nchar(res_m))
res_m <- paste0(formatC(res_m, 
                        width = ndig, 
                        flag = 0, 
                        format = 'd'), 'm')

# Dg raster
# gamma diversity rasters listed by resolution
gpath <- 'D:/cropdiv_usa/GammaDiversity'
gfn <- lapply(res_m, function(res) Sys.glob(file.path(gpath, 
                                                      paste0('*',res,'*.tif'))))

# remove the mean raster, if there is one
no_mean <- function(x) if(any(grepl('Mean', x))) x = x[!grepl('Mean', x)] else x
gfn <- lapply(gfn, no_mean)
gdr <- lapply(gfn, function(x) lapply(x, raster))

# ncell rasters
npath <- 'D:/cropdiv_usa/nCellSpatial'

nfn <- lapply(res_m, function(res) Sys.glob(file.path(npath, 
                                                    paste0('*',res,'*.tif'))))
nfn <- lapply(nfn, no_mean)
nr <- lapply(nfn, function(x) lapply(x, raster))

resol <- as.numeric(gsub('m','',res_m))
# sampling units resolutions index
sur <- which(resol <= 3960)
# total area resolution index
tar <- which(resol >= 3960)
## the 3.96 km is in both sampling units and total area resolutions

# file names
res_su_m <- gsub('000','',res_m[sur])
resol_tar_km <- round(resol[tar] / 1000)
ndig_tar <- max(nchar(as.character(resol_tar_km)))
res_tar_km <- paste0(formatC(resol_tar_km, 
                             width = ndig_tar, 
                             flag = 0, 
                             format = 'd'), 'km')
afn_comp <- expand.grid('Da', 2008:2017, res_su_m, res_tar_km, '.tif', 
                        stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
dc_args <- c(afn_comp[c(1,4,3,2)], sep="_")
afn_inc <- do.call(paste, dc_args)
afn <- paste0(afn_inc, '.tif')
bfn <- gsub("Da", "Db", afn)

apath <- 'D:/cropdiv_usa/AlphaDiversity'
dir.create(apath)

bpath <- 'D:/cropdiv_usa/BetaDiversity'
dir.create(bpath)

# loop for total area resolution over
# loop for sampling unit resolution over
# loop for each year
for(ta in tar){
  for(su in sur){
    for(y in 1:10){
      # sampling unit resol must be greater than total area resol
      # and sampling units cells must perfectly fit within total area cells
      if(resol[ta] > resol[su] & resol[ta] %% resol[su] == 0){
        i <- (ta - min(tar)) * length(sur) * 10 +  (su - 1) * 10 + y
        # Alpha Diversity
        Da <- aggrAlpha(sud = gdr[[su]][[y]],  # sampling units Dg raster
                        sun  = nr[[su]][[y]],  # sampling units nCell raster
                        totn = nr[[ta]][[y]],  # total area nCell raster 
                        filename = file.path(apath, afn[i]), 
                        options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
                        NAflag = 0, overwrite = TRUE)  
        # Beta Diversity
        overlay(gdr[[ta]][[y]], Da, 
                fun = function(x,y) x/y, 
                filename = file.path(bpath, bfn[i]),
                options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
                progress = 'text', NAflag = 0, overwrite = TRUE)
        print(paste(i, "/", length(tar) * length(sur) * 10))
      }
    }
  }
}
