# calculate weighted 10y averages 
library(raster)
library(snow)

mysum <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = T)
mymean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = T)
no_mean <- function(x) if (any(grepl('Mean', x))) x[!grepl('Mean', x)] else x
f1 <- function(a,b) a/b
f2 <- function(x,w) log(x) * w
f3 <- function(x) if (all(is.na(x))) NA_real_ else exp(sum(x, na.rm = T))

# upload ncell (weight factor)
npath <- 'D:/cropdiv_usa/nCellSpatial'
nfn <- Sys.glob(file.path(npath, '*.tif'))
nfn <- no_mean(nfn)
nCells <- lapply(nfn, raster)
n_res_m <- (gsub(paste0(npath,'/n_'), '', sub('m_20...tif', '', nfn)))
n_res_km <- formatC(round(as.numeric(n_res_m) / 1000), 
                    width = 4, format = 'd', flag = '0')

# Gamma ----
gpath <- 'D:/cropdiv_usa/GammaDiversity'
gfn <- Sys.glob(file.path(gpath, '*.tif'))
gfn <- no_mean(gfn)

Dg <- lapply(gfn, raster)

# resolutions
res_char <- unique(gsub(paste0(gpath,'/Dg_'), '', gsub('_20...tif', '', gfn)))
resols <- as.numeric(gsub('m','',res_char))
Dg_in_res <- lapply(res_char, function(x) grep(x, gfn))
n_in_res <- lapply(res_char, function(x) grep(x, nfn))

# 10 y weighted average for each resolutions
# high resolutions are run in parallel
hr <- which(resols < 2000)
lr <- which(resols >= 2000)

beginCluster()
for (i in hr){
  gl <- Dg[Dg_in_res[[i]]]
  nl <- nCells[n_in_res[[i]]]
  ns <- stack(nl)
  nsum <- clusterR(ns, calc, args = list(fun = mysum), progress = 'text')
  wl <- lapply(nl, function(x) clusterR(stack(x, nsum), overlay, 
                                        args = list(fun = f1), 
                                        progress = 'text'))
  wh <- mapply(function(g,w) clusterR(stack(g,w), overlay,
                                      args = list(fun = f2)), 
               g = gl, w = wl)
  
  clusterR(stack(wh), calc, args = list(fun = f3), 
           filename = file.path(gpath, paste0('Dg_', res_char[i], '_Mean.tif')),
           options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
           NAflag = 0, overwrite = TRUE, progress = 'text')
  rm(gl, nl, ns, nsum, wl, wh)
}
endCluster()

for (i in lr){
  gl <- Dg[Dg_in_res[[i]]]
  nl <- nCells[n_in_res[[i]]]
  ns <- stack(nl)
  nsum <- calc(ns, fun = mysum)
  wl <- lapply(nl, function(x) overlay(stack(x, nsum), fun = f1))
  wh <- mapply(function(g,w) overlay(stack(g,w), fun = f2), g = gl, w = wl)
  calc(stack(wh), fun = f3, 
       filename = file.path(gpath, paste0('Dg_', res_char[i], '_Mean.tif')),
       options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
       NAflag = 0, overwrite = TRUE, progress = 'text')
  rm(gl, nl, ns, nsum, wl, wh)
}

# Alpha and Beta -----
apath <- 'D:/cropdiv_usa/AlphaDiversity'
bpath <- 'D:/cropdiv_usa/BetaDiversity'

afn <- Sys.glob(file.path(apath, '*.tif'))
afn <- no_mean(afn)
bfn <- Sys.glob(file.path(bpath, '*.tif'))
bfn <- no_mean(bfn)

Da <- lapply(afn, raster)
Db <- lapply(bfn, raster)

# resolutions of total area and sampling units
ta_su <- unique(sub(paste0(apath,'/Da_'), '', gsub('_20...tif', '', afn)))
a_ta_su <- lapply(ta_su, function(x) grep(x, afn))
b_ta_su <- lapply(ta_su, function(x) grep(x, bfn))
n_ta_su <- lapply(ta_su, function(x) grep((substr(x, 1, 4)), n_res_km)) 

# 10 y weighted average for each a_ta_su
for (i in 1:length(a_ta_su)){
  al <- Da[a_ta_su[[i]]]
  bl <- Db[b_ta_su[[i]]]
  nl <- nCells[n_ta_su[[i]]]
  ns <- stack(nl)
  nsum <- overlay(ns, fun = mysum)
  wl <- lapply(nl, function(x) overlay(x, nsum, fun = f1))
  # alpha
  wha <- mapply(function(a,w) overlay(a,w, fun = f2), a = al, w = wl)
  calc(stack(wha), fun = f3, 
       filename = file.path(apath, paste0('Da_', ta_su[i], '_Mean.tif')),
       options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
       NAflag = 0, overwrite = TRUE)
  # beta
  whb <- mapply(function(b,w) overlay(b,w, fun = f2), b = bl, w = wl)
  calc(stack(whb), fun = f3, 
       filename = file.path(bpath, paste0('Db_', ta_su[i], '_Mean.tif')),
       options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
       NAflag = 0, overwrite = TRUE)
  rm(al, bl, nl, ns, nsum, wl, wha, whb)
}

# nCellSpatial -----

n_res_m <- unique(n_res_m)
n_in_res_m = lapply(n_res_m, function(x) grep(x, nfn))

beginCluster()
for (i in 1:length(n_res_m)){
  ns = stack(nCells[n_in_res_m[[i]]])
  clusterR(ns, calc, args = list(fun = mymean),  
           filename = file.path(npath, paste0('n_', n_res_m[i], 'm_Mean.tif')), 
           options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
           NAflag = 0, overwrite = TRUE)
  rm(ns)
}
endCluster()
