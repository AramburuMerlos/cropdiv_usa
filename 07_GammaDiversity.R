library(raster)
library(data.table)
library(Rcpp)

Rpath <- "D:/cropdiv_usa/ReclMaskedData"
rnames <- Sys.glob(file.path(Rpath, '*.tif'))
rl <- lapply(rnames, raster)
names(rl) <- rnames

source("functions/BigAggrDiv.R")
source("functions/Dcpp.R")
source("functions/Aggr.R")

m <- read.csv('DblCropMatrix.csv')
m <- unname(as.matrix(m))

gammapath <- 'D:/cropdiv_usa/GammaDiversity'
dir.create(gammapath)

# aggregation factors and final resolutions
facts <- c(11 * 1:6, 33 * 2^(2:11))
(res_m <- facts * 30)

# output names
ndig <- max(nchar(res_m))
res_m <- paste0(formatC(res_m, 
                        width = ndig, 
                        flag = 0, 
                        format = 'd'), 'm_')
outnames <- expand.grid('Dg_', 2008:2017, res_m, ".tif", 
                       stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
dc_args <- c(outnames[c(1,3,2,4)], sep="")
outnames <- do.call(paste, dc_args)

# factors to use with aggr (small factors) and bigaggr (big factors)
sf <- which(facts < 1000) # threshold should depend on RAM
bf <- which(facts >= 1000)

# Small Factors ----
# loop for each year inside loop for each aggregation factor
for (f in sf){   
  for (i in 1:10){
    aggr(rl[[i]], fact = facts[f], fun = function(x, ...)Dcpp(v = x, b = m),
         filename = file.path(gammapath, outnames[(f-1)*10+i]), 
         progress = 'text', NAflag = 0, overwrite = TRUE)
  }
}

# Big Factors ---- 
# loop for each year inside loop for each aggregation factor
for (f in bf){   
  for (i in 1:10){
    BigAggrDiv(rl[[i]], fact = facts[f], m = m,
         filename = file.path(gammapath, outnames[(f-1)*10+i]),
         options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
         progress = 'text', NAflag = 0, overwrite = TRUE)
  }
}
