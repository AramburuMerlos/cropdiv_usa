# Aggregate Temporal Diversity Estimations. 
# (by exp(mean of log(D)) or exp(mean(H))

library(raster) 
library(data.table)

source('functions/Aggr.R')
source('functions/BigAggrTmp.R')

Tpath <- "D:/TmpDivData"
Dt = raster(file.path(Tpath,"TmpDiv.tif"))

# aggregation factors and final resolutions
facts <- c(11 * 1:6, 33 * 2^(2:11))
(res_m = facts * 30)

ndig = max(nchar(res_m))
outnames = paste0('Dt_',
                  formatC(res_m, width = ndig, flag = 0, format = 'd'),
                  'm.tif')

# function to average Shannon indexes
mymean <- function(x) if (all(is.na(x))) NA_real_ else exp(mean(log(x),na.rm=T))

# factors to use with aggr (small factors) and bigaggr (big factors)
sf <- which(facts < 1000) # threshold should depend on RAM
bf <- which(facts >= 1000)

for (i in sf){
  aggr(Dt, fact = facts[i], fun = function(x, ...)mymean(x),
       filename = file.path(Tpath, outnames[i]),
       options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
       progress = 'text', overwrite = TRUE)
}

for (i in bf){
  BigAggrTmp(Dt, fact = facts[i], 
       filename = file.path(Tpath, outnames[i]),
       options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
       progress = 'text', overwrite = TRUE)
}

# Perennials Masked ------
PMpath <- "D:/PerennialMask"
Dt_pm <- raster(file.path(PMpath,"TmpDivPM.tif"))

facts_pm <- c(33 * 2^(0:2)) #only 1, 2 and 4km
(res_m = facts_pm * 30)
ndig = max(nchar(res_m))
outnames_pm = paste0('Dt_',
                     formatC(res_m, width = ndig, flag = 0, format = 'd'),
                     'm_PM.tif')

for (i in 1:length(facts_pm)){
  aggr(Dt_pm, fact = facts_pm[i], fun = function(x, ...)mymean(x),
       filename = file.path(PMpath, outnames_pm[i]),
       options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
       progress = 'text', overwrite = TRUE)
}
