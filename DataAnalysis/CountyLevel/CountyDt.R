library(raster)
library(data.table)

source(file.path('Functions/CrossTab.R'))

county <- raster('D:/USrasterized/CountiesGEOID_30m.tif')
Dt <- raster("D:/TmpDivData/TmpDiv.tif")

# Dt frequencies by County
d <- CrossTab(county, Dt)

fwrite(d, "Results/CountyDt_freq.csv")

