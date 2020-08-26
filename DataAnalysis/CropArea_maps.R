library(raster)
library(sf)
library(data.table)

# Crop Frequencies ####################################

rnames <- Sys.glob("D:/ReclMaskedData/CDL*RCM.tif")
rlist <- lapply(rnames, raster)
ny <- length(rlist)
dtlist <- vector(mode = 'list', length = ny)

source("Functions/BigAggr.R")
fact <- (3960 / 30) # ~ 4 km resolution

# crop frequencies for each cell
for(i in 1:ny){
  dtlist[[i]] <- BigAggr(x = rlist[[i]], fact = fact, returnRaster = (i == 1))
}

# empty raster
r <- dtlist[[1]][[1]]

dtlist[[1]] <- dtlist[[1]][[2]]

# combine and sum each year freqs
DT <- rbindlist(dtlist)
rm(dtlist)
DT <- DT[, .(N = sum(N, na.rm = T)), by = .(xval, outcell)]

# rellocate double crops
m <- read.csv("DblCropMatrix.csv")
dc_dt <- function(DT, m){
  DT_dc <- DT[xval >200,]
  DT_dc <- DT_dc[, xval:= m[match(xval,m[,1]), 3]]
  DT[, xval:= ifelse(xval > 200, m[match(xval, m[,1]), 2], xval)]
  DT_ndc <- rbind(DT, DT_dc)
  DT_out <- DT_ndc[, .(N = sum(N)), by = .(outcell, xval)]
  return(DT_out)
}
DT <- dc_dt(DT, m)

# crop proportion of total crop area per outcell
DT[, prop:= N/sum(N), by = .(outcell)]
setnames(DT, old = 'xval', new = 'crop')
# remove cells without crops (NA)
DT <- DT[!is.na(crop), ]

# save/read frequencies
fwrite(DT, "Results/CropProp_Res3960m.csv")



# Map ################################################# 

DT <- fread("Results/CropProp_Res3960m.csv")

# empty raster
r <- raster(nrows = 732, 
            ncols = 1166, 
            crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs", 
            ext = extent(c(-2356095, 2261265, 273885, 3172605)), 
            resolution = 3960, 
            vals = NULL)
# crops to plot
crop_names <- fread("cropnames.csv")
DT_Ntot <- DT[, .(Ntot = sum(N)), by = .(crop)]
DT_Ntot <- merge(DT_Ntot, crop_names, by.x = "crop", by.y = "ID")
setorderv(DT_Ntot, "Ntot", -1)
crops <- DT_Ntot[1:8,]


# polygon with states 
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
states <- st_transform(states, crs(r))
states <- as_Spatial(states)

# crop area mask
m3960 <- raster("D:/LowCropAreaMask/lcam_0003960m.tif")

# Figure with 8 maps 
{
  tiff(filename = "Plots/CropAreaMaps.tif", width = 6, height = 7.77, units = 'in', 
       type = "cairo", res = 900, compression = "zip")
  {
    par(mfrow = c(4, 2) , mar = rep(0,4))
    cols <- RColorBrewer::brewer.pal(8, 'Greens')
    for (i in 1:nrow(crops)){
      vali <- rep(NA, ncell(r))
      DTi <- DT[crop == crops$crop[i]]
      vali[DTi$outcell] <- DTi$prop
      ri <- setValues(r, vali)
      ri <- mask(ri, m3960)
      image(ri, axes = FALSE, col = cols, xlab = "", ylab = "", maxpixels = ncell(ri))
      plot(states, lwd =1.5, add = T)
      mtext(crops$Name[i], cex = 1, adj = 0.9, side = 3 , line = -2)
    }
    par(new = T, fig = c(0.42, 0.62, 0.01, 0.06), xpd = T, mgp = c(3, 0, 0))
    l.args = list(text = 'Crop Area \n(% of Cropland)', side = 3, line = 0.5, cex = 0.7, adj = 0.2)
    plot(ri, legend.only = TRUE, legend.width = 1, axis.args = list(cex.axis = .9, line = 0), 
         legend.shrink= 1, col = cols, legend.args = l.args, 
         horizontal = T, smallplot = c(0,1,0.5,.8))
  }
  dev.off()  
}  

