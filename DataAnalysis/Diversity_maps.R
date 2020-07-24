library(maps)
library(sp)
library(raster)
library(sf)
library(colorRamps)

# Temporal Diversity ########

# Dt at 3960 m resolution 
Dt <- raster('D:/TmpDivData/Dt_0003960m.tif')
lcam <- raster('D:/LowCropAreaMask/lcam_0003960m.tif')
Dt_m <- mask(Dt, lcam)

# limit all values up to 6.5 (for scale bar)
m6 <- Dt_m > 6.5
Dt_m <- mask(Dt_m, m6, maskvalue = TRUE, updatevalue = 6.5)

# Dt with pereniials masked 
DtPM <- raster('D:/PerennialMask/Dt_3960m_PM.tif')
DtPM_m <- mask(DtPM, lcam)

m6 <- DtPM_m > 6.5
DtPM_m <- mask(DtPM_m, m6, maskvalue = TRUE, updatevalue = 6.5)

maiw = (11.4/2.54 - ncol(Dt_m)/300)/2

# polygon with states 
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states <- st_transform(states, crs(Dt_m))
states <- as_Spatial(states)

{ # run this line to save plot 
  fgfn = ('Plots/Dt_maps.tif')
  tiff(filename = fgfn, width = (11.4/2.54*300), height = nrow(Dt_m)*2, 
       type = "cairo", res = 300, compression = "zip")
  {
    par(mai = c(0,maiw,0,maiw), mfrow = c(2,1))
    brks = c(1,seq(1.1,3.5,0.2),seq(3.7,5.7,0.5),6.5)
    cols = rev(matlab.like2(length(brks)-1))
    a.args = list(at = 1:6, labels = c(as.character(1:5),'\u2265 6'), 
                  cex.axis = 0.65, line = -1, lty = 0)
    l.args = list(text = expression(paste(italic('D'),tau)), side = 3, 
                  font = 1, line = 0.2, cex = 1, adj = 0.5)
    image(Dt_m, axes = FALSE, breaks = brks, col = cols, xlab = "", ylab = "", 
          maxpixels = ncell(Dt_m))
    mtext('A',line = -2, adj = 0.85, cex = 1)
    plot(states, lwd = 0.5, add = T)
    image(DtPM_m, axes = FALSE, breaks = brks, col = cols, xlab = "", ylab = "", 
          maxpixels = ncell(DtPM_m))
    mtext('B',line = -2, adj = 0.85, cex = 1)
    plot(states, lwd = 0.5, add = T)
    par(new = T, fig = c(0.02,0.38,0.545,0.562))
    plot(DtPM_m, legend.only = TRUE, legend.width = 0.8, axis.args = a.args, 
         legend.shrink= 1, col = cols, breaks = brks, legend.args = l.args, 
         horizontal = T, smallplot = c(0,1,0,1))
  }
  dev.off()
}



# SPATIAL DIVERSITY ########

# 4 pannels 660, 1320, 3960, 15840m 
afn <- paste0('D:/AlphaDiversity/Da_0016km_',
              formatC(c(660, 1320, 3960), width = 4, flag = 0)
              , 'm_Mean.tif')
gfn <- paste0('D:/GammaDiversity/Dg_0015840m_Mean.tif')
fn <- c(afn, gfn)
Dsp <- lapply(fn, raster)

lcam16k <- raster('D:/LowCropAreaMask/lcam_0015840m.tif')
Dsp_m <- lapply(Dsp, mask, mask = lcam16k)

# limit all values up to 6.5 (for scale bar)
Dsp_m <- lapply(Dsp_m, function(r){
  m6 <- r > 6.5
  mask(r, m6, maskvalue = TRUE, updatevalue = 6.5)
})

# aspect.ratio (width/height)
ar = ncol(Dsp_m[[1]])/nrow(Dsp_m[[1]])

{
  fgfn = ('Plots/Dspatial_maps.tif')
  tiff(filename = fgfn, width = 17.8, height = (17.8-1.27)/ar, unit = 'cm', 
       type = "cairo", res = 300, compression = "zip")
  {
    par(mfrow = c(2,2), mar = c(0,0,0,0), omi = c(0,0,0,0.5), xpd =T)
    for(i in 1:length(Dsp_m)){
      brks = c(1,seq(1.1,3.5,0.2),seq(3.7,5.7,0.5),6.5)
      cols = rev(matlab.like2(length(brks)-1))
      image(Dsp_m[[i]], axes = FALSE, breaks = brks, col = cols, 
            xlab = "", ylab = "", maxpixels = ncell(Dsp_m[[i]]))
      plot(states, lwd =0.5, add = T)
      mtext(LETTERS[i], cex = 0.9, line = -1.5, adj = 0.9)
    }
    par(new = T, fig = c(0.985,1, 0.481,0.7), xpd = T)
    a.args = list(at = 1:6, labels = c(as.character(1:5),'\u2265 6'), 
                  cex.axis = 0.8, line = 0, lty = 1)
    l.args = list(text = expression(paste(italic('D'),gamma,", ",italic('D'),alpha)),
                  side = 3, adj = 0.2, line = 0.2, cex = 0.65)
    plot(Dsp_m[[2]], legend.only = TRUE, axis.args = a.args, legend.args = l.args,
         col = cols, breaks = brks, smallplot = c(0,1,0,1))
    
    
  }
  dev.off()
}


# Alpha and Beta Diversity ###### 
fn <-  paste0(rep(c('D:/AlphaDiversity/Da_', 'D:/BetaDiversity/Db_'), each = 2), 
              rep(c("0016km", "0063km"),2), 
              '_1980m_Mean.tif')
DaDb <- lapply(fn, raster)
masks <- lapply(c("D:/LowCropAreaMask/lcam_0015840m.tif",
                  "D:/LowCropAreaMask/lcam_0063360m.tif"),
                raster)
DaDb <- mapply(function(d,m) mask(d,m), d = DaDb, m = masks)

# limit all values up to 6.5 (for scale bar)
DaDb <- lapply(DaDb, function(r){
  m6 <- r > 6.5 
  out <- mask(r, m6, maskvalue = TRUE, updatevalue = 6.5)
  return(out)
})

titles <- list(expression(paste(italic('D'),alpha)),
               expression(paste(italic('D'),alpha)),
               expression(paste(italic('D'),beta)), 
               expression(paste(italic('D'),beta)))
subtitles <- list(bquote(25091 ~ ha ~ "| 392 ha"),
                  bquote(401449 ~ ha ~ "| 392 ha"),
                  bquote(25091 ~ ha ~ "| 392 ha"),
                  bquote(401449 ~ ha ~ "| 392 ha"))

{
  fgfn = ('Plots/Da_Db_maps.tif')
  tiff(filename = fgfn, width = ncol(DaDb[[1]])*32, height = nrow(DaDb[[1]])*32,
       type = "cairo", res = 300, compression = "zip")
  {
    par(mfrow = c(2,2), mar = c(0,0,6,12))
    for(i in 1:length(DaDb)){
      brks = c(1,seq(1.1,3.5,0.2),seq(3.7,5.7,0.5),6.5)
      cols = rev(matlab.like2(length(brks)-1))
      image(DaDb[[i]], axes = FALSE, breaks = brks, col = cols, 
            xlab = "", ylab = "", maxpixels = ncell(DaDb[[i]]))
      plot(states, lwd =1.5, add = T)  
      mtext(titles[[i]], cex = 3, line = -1.9, adj = 0.68 + (i %% 2)/100)
      mtext(subtitles[[i]], cex = 2, line = -2.4, adj = 0.9)
      if(i == 1){
        a.args = list(at = 1:6, labels = c(as.character(1:5),'\u2265 6'),
                      cex.axis = 3, line = 1)
        l.args = list(text = 'Diversity', side = 3, adj = 0.3, line = 1, cex = 2.5)
        plot(DaDb[[2]], legend.only = TRUE, axis.args = a.args, legend.args = l.args,
             col = cols, breaks = brks, smallplot = c(0.9,.93,0,0.45))
      }  
    }
  }
  dev.off()
}

