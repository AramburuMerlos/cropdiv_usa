library(data.table)
library(raster)

# gamma #####
gpath <- "D:/GammaDiversity"
gnames <- Sys.glob(file.path(gpath, "*Mean.tif"))
gres <- as.numeric(gsub(paste0(gpath, '/Dg_'), '', 
                        sub('m_Mean.tif', '', gnames)))
grasters <- lapply(gnames, raster) 
gvalues <- lapply(grasters, getValues)
gdts <- mapply(function(res, g) data.table(res_m = res, g = g), 
               res = gres, g = gvalues, SIMPLIFY = F)
gDT <- rbindlist(gdts)
rm(gdts, gvalues, grasters, gnames, gpath)

# nCell #####
npath <- 'D:/nCellSpatial'
nnames <- Sys.glob(file.path(npath, "*Mean.tif"))
nres <- as.numeric(gsub(paste0(npath, '/n_'), '', sub('m_Mean.tif', '', nnames)))
all.equal(nres, gres) # if FALSE stop and check that rasters match
nrasters <- lapply(nnames, raster) 
nvalues <- lapply(nrasters, getValues)
gDT[,n:= unlist(nvalues)]
rm(nvalues, nrasters, nnames, npath)

# ncell thresholds
th = fread('PorcAreaThreshold.csv')
th[,c("Threshold_p", "res") := NULL]
gDT <- merge(gDT, th, by.x = 'res_m', by.y = 'Resol_m')
gDT <- gDT[n > Threshold_n,]
gDT[, Threshold_n := NULL]
gDT[, n:= NULL]

# cumulative frequencies Dg #####
gDT[, cf:= ecdf(g)(g), by = res_m] 
setorderv(gDT, cols = c('res_m', 'cf'), order = 1L)
gDT <- unique(gDT)

# Cumulative Freq Dt #####
tDT <- fread("Results/CountyDt_freq.csv")
tDT <- tDT[, .(freq = sum(freq)), by = Dt]
setorder(tDT, Dt)
tDT[, cf:= cumsum(freq)/sum(freq)]
tDT <- rbind(data.table(Dt = 1, freq = 0, cf = 0),tDT)

# Cumulative Freq temporal richness
frich <- read.csv('Results/FreqTmpRich.csv')
frich <- frich[!is.na(frich$value),]
frich$p <- frich$count/sum(frich$count)
frich$cp <- cumsum(frich$p)
frich <- rbind(cbind(frich[,c('value', 'cp')]), 
               cbind('value' = frich$value+1,
                     'cp' = frich$cp)) 
frich <- frich[order(frich$cp),]
frich <- rbind(data.frame(value = 1, cp = 0), frich)
# resolutions to area in ha
garea <- round(gres^2 / 10000)

# select observational units sizes (ha) to plot
area.sel <- c(44, 174, 392, 1568, 25091)
gres <- gres[garea %in% area.sel]
blegend <- paste(area.sel, 'ha')

# some points 
gDT[which(g == 2 & res_m == 660)-1,]
gDT[floor(g) == 3 & res_m == 660 ,][1,]

gDT[floor(g) == 2 & res_m == 3960,][1,]
gDT[floor(g) == 2 & res_m == 15840,][1,]

##### PLOT #####
{ # run this line to save fig to folder
  set.seed(0)
  fig.file = "Plots/CumFreqs.pdf"
  #Delete file if it exists
  if(file.exists(fig.file)) file.remove(fig.file)
  pdf(file = fig.file, width = 11.4/2.54, height = 5.7/2.54, pointsize = 8)
  
  { #run this line to get plots in R 
    par(mfrow = c(1,2), mgp = c(0,0.8,0), las = 1, 
        mar = c(3,3,0.5,0.5), pty = 's')
    xr = c(0,8)
    yr = c(0,1)
    cols = viridis::plasma(length(gres), begin = 0.05, end = 0.95, direction = -1)
    for(p in 1:2){
      ylab = expression('Cumulative Relative Frequency')
      plot(1, axes = FALSE, type = "n", xlim = xr, ylim = yr, ylab = "", xlab = "")
      clip(xr[1], xr[2], yr[1], yr[2])
      abline(h = seq(yr[1], yr[2], length.out = 11)[2:10], col = 'grey80', lty = 3)
      abline(v = seq(xr[1], xr[2], length.out = 9)[2:8], col = 'grey80', lty = 3)
      # gamma
      if(p==2){
        xlab = bquote(italic('D')*gamma*"") 
        for (i in 1:length(gres)){
          # but first, reducing data size
          nr = nrow(gDT[res_m == gres[i],])
          if(nr > 5000){
            sr = sample(1:nr, size = 5000)
            sr = sr[order(sr)]
            sdf = gDT[res_m == gres[i],][sr,]  
          } else {
            sdf = gDT[res_m == gres[i],]
          }
          lines(sdf$g, sdf$cf, lwd = 1.5, col = cols[i])
        }
        # legend 
        rect(xr[2]*.57, 0, xr[2], 0.4, col = 'white')
        legend(x = c(xr[2]*.57, xr[2]), y = c(0, 0.4), 
               legend = sapply(blegend, as.expression),
               y.intersp = 1, x.intersp = 1, cex = 0.8, 
               col = cols, bty = 'n', lwd = 2)
      } else {
        xlab = bquote(italic('D')*tau*"")
        lines(tDT$Dt, tDT$cf, lwd = 2)
        lines(frich$value, frich$cp, col = 'gray10', lwd = 2, lty = 3)
        rect(xr[2]*.555, 0, xr[2], 0.18, col = 'white')
        legend(x = c(xr[2]*.555, xr[2]), y = c(0, 0.18), 
               legend = c(expression(italic("Diversity")),
                          expression(italic('Richness'))),
               y.intersp = 1, x.intersp = 1, cex = 0.8, lty = c(1,2), 
               col = c('black', 'gray10'), bty = 'n', lwd = 1.3)
      }
      mtext(text = xlab, side = 1, line = 1.3, adj = 0.5)
      axis(side = 1, at = pretty(xr), pos = yr[1], lwd = 1, cex.axis = 0.9)
      axis(side = 2, at = pretty(yr), pos = xr[1], lwd =1, las = 1, cex.axis = 0.9)
      axis(side = 3, at = pretty(xr), pos = yr[2], lwd = 1, lwd.ticks = 0, labels = F)
      axis(side = 4, at = pretty(yr), pos = xr[2], lwd = 1, lwd.ticks = 0, labels = F)
      mtext(LETTERS[p], line = -1.8, adj = 0.06, cex = 1.3)
      if(p==1) mtext(text = ylab, side = 2, line = 1.5, las = 0)
    }
  }
  dev.off()
}
