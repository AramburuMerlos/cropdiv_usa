library(raster)
library(data.table)

# gamma #####
gpath <- "D:/GammaDiversity"
gnames <- Sys.glob(file.path(gpath, "*Mean.tif"))
gres <- as.numeric(gsub(paste0(gpath, '/Dg_'), '', 
                        sub('m_Mean.tif', '', gnames)))
grasters <- lapply(gnames, raster) 
gvalues <- lapply(grasters, getValues)
gdts <- mapply(function(res, g) data.table(res_m = res, g = g), 
               res = gres, g = gvalues, SIMPLIFY = F)
DT <- rbindlist(gdts)
DT[,res_km:= round(res_m/1000)]
DT[, cellN:= seq_len(.N), by = res_m]
rm(gdts, gvalues, grasters, gnames, gpath)

# temporal #####
tpath <- "D:/TmpDivData"
tnames <- Sys.glob(file.path(tpath, "*Dt*.tif"))
tres <- as.numeric(gsub(paste0(tpath, '/Dt_'), '', sub('m.tif', '', tnames)))
all.equal(tres, gres) # if FALSE stop and check that rasters match
trasters <- lapply(tnames, raster) 
tvalues <- lapply(trasters, getValues)
DT[,t:= unlist(tvalues)]
rm(tvalues, trasters, tnames, tpath, tres)

# nCell #####
npath <- 'D:/nCellSpatial'
nnames <- Sys.glob(file.path(npath, "*Mean.tif"))
nres <- as.numeric(gsub(paste0(npath, '/n_'), '', sub('m_Mean.tif', '', nnames)))
all.equal(nres, gres) # if FALSE stop and check that rasters match
nrasters <- lapply(nnames, raster) 
nvalues <- lapply(nrasters, getValues)
DT[,n:= unlist(nvalues)]
rm(nvalues, nrasters, nnames, npath, nres)

# alpha SU = 1980 ##########
apath <- "D:/AlphaDiversity"
anames <- Sys.glob(file.path(apath, "*1980m_Mean.tif"))
ares <- as.numeric(gsub(paste0(apath, '/Da_'), '', 
                        sub('km_1980m_Mean.tif', '', anames)))
arasters <- lapply(anames, raster) 
avalues <- lapply(arasters, getValues)
adts <- mapply(function(res, a) data.table(res_km = res, a = a), 
               res = ares, a = avalues, SIMPLIFY = F)
DTab <- rbindlist(adts)
DTab[, cellN:= seq_len(.N), by = res_km]
rm(adts, avalues, arasters, anames, apath)

# beta SU = 1980 ############
bpath <- 'D:/BetaDiversity'
bnames <- Sys.glob(file.path(bpath, "*1980m_Mean.tif"))
bres <- as.numeric(gsub(paste0(bpath, '/Db_'), '', sub('km_1980m_Mean.tif', '', bnames)))
all.equal(ares, bres) # if FALSE stop and check that rasters match
brasters <- lapply(bnames, raster) 
bvalues <- lapply(brasters, getValues)
DTab[,b:= unlist(bvalues)]
rm(bvalues, brasters, bnames, bpath, bres)

# join DT with DTab
DT <- DTab[DT, on = .NATURAL]

rm(DTab)
DT <- DT[!is.na(g),]
DT[, cellN:= NULL]

# ncell thresholds
th = fread('PorcAreaThreshold.csv')
th[,c("Threshold_p", "res") := NULL]
DT <- merge(DT, th, by.x = 'res_m', by.y = 'Resol_m')
DT <- DT[n > Threshold_n,]
DT[, Threshold_n := NULL]

# ncell weights 
DT[, w:= n/sum(n), by = res_m]

# calculate weighted avg Diversities per resol
myavg <- function(D, w) exp(sum(log(D) * w))
cols <- c("g", "t", "a", "b")
DTavg <- DT[, lapply(.SD, function(x) myavg(x, w)), .SDcols = cols, by = res_m]

# regression for county fix #####
d <- DTavg[res_m > 1000,]
d <- d[res_m < 1000000,]
d[,lr:= log10(res_m)]
mod <- lm(g ~ poly(lr, 3), data = d)
plot(d$g ~ d$lr)
lines(d$lr, predict(mod))
saveRDS(mod, "Results/lm_Dg_f(log10(res_m)).RDS")

# temporal diversity of USA #####
Dt_freq <- fread("Results/CountyDt_freq.csv")
Dt_freq <- Dt_freq[, .(freq = sum(freq)), by = Dt]
Dt_freq[, w:= freq/sum(freq)]
usa_Dt <- Dt_freq[, exp(sum(log(Dt) * w))]

# where Dg intersects Dt (approx) in ha ######
approx(x = DTavg$g, y = DTavg$res_m, xout = usa_Dt)$y^2/10000 
 
# add 30m point ######
avg_30m <- data.table(res_m = 30, 
                      g = 1.020469, # isn't 1 because of double crops. 
                      t = usa_Dt) 
DTavg <- rbind(avg_30m, DTavg, fill = TRUE)

# g ~ t dissimilarity stats (MSD and components) ######
MSD <- DT[, .(MSD = sum((g - t)^2 * w), 
              SB = (sum(g * w) - sum(t * w))^2), 
          by = res_m]

# components of MSD after Kobayasi_Salam 2000 (considering weights)
MSDc <- DT[, .(SDt = sqrt(sum((t - sum(t * w))^2 * w)), 
              SDg = sqrt(sum((g - sum(g * w))^2 * w)), 
              rnum = sum((g - sum(g * w))*(t - sum(t * w)) * w)), 
          by = res_m]
MSDc[, SDSD:= (SDt - SDg)^2]
MSDc[, r:= rnum/(SDt * SDg)]
MSDc[, LCS:= 2 * SDt * SDg * (1-r)]
MSDc[, c('SDt', 'SDg', 'rnum') := NULL]

MSD <- merge(MSD, MSDc)

# add 30m point to MSD components
MSD_30m <- data.table(res = 0.03, 
                      MSD = Dt_freq[, sum((Dt - avg_30m$g)^2 * w)], 
                      SB = (avg_30m$g - usa_Dt)^2, 
                      SDt = Dt_freq[, sqrt(sum((Dt - usa_Dt)^2 * w))], 
                      SDg = 0.01148) # isn't 0 because of double crops.
MSD_30m[, SDSD:= (SDt - SDg)^2]
MSD_30m[, LCS:= MSD - SDSD - SB] # estimated by difference because cannot compute correlation
MSD_30m[, c('SDt', 'SDg'):=NULL]

MSD <- rbind(MSD_30m, MSD, fill = T)
MSD[,(SDSD + LCS + SB)-MSD] # difference because of rounding problems

# complete plot ################

{ # run this line to save fig to folder
  fig.file = "Plots/scale_dependency.pdf"
  #Delete file if it exists
  if(file.exists(fig.file)) file.remove(fig.file)
  pdf(file = fig.file, width = 11.4/2.54, height = 5.5/2.54, 
      pointsize = 8)
  { # run this line to get plot in R
    par(mfrow = c(1,2), mar = c(3,2,1,2), oma = c(0,1,0,1), las = 1)
    for(i in 1:2){
      x <- log10((DTavg$res)^2/10000)
      xr = c(floor(min(x)), ceiling(max(x)))
      yr = if(i == 1) c(1,6) else c(0,5)
      xlabels <- formatC(10^pretty(xr), format = "G", digits = 0, drop0trailing = TRUE)
      xlabels <- gsub("\\+","",gsub("\\+0", "", xlabels))
      xlabels <- gsub("1E2", 100, xlabels)
      plot(1, axes = F, type = "n", xlim = xr, ylim = yr, xlab = "", ylab = "")
      axis(side = 1, at = pretty(xr), pos = yr[1], lwd = 1, cex.axis = 0.8, tcl = -0.5, labels = xlabels)
      axis(side = 2, at = pretty(yr), pos = xr[1], lwd = 1, cex.axis = 0.8, tcl = -0.5)
      axis(side = 3, at = pretty(xr), tick = T, lwd.ticks = 0, labels = F, pos = yr[2], lwd = 1)
      axis(side = 4, at = pretty(yr), tick = T, lwd.ticks = 0, labels = F, pos = xr[2], lwd = 1)
      mtext(LETTERS[i], 3, -1.5, adj = 0.9, cex = 1.1)
      clip(xr[1],xr[2], yr[1], yr[2])
      if(i == 1){
        points(x,DTavg$g, pch = 16, cex = 1)
        lines(x,DTavg$g, lwd = 1)
        abline(h = usa_Dt, lwd = 1, lty = 2, col = 'dark red')
        mtext(side = 1, 'Area (ha)', line = 2, cex = 1)
        mtext(side = 2, expression(italic('D')), line = 1.5, cex = 1, las = 0)
        legend(xr[1],yr[2], legend = c(expression(bar(paste(italic('D'),gamma))), 
                                       expression(bar(paste(italic('D'),tau)))), 
               cex = 1, lty = c(1,2), pch = c(16, NA), 
               col = c('black', 'dark red'), adj = .05, seg.len = 1.5)
        
      } else {
        cols = c('black', RColorBrewer::brewer.pal(3, 'Set1'))
        lines(x,sqrt(MSD$MSD), lwd = 1.5, lty = 1, col = cols[1])
        #lines(x,sqrt(MSD$SB), lwd = 1.5, lty = 2, col = cols[2])
        #lines(x,sqrt(MSD$SDSD), lwd = 1.5, lty = 3, col = cols[3])
        mtext(side = 1, 'Area (ha)', line = 2, cex = 1)
        mtext(side = 2, expression(paste(italic('RMSD'))),#' , ', 
                                         #italic('Bias'),' , ', 
                                         #Delta['SD'])), 
              line = 1.5, cex = 1, las = 0)
        legend(xr[1],yr[2], cex = .8, lty = 1:4, col = cols, 
               adj = .05, seg.len = 1.5, 
               legend = c(expression(italic("RMSD")),
                          #expression(italic("Bias")),
                          #expression(Delta['SD']),
                          expression(paste( "1 - ",italic('r')))))
        
        par(new = T)
        plot(1, axes = F, type = "n", xlim = xr, ylim = c(0,2), xlab = "", ylab = "")
        axis(side = 4, at = pretty(c(0,2)), pos = xr[2], lwd = 1, cex.axis = 0.8, tcl = -0.5)
        lines(x,1- MSD$r, lwd = 1.5, lty = 2, col = cols[2])#lty = 4, col = cols[4])
        mtext(side = 4, expression(paste('1 - ',italic('r'))), line = 1.7, cex = 1,las = 0)
      }
    }
  }
  dev.off()
}

# Da of the whole USA using 1980m res sampling units
da2 <- data.table(g = getValues(raster('D:/GammaDiversity/Dg_0001980m_Mean.tif')),
                  n = getValues(raster('D:/nCellSpatial/n_0001980m_Mean.tif')))
da2 <- da2[n > th$Threshold_n[th$Resol_m == 1980],]
usa_a <- da2[,exp(sum(log(g) * n/(sum(n))))]

DTavg_b <- DTavg[res_m > 1980, ]

# USA crop species diversity ####
usa_freq <- fread("Results/CDLfrequencies/CDLfreq_Tot.csv")
usa_freq <- usa_freq[!is.na(perc.ag),]
usa_g <- usa_freq[,exp(-sum(perc.ag/100 * log(perc.ag/100)))]
usa_res <- sqrt(8080464.3)*1e3

usa <- data.table(res_m = usa_res, g = usa_g, t = usa_Dt, a = usa_a, b = usa_g/usa_a)
DTavg_b <- rbind(DTavg_b, setDT(usa), fill = TRUE)
DTavg_b

# beta diversity plot ###########
{ # run this line to save fig to folder
  fig.file = "Plots/scale_beta.png"
  #Delete file if it exists
  if(file.exists(fig.file)) file.remove(fig.file)
  png(filename = fig.file, units = 'in', width = 3.5, height = 3.5, type = "cairo", res = 900)
  { # run this line to get plot in R
    par(mfrow = c(1,1), mar = c(3,3,2,2), oma = c(0,0,0,0), las = 1)
    yr = c(1,10)
    x <- log10((DTavg_b$res_m)^2/10000)
    xr = c(floor(min(x)), ceiling(max(x)))
    xlabels <- 10^pretty(xr)
    xlabels <- formatC(10^pretty(xr), format = "G", digits = 0, drop0trailing = TRUE)
    xlabels <- gsub("\\+","",gsub("\\+0", "", xlabels))
    plot(1, axes = F, type = "n", xlim = xr, ylim = yr, xlab = "", ylab = "")
    axis(side = 1, at = pretty(xr), pos = yr[1], lwd = 1, cex.axis = 0.8, tcl = -0.5, labels = xlabels)
    rug(x = seq(1, max(xr),2), side = 1, ticksize = -0.015, pos = yr[1])
    axis(side = 2, at = pretty(yr), pos = xr[1], lwd = 1, cex.axis = 0.8, tcl = -0.5)
    rug(x = seq(1, max(yr),2), side = 2, ticksize = -0.015, pos = xr[1])
    axis(side = 3, at = pretty(xr), tick = T, lwd.ticks = 0, labels = F, pos = yr[2], lwd = 1)
    axis(side = 4, at = pretty(yr), tick = T, lwd.ticks = 0, labels = F, pos = xr[2], lwd = 1)
    #mtext(LETTERS[i], 3, -1.8, adj = 0.93, cex = 1.3)
    clip(xr[1],xr[2], yr[1], yr[2])
    
    points(x,DTavg_b$g, pch = 16, cex = 1.2)
    lines(x,DTavg_b$g, lwd = 1.5)
    points(x,DTavg_b$b, pch = 24, cex = 1.2, bg = 'dark red')
    lines(x,DTavg_b$b, lwd = 1.5, lty = 1, col = 'dark red')
    abline(h = usa_a, lwd = 1, lty = 2, col = 'dark blue')
    mtext(side = 1, 'Area (ha)', line = 2, cex = 1)
    mtext(side = 2, expression(italic('D')), line = 1.5, cex = 1, las = 0)
    legend(min(xr),max(yr), lwd = 1.5, cex = 1, lty = c(1,2,1), pch = c(16,NA, 24), 
           col = c('black','dark blue', 'dark red'), pt.bg = c('dark red'), adj = .05, seg.len = 1.5,
           legend = c(expression(gamma), expression(alpha), expression(beta))) 
  }
  dev.off()
}

