# thresholds of low crop area cells to be removed from the analysis.

library(raster)
library(lspline)

gpath <- "D:/GammaDiversity"
npath <- "D:/nCellSpatial"

gfn <- Sys.glob(file.path(gpath, '*Mean.tif'))
gl <- lapply(gfn, raster)

nfn <- Sys.glob(file.path(npath, '*Mean.tif'))
nl <- lapply(nfn, raster)

# check if raster layers have equal extent and dimensions
all(mapply(function(d, n) (all(extent(d)==extent(n), dim(d)==dim(n))), gl, nl))

# list of data frame withs spatial diversity and ncell (abs and perc) values
# only complete cases
dfs <- mapply(function(d, n){
  dv <- getValues(d)
  nv <- getValues(n)
  df <- data.frame(n = nv, d = dv)
  df <- df[complete.cases(df),]
  return(df)
}, gl, nl, SIMPLIFY = FALSE)

names_dfs <- gsub(paste0(gpath, '/Dg_'), '', sub('_Mean.tif', '', gfn))
names(dfs) <- names_dfs

resols <- as.numeric(gsub('m', '', names_dfs))
  
# check that resols from file name match raster resolutions
all(mapply(function(x,y)  res(x)[1] == y, gl, resols))

# total number of cells per aggCell for each resol
ntots <- (resols/30) ^ 2

# create column for crop area proportion
for(i in 1:length(dfs)){
  dfs[[i]]['p'] <- dfs[[i]]['n'] / ntots[i]
}

# keep only cells with 20% of crop area or less
dfs = lapply(dfs, function(df)df[df$p<=0.2,])

# knots to fit linear splines
knots = c(.5,1:18)/100

# empty data frame to save regression coefficients
regs <- expand.grid('Resol_m' = resols, 'knots'= knots)
regs <- regs[order(regs$Resol_m),]
regs[c('intecept', 'Slope1', 'pvalue1', 'Slope2', 'pvalue2')] <- NA

sk <- ifelse(resols < 4000, 6, 1) #starting knot changes with resol

for(i in 1:length(dfs)){
  for(k in sk[i]:length(knots)){
    mod <- lm(d ~ lspline(p, knots = knots[k]), data = dfs[[i]])
    sc <- summary(mod)$coefficients
    r <- (i-1)*length(knots) + k
    regs$intecept[r] <- sc[1,1]
    regs$Slope1[r] <- sc[2,1]
    regs$pvalue1[r] <- sc[2,4]
    regs$Slope2 [r] <- sc[3,1]
    regs$pvalue2[r] <- sc[3,4]
    if(regs$pvalue2[r] > 0.01) break
    if(regs$Slope2[r] < 0 ) break
  }
}

regs <- regs[!is.na(regs$Slope1),]

final_th = tapply(regs$knots, regs$Resol_m, max)
final_th <- data.frame(Resol_m = names(final_th), Threshold_p = final_th)

# check if there is no significant positive slope for some resolutions. Then, change those thresholds to 0
ns <- tapply(regs$pvalue2, regs$Resol_m, min) > 0.01 & 
      tapply(regs$pvalue1, regs$Resol_m, max) > 0.01

final_th$Threshold_p[ns] <- 0

# add ncell threshold column
final_th$Threshold_n <- round(final_th$Threshold_p * ntots)
final_th$res <- names_dfs 
write.csv(final_th, 'PorcAreaThreshold.csv', row.names = F)


# FIGURE ----
library(RColorBrewer)

set.seed(0)
source('Functions/transparency.R')

figpath <- 'Plots/'
dir.create(figpath)

{ # run this line to save fig to folder
  figfn <- file.path(figpath, 'CropAreaThreshold.png')
  #Delete file if it exists
  if(file.exists(figfn)) file.remove(figfn)
  png(filename = figfn, units = 'in', 
      width = 6, height = 6, type = "cairo", res = 900)
  
  { #run this line to get plots in R 
    par(mfrow = c(4,4), mgp = c(0,0.5,0), 
        las = 1, mar = c(1,1,0.5,0.5), oma = c(1,1,0,0))
    for (i in 1:length(dfs)){             
      maxy <- if(i < 6) 6 else if (i < 14) 8 else 16
      plot(1, axes = F, type = "n", xlim = c(0, 20), 
           ylim = c(1, maxy), ylab = "", xlab = "")
      axis(side = 1, at = seq(0, 20, 2), pos = 1, 
             lwd = 1, cex.axis = 0.9, tcl = -0.3)
      axis(side = 2, at = if(i < 14) 1:maxy else seq(1, 16, 3),
           pos = 0, lwd = 1, cex.axis = 0.9, tcl = -0.3)
      axis(side = 3, at = seq(0, 20, 2), tick = T, lwd.ticks = 0, 
           labels = F, pos = maxy, lwd = 1)
      axis(side = 4, at = if(i < 14) 1:maxy else seq(1, 16, 3),
           tick = T, lwd.ticks = 0, labels = F, pos = 20, lwd = 1)
      mtext(paste(resols[i]/1000, 'km'), 
            side = 3, adj = 0.92, line = -1.5, cex = 0.8)
      clip(0, 20, 1, maxy)
      nr = nrow(dfs[[i]])
      bw = transparency(nr, cluster = nr > 1000)
      if(nr > 30000){
        xy = dfs[[i]][sample(1:nr, 30000),]
      }else{
        xy = dfs[[i]]
      }
      
      points(xy$p * 100, xy$d, pch = 21, col = bw,
             cex = if(nr > 1000) 0.6 else if(nr > 100) 0.8 else 1)
      
      reg = regs[regs$Resol_m == resols[i],]
      cols = c(colorRamps::blue2red(nrow(reg))[nrow(reg):2], 'dark blue')
      if(nrow(reg) == 1) cols = 'dark blue'
      if(final_th$Threshold_p[i] > 0){
        for (k in 1:nrow(reg)){
          x = seq(0, 0.2, 0.002)
          yhat = ifelse(x < reg$knots[k], 
                        x * reg$Slope1[k] + reg$intecept[k],
                        reg$knots[k] * reg$Slope1[k] + reg$intecept[k] + x * reg$Slope2[k])
          pred = cbind(x, yhat)  
          lines(pred[,1] * 100, pred[,2], lwd = 1, col = cols[k], 
                lty = ifelse(k == nrow(reg),1,2))
        }
        abline (v = final_th$Threshold_p[i]*100, col = 'green3', lty = 1, lwd = 2)
      }  
    }
    mtext(side = 1, "Crop Area (%)", line = 1.8, cex = 0.7, outer = T )
    mtext(side = 2, expression(italic('D')[gamma]), line = 1.5, las = 0, cex = 0.7, outer = T)
  } # end of plot
  dev.off()
} # end saving fig to folder

