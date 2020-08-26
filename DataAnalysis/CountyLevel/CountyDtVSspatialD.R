library(raster)
library(data.table)

myavg <- function(Dt, freq) exp(sum(log(Dt) * freq/sum(freq)))

# County Dg
Dg <- fread("Results/CountyDg_cdl.csv")

# County Dt
Dt <- fread("Results/CountyDt_freq.csv")
Dt <- Dt[ , .(Dt = myavg(Dt, freq)), by = .(county)]
setnames(Dt, 'county', 'GEOID')

# County Da (1320m) 
g132 <- raster("D:/GammaDiversity/Dg_0001320m_Mean.tif")
c132 <- raster("D:/USrasterized/CountiesGEOID_1320m.tif")
n132 <- raster("D:/nCellSpatial/n_0001320m_Mean.tif")
Da <- data.table('GEOID' = getValues(c132), 
                 'D' = getValues(g132), 
                 'n' = getValues(n132))
th <- read.csv("PorcAreaThreshold.csv") # ncell threshold
th132 <- th$Threshold_n[th$Resol_m == 1320]
Da <- Da[n > th132,]
Da <- Da[, .(Da = myavg(Dt = D, freq = n), n = sum(n)), by = .(GEOID)]

# join (merge) data tables
d <- merge(Dt, Dg, by = "GEOID", all = FALSE)
d <- merge(d, Da, by = "GEOID", all = FALSE)

# County Db
d[, Db:= Dg/Da]

# county area and crop area (%)
counties <- tigris::counties()
counties <- spTransform(counties, CRSobj = crs(c132))
counties$area_km2 <- rgeos::gArea(counties, byid = TRUE)/1e6
c_area <- data.table(GEOID = as.numeric(counties$GEOID),
                     area_km2 = counties$area_km2)
d <- merge(d, c_area, by = "GEOID", all = FALSE)
d[ , crop_prop := (n*900/1e6)/area_km2]

summary(d)
# remove low percentage counties (5%)
d <- d[crop_prop > 0.05,]

# Diversity downscaled by county area
mod <- readRDS("Results/lm_Dg_f(log10(res_m)).RDS") # linear model
d[, lrx:= log10(sqrt(area_km2 * 1e6))] # x of linear regression
preds <- function(x) predict.lm(mod, newdata = data.frame(lr = x))
d[, Dhat:= preds(lrx)] # County average diversity at each county area
D_200ha <- predict.lm(mod, newdata = data.frame(lr = log10(sqrt(200 * 1e4))))
d[, Dg_af:= Dg - (Dhat - D_200ha)]


# dissimilarity functions & stats
RMSD <- function(x,y) sqrt(mean((x - y)^2))
bias <- function(x,y) abs(mean(x) - mean(y))
SD <- function(x) sqrt(mean((x - mean(x))^2))

rmsd1 <- d[, RMSD(Dt, Dg)]
b1 <- d[, bias(Dg, Dt)]
sdt <- d[, SD(Dt)]
sdg <- d[, SD(Dg)]
Dsd1 <- sdg - sdt
r_tg <- d[, cor(Dt,Dg)]

rmsd2 <- d[, RMSD(Dt, Dg_af)]
b2 <- d[, bias(Dg_af, Dt)]
sdgaf <- d[, SD(Dg_af)]
r_tgaf <- d[, cor(Dt, Dg_af)]
Dsd2 <- sdgaf - sdt

rmsd3 <- d[, RMSD(Dt, Da)]
b3 <- d[, bias(Da, Dt)]
sda <- d[, SD(Da)]
r_ta <- d[, cor(Da, Dt)]
Dsd3 <- sda - sdt

#rmsd <- c(rmsd1, rmsd2, rmsd3)
#b <- c(b1, b2, b3)
#Dsd <- c(Dsd1, Dsd2, Dsd3)
#r <- c(r_tg, r_tgaf, r_ta)

rmsd <- c(rmsd1, rmsd3)
b <- c(b1, b3)
Dsd <- c(Dsd1, Dsd3)
r <- c(r_tg, r_ta)


setorder(d, Db)
d[, Db:= round(Db, 2)]

{ # run this line to save fig to folder
  fig.file = "Plots/Dt(fx)DCounty.pdf"
  #Delete file if it exists
  if(file.exists(fig.file)) file.remove(fig.file)
#  png(filename = fig.file, units = 'cm', width = 17.8, 
#      height = 5, type = "cairo", res = 300, pointsize = 9)

  pdf(file = fig.file, width = 11.4/2.54, height = 5/2.54, pointsize = 9)
  
  { #run this line to get plots in R 
    #par(mfrow = c(1,3), mgp = c(0,1,0), mar = c(2,2,1,1), oma = c(2,2,0,0), las = 1)
    par(mfrow = c(1,2), mgp = c(0,0.9,0), mar = c(2,2,1,1), oma = c(1,1,0,0), las = 1)
    for (i in 1:2){
      xr = c(0,14)
      yr = c(0,4)
      plot(1, axes = F, type = "n", xlim = xr, ylim = yr, ylab = "", xlab = "")
      axis(side = 1, at = pretty(xr), pos = yr[1], lwd = 1, cex.axis = 0.9, tcl = -0.5)
      axis(side = 2, at = pretty(yr), pos = xr[1], lwd = 1, cex.axis = 0.9, tcl = -0.5)
      axis(side = 3, at = pretty(xr), tick = T, lwd.ticks = 0, 
           labels = F, pos = yr[2], lwd = 1)
      axis(side = 4, at = pretty(yr), tick = T, lwd.ticks = 0, 
           labels = F, pos = xr[2], lwd = 1)
      mtext(side = 3, line = -1.5, cex = 1.1, adj = 0.92, text = LETTERS[i])
      cols_lin <- viridis::plasma(d[,max(Db) - min(Db),] * 100 + 1) 
      cols <- cols_lin[(d$Db - min(d$Db))*100 + 1] # resample cols to match Db value
      if(i == 1){ 
        points(d$Dg, d$Dt, pch=1, col= cols, cex = 0.8)
        mtext(side = 1,bquote("County" ~ italic('D') * gamma), line = 2, cex = 1)
        mtext(side = 2, bquote("County" ~ italic('D') * tau), 
              line = 1.5, cex = 1,las = 0)
      }
#     if(i == 2){
#       points(d$Dg_af, d$Dt, pch=1, col = cols, cex = 0.8)
#       mtext(side = 1,bquote("County" ~ italic('D') * gamma ~ '- Bias'), line = 2.4, cex = 1)
#     } 
      if(i == 2){
        points(d$Da, d$Dt, pch=1, col = cols, cex = 0.8)
        mtext(side = 1,bquote("County" ~ italic('D') * alpha), line = 2, cex = 1)
      } 
      clip(xr[1], xr[2], yr[1], yr[2])
      abline(b=1,a=0, lty = 2)
      text(x= 8.3, y = 1.1, labels = paste0('RMSD = ', round(rmsd[i],2)), adj = 0, cex = 0.8)
      text(x= 8.3, y = 0.8, labels = paste0('Bias = ', round(b[i],2)), adj = 0, cex = 0.8)
      text(x= 8.3, y = 0.5, labels = bquote(Delta['SD'] ~ '=' ~ .(round(Dsd[i],2))), adj = 0, cex = 0.8)
      text(x= 8.3, y = 0.2, labels = bquote(italic('r') ~ '=' ~ .(round(r[i],2))), adj = 0, cex = 0.8)
     
      if(i == 2){
        plotfunctions::gradientLegend(range(d$Db), color = cols_lin, 
                                      pos = c(9,1.5,10.5,3), coords = T, dec = 1, 
                                      border.col = 'black', n.seg = 1)
        text(x = 9.8, y = 3.3, labels = bquote(italic('D') * beta))
      }
    }
  } # end of plot
  dev.off()
} # end saving fig to folder


