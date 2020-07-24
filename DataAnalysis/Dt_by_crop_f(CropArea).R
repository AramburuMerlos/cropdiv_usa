library(data.table)
library(nlme)
library(minpack.lm)

d <- fread("Results/Dt_by_crop.csv")
d[, freq:= as.double(freq)]
d <- d[, .(Dt = sum(freq/sum(freq) * Dt), n = sum(freq)), by = .(ID)]
d[, prop := n/sum(n)]

cnames <- fread("cropnames.csv")
d <- merge(d, cnames, by = 'ID')
d <- d[A_N == 'A',]
d[,c("ID", "n", "A_N"):= NULL]
d <- d[!grep("Other", Name),]
setorderv(d, "prop", -1L)


# plateau - linear models
mod1 <- function(x,a,b,yo) pmin( a + b * x , yo)
mod2 <- function(x,b,yo) pmin( b * x + 1, yo)

df = data.frame(x = log10(d$prop), y = d$Dt)
# linear regression coeficcients to get starting values
lrc <- coef(lm(y~x,data = df))
mod.fit <- nlsLM(y ~ mod1(x,a,b,yo), data = df, start = list(a = lrc[1], b = lrc[2], yo = 3.5))
nlc <- coef(mod.fit)

# knot possition (in %)
(bpx <- 10^((nlc[3]-nlc[1])/nlc[2])*100)
summary(mod.fit)

fitted <- data.frame(x = df$x, y = as.vector(mod.fit$m$fitted()))
# add knot to fitted data and reorder
fitted <- rbind(fitted, data.frame(x = log10(bpx/100), y = nlc[3]))
fitted <- fitted[order(fitted$x),]

#theoretical max
tmax <- data.frame(p = seq(0.1,1,0.01))
tmax$m <- 1/tmax$p
tmax$lp <- log10(tmax$p)


mod2.fit <- nlsLM(y ~ mod2(x,b,yo), data = df, start = list(b = lrc[2], yo = 2.5))
nlc2 <- coef(mod2.fit)

# knot possition (in %)
(bpx2 <- 10^((nlc2[2]-1)/nlc2[1])*100)
summary(mod2.fit)

fitted2 <- data.frame(x = df$x, y = as.vector(mod2.fit$m$fitted()))
# add knot to fitted data and reorder
fitted2 <- rbind(fitted2, data.frame(x = c(0,log10(bpx2/100)), y = c(1,nlc2[2])))
fitted2 <- fitted2[order(fitted2$x),]

# complete plot ####
# plot
{ # run this line to save fig to folder
  fig.file = "Plots/CropDt_f(CropArea).png"
  #Delete file if it exists
  if(file.exists(fig.file)) file.remove(fig.file)
  png(filename = fig.file, units = 'cm', width = 11.4, 
      height = (11.4-(2.54*1.5)), type = "cairo", res = 300)
  { # run here to get plot in R
    par(mfrow = c(1,1), mai = c(0.8,0.5,0,1.8), oma = c(0,0,0,0), las = 1, xpd = T)
    xr = c(-7,0)
    yr = c(1,5)
    xn <- length(df$x)
    cols <- viridisLite::viridis(xn)
    pt <- rep(c(21:25,0:9),10)[1:xn]
    plot(1, axes = F, type = "n", xlim = xr, ylim = yr, xlab = "", ylab = "")
    axis(side = 1, at = pretty(xr), pos = yr[1], lwd = 1, cex.axis = .85, 
         tcl = -0.4, labels = 10^(pretty(xr))*100, cex.axis = 0.7, mgp = c(0,0.7,0))
    axis(side = 2, at = pretty(yr), pos = xr[1], lwd = 1, cex.axis = 0.85, tcl = -0.4, cex.axis = 0.7)
    axis(side = 3, at = pretty(xr), tick = T, lwd.ticks = 0, labels = F, pos = yr[2], lwd = 1)
    axis(side = 4, at = pretty(yr), tick = T, lwd.ticks = 0, labels = F, pos = xr[2], lwd = 1)
    
    points(df$x, df$y, pch = pt, cex = 0.8, col = ifelse(pt < 20, cols,'black'), bg = cols)
    
    mtext(side = 1, 'Crop area (%)', line = 1.5, cex = 1)
    mtext(side = 2, expression(paste(italic('D'), tau)), line = 1.5, cex = 1, las = 0)
    legend(x = max(xr)+(xr[2]-xr[1])*.05, y = max(yr)*1.05, 
           legend = d$Name[1:24], col = ifelse(pt<20,cols,'black')[1:24], bty = 'n', 
           pch = pt[1:24], pt.bg = cols[1:24], cex = 0.58)
    legend(x = max(xr)+(xr[2]-xr[1])*.48,  y = max(yr)*1.05, 
           legend = d$Name[25:xn], col = ifelse(pt<20,cols,'black')[25:xn], bty = 'n', 
           pch = pt[25:xn], pt.bg = cols[25:xn], cex = 0.58)
    clip(xr[1],xr[2], yr[1], yr[2])
    lines(fitted, col = 'red', lty = 2)
    lines(fitted2$x,fitted2$y, col = 'blue', lty = 4)
    lines(tmax$lp, tmax$m, col = 'grey50', lty = 3)
    legend(x = xr[1], y = 2, cex = 0.6, col = c("red", "blue", "grey50"), lty = c(2,4,3), bty = 'n',
           legend = c("Empirical", "Empirical (1, 100)", "Theoretical Maximum"))
    
    #text(xr[1]+(xr[2]-xr[1])*.05,yr[1]+(yr[2]-yr[1])*.15, pos = 4, label = "p-value < 0.001")
  }
  dev.off()
}


