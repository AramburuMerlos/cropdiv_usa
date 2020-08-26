path <- "Results/"
cdl <- read.csv(file.path(path, "CountyDg_cdl.csv"))
census <- read.csv(file.path(path, "CountyDg_2012census.csv"))

comp <- merge(cdl, census, by.x = "GEOID", by.y = "geo.id")
comp <- comp[complete.cases(comp),]
head(comp)
# include only counties with more than 20% of crop area (as in Aguilar)
comp20 <- comp[comp$CropArea_prop > 0.2,]

# Compare Diversity or Shannon Index
#index <- "D"
index <- "H"

# define x and y based on indexes to be compared
if(index == "D"){
  x <- comp20$Dg_allSP
  y <- comp20$Dg
} else if (index == "H"){
  x <- log(comp20$Dg_allSP)
  y <- log(comp20$Dg)
} else {
  print("Define a valid index")
}

# linear regression
fit <- lm(y ~ x)
summary(fit)

# prediction
newx <- seq(min(x), max(x), length.out=100)
preds <- predict(fit, newdata = data.frame(x = newx), 
                 interval = 'prediction')

# stats
r2 <- summary(fit)$r.squared
rmse <- sqrt(mean(x - y)^2)
coef <- round(coef(fit), 3)

rmse/mean(x)*100

# Figure -----

{ # run this line to save fig to folder
  fig.file <- paste0("Plots/CensusVsCDL_", index, ".png")
  #Delete file if it exist
  if(file.exists(fig.file)) file.remove(fig.file)
  png(filename = fig.file, units = 'in', width = 3.5, height = 3.5, type = "cairo", res = 900)
  
  { #run this line to get plot in R 
    par(pty = "s", mgp = c(0,0.8,0.8), mar = c(3.5,3.5,0.5,0.5), las = 1)
    maxy <- ceiling(max(y))
    plot(1, axes = F, type = "n", xlim = c(0,maxy), ylim = c(0,maxy), ylab = "", xlab = "")
    axis(side = 1, at = pretty(c(0,maxy)), pos = 0, lwd = 2)
    axis(side = 2, at = pretty(c(0,maxy)), pos = 0, lwd = 2)
    axis(side = 3, at = pretty(c(0,maxy)), tick = T, lwd.ticks = 0, labels = F, pos = maxy, lwd = 2)
    axis(side = 4, at = pretty(c(0,maxy)), tick = T, lwd.ticks = 0, labels = F, pos = maxy, lwd = 2)
    if(index == "H"){
      mtext(side = 1, expression(italic(H)['(USDA Census)']) , line = 2)
      mtext(side = 2, expression(italic(H)['(Crop Data Layer)']), line = 2, las = 0)
    } else{
      mtext(side = 1, expression(italic(D)['(USDA Census)']) , line = 2)
      mtext(side = 2, expression(italic(D)['(Crop Data Layer)']), line = 2, las = 0)
    }
    points(x,y, pch = 21, cex = 0.5, col = "#00000080")
    lines(c(0:maxy),c(0:maxy), col = "red", lty = 3, lwd = 2)
    clip(0,maxy,0,maxy)
    lines(newx, preds[,1], col = "dark blue", lwd = 2)
    polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = "#ADD8E633", border = NA)
    lines(newx, preds[,2], col = "dark blue", lty = 2)
    lines(newx, preds[,3], col = "dark blue", lty = 2)
    
    text(maxy*0.45,maxy*0.1, pos = 4, label = paste("y =",coef[1], "+", paste0(coef[2], "x"),
                                                   "\nRMSE = ", round(rmse,3), "\n"), cex = 0.9)
    text(maxy*0.45,maxy*0.05, pos = 4, bquote(R^2 == .(round(r2, 3))), cex = 0.9)
  } # end of plot
  dev.off()
} # end saving fig to folder



