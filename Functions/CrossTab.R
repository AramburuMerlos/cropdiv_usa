CrossTab <- function(x, y, digits = 2, x_digits = digits, y_digits = digits, ...) {
  
  if(!compareRaster(x,y)) stop ("x and y rasters don't match")
  
  bs <- blockSize(x, n = 2)
  x_int <- grepl("INT", dataType(x))
  y_int <- grepl("INT", dataType(y))
  
  pb <- pbCreate(bs$n, progress = 'text')
  
  for (i in 1:bs$n) {
    xv <- getValues(x, bs$row[i], bs$nrows[i]) 
    yv <- getValues(y, bs$row[i], bs$nrows[i]) 
    noNA <- which(!is.na(yv))
    
    if(length(noNA) != 0){
      DTt <- data.table(x = xv, y = yv)
      DTt <- DTt[noNA,]
      if(!x_int) DTt[, x:= round(x, digits = x_digits)]
      if(!y_int) DTt[, y:= round(y, digits = y_digits)]
      DTt <- DTt[, .(freq=.N), by = .(x, y)] 
      if(!exists("DT")){
        DT <- DTt
      } else {
        DT <- rbindlist(list(DT,DTt))  
        DT <- DT[,lapply(.SD,sum), by = .(x,y)]   
      }
    }
    pbStep(pb, i)
  }
  pbClose(pb)
  DT <- DT[freq != 0,]
  cn <- c(deparse(substitute(x)), deparse(substitute(y)))
  setnames(DT, old = c("x", "y"), new = cn)
 
  return(DT)
}
    