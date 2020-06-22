BigAggrN <- function(x, fact, filename = '', ...){
  
  # output resolution
  outres <- res(x)*fact
  # raster output (out) 
  out <- raster(x)
  # check if it is necessary to expand the raster...
  exp.rast = !all((dim(x) %% fact)[1:2] == 0)
  # if it is necessary to expand the raster...
  if(exp.rast){
    xe <- extent(x)
    dimout <- ceiling(dim(x)/fact)
    oute <- extent(c(
      xmin = xe[1],
      xmax = xe[1] + dimout[2]*outres[1], 
      ymin = xe[4] - dimout[1]*outres[2],
      ymax = xe[4]))
    extent(out) <- oute
  }
  # set output resolution 
  res(out) <- outres
  # number of cells in input raster
  ncx <- ncol(x)
  ncolout <- ncol(out)
  # predefined operations for functions with raster
  out <- writeStart(out, filename, ...)
  bs <- blockSize(x, n = 1)
  pb <- pbCreate(bs$n, progress = 'text')  
 
  for (i in 1:bs$n) {
    xval <- getValues(x, bs$row[i], bs$nrows[i]) 
    # add row and column number for each observation from input (x) raster
    DTt <- data.table(xrow = rep(bs$row[i]:(bs$row[i]+bs$nrows[i]-1), each = ncx),
                     xcol = rep(1:ncx, times = bs$nrows[i]),
                     xval = xval)
    rm(xval)
    # add column with cell number that each observation corrersponds to for the out raster
    DTt[,outrow := ceiling(xrow/fact)]
    DTt[,outcol := ceiling(xcol/fact)]
    DTt[,outcell:= outcol + (outrow-1)*ncolout]
    DTt[,c("outrow", "outcol", "xrow", "xcol"):= NULL]
    DTt <- DTt[!is.na(xval),]
    DTn <- DTt[,.N, by = outcell]
    rm(DTt)
    if(i == 1){
      DT <- DTn 
    } else{
      DT <- rbind(DT, DTn)
      DT <- DT[,.(N = sum(N, na.rm = T)), by = outcell]
    }
    rm(DTn)
    pbStep(pb, i)
  }
  na.outcells <- (1:ncell(out))[!(1:ncell(out) %in% DT$outcell)]
  DT <- rbind(DT, data.table(outcell = na.outcells, N = NA))
  setorder(DT, outcell)
  out <- writeValues(out, DT$N, 1)     
  out <- writeStop(out)
  pbClose(pb)
} 
  
  