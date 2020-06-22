BigAggrDiv <- function(x, fact, m, filename = '', ...){
  m <- apply(m, 2, as.character)
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
    # compute frequencies for each crop and out cell
    DTn <- DTt[,.N, by = .(xval, outcell)]
    rm(DTt)
    DTn <- DTn[!is.na(xval), ]
    if(i == 1){
      DT <- DTn 
    } else{
      DT <- rbind(DT, DTn)
      DT <- DT[,.(N = sum(N, na.rm = T)), by = .(xval,outcell)]
    }
    rm(DTn)
    pbStep(pb, i)
  }
  DT <- dcast(DT, outcell~xval, value.var = 'N')
  mt <- m[m[,1] %in% colnames(DT),]
  for(j in 1:nrow(mt)) DT[, mt[j,2] := rowSums(.SD, na.rm = T), .SDcols = c(mt[j,2], mt[j,1])]
  for(j in 1:nrow(mt)) DT[, mt[j,3] := rowSums(.SD, na.rm = T), .SDcols = c(mt[j,3], mt[j,1])]
  DT[,(mt[,1]):=NULL]
  sc <- colnames(DT)[colnames(DT)!='outcell']
  DT[,(sc):= .SD/rowSums(.SD, na.rm = T), .SDcols = (sc)]
  DT[,div:= exp(-rowSums(.SD * log(.SD), na.rm = T)), .SDcols = (sc)]
  na.outcells <- (1:ncell(out))[!(1:ncell(out) %in% DT$outcell)]
  DT <- rbind(DT, data.table(outcell = na.outcells), fill = TRUE)
  setorder(DT, outcell)
  out <- writeValues(out, DT$div, 1)     
  out <- writeStop(out)
  pbClose(pb)
} 
  
  