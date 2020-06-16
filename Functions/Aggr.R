# similar to raster::aggregate, but a little faster
# it was used as the skeleton for other aggregation functions

aggr <- function(x, fact, fun = sum, na.rm = TRUE, filename = '', ...){
  
  # output resolution
  outres <- res(x) * fact
  # raster output (out) 
  out <- raster(x)
  
  # check if it is necessary to expand the raster
  exp.rast = !all((dim(x) %% fact)[1:2] == 0)
  if(exp.rast){
    xe <- extent(x)
    dimout <- ceiling(dim(x) / fact)
    oute <- extent(c(
      xmin = xe[1],
      xmax = xe[1] + dimout[2] * outres[1], 
      ymin = xe[4] - dimout[1] * outres[2],
      ymax = xe[4]))
    extent(out) <- oute
  }
  
  # set output resolution 
  res(out) <- outres
  
  # number of cells/cols/rows of raster x/out
  ncout <- ncol(out)
  ncx <- ncol(x)
  nrout <- nrow(out)
  n <- ncell(out)
  
  # check if it is too big to process in Memory
  big <- ! canProcessInMemory(x, 2) 
  
  filename <- trim(filename)
  # create a temporary file for big rasters and no specified file output
  if(big & filename == ''){
    filename <- rasterTmpFile()
  }
  # when filename is specified (or big raster)
  if (filename != ''){
    out <- writeStart(out, filename, ...)
    todisk <- TRUE
  } else {
    # no specified filename and small rasters
    todisk <- FALSE
  }

  # cells index for first focal square
  cells.c1 = rep(1:fact, fact) + rep(ncx * (1:fact - 1), each = fact)
  # list with cells index for focal squares col = 2:ncout
  cells.cs = lapply(1:(ncout - 1), function(col)(cells.c1 + col * fact))
  
  # combine lists to get cell index for one row
  cells = c(list(cells.c1), cells.cs)
  
  # if there is a leftover when aggregate by fact (and expand = TRUE)
  remainder = ncx %% fact
  
  if(remainder != 0){ 
    m = matrix(cells[[ncout]], fact, fact, byrow = TRUE) 
    m <- m[,1:remainder] 
    cells[[ncout]] <- as.vector(m)[order(m)]
  }

  # starting row for each loop
  sr = fact * (1:nrout - 1) + 1
  
    #### Focal Loop ####
  if(todisk){
    # progress bar
    pb <- pbCreate(nrout, progress = "text")   
    for(r in 1:nrout){
      # save rows values from the starting row, fact number of rows
      rowsVal <- getValues(x, sr[r], fact)
      # lapply function to list of cell index
      val = sapply(cells, function(i)fun(rowsVal[i], na.rm = na.rm))
      # write values to raster out
      out <- writeValues(out, val, r)
      pbStep(pb,r)
      rm(val, rowsVal)
      gc(reset = T)
    }
    out <- writeStop(out)
    pbClose(pb)
  } else {
    # list to save values for each row of out
    vals <- vector(length = nrout, mode = 'list')
    for(r in 1:nrout){
      # save rows values from the starting row, fact number of rows
      rowsVal <- getValues(x, sr[r], fact)
      # lapply function to list of cell index
      vals[[r]] = sapply(cells, function(i)fun(rowsVal[i], na.rm = TRUE))
    }
    out <- setValues(out,unlist(vals))
  }
return(out)
}

