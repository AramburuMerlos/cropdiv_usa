# sud = sampling unit (true gamma) diversity
# sun = samplit unit number of crop cells
# totn = toatal number of crop cells

aggrAlpha <- function(sud, sun, totn, filename = '', ...){
  
  extsud = extent(sud)
  extsun = extent(sun)
  exttot = extent(totn)
  ressud = res(sud)
  ressun = res(sun)
  restot = res(totn)
  
  if(!all(identical(extsud, extsud), 
           identical(ressud,ressun))) {
    stop('sud and sun must have same extent and resolution')
  }
  
  if(any(restot %% ressun != 0)) {
    stop('totn resolution must be multiple of sun resolution')
  }
  
  fact = restot[1] / ressun[1] # factor by which to aggregate
  
  # raster output (out) 
  out <- raster(totn)
  
  #  if it is necessary to expand the raster...
  exp.rast = !all((dim(sud) %% fact)[1:2] == 0)
  
  if(exp.rast){
    dimout <- ceiling(dim(sud)/fact)
    oute <- extent(c(
      xmin = extsud[1],
      xmax = extsud[1] + dimout[2]*restot[1], 
      ymin = extsud[4] - dimout[1]*restot[2],
      ymax = extsud[4]))
    extent(out) <- oute
  }
  
  # set output resolution 
  res(out) <- restot
  
  # number of cells/cols/rows of raster x/out
  ncout <- ncol(out)
  ncsud <- ncol(sud)
  nrout <- nrow(out)
  n <- ncell(out)
  
  # Define how to save the output ----
  big <- !canProcessInMemory(sud, 5)
  filename <- trim(filename)
  
  if(big & filename == ''){
    filename <- rasterTmpFile()
  }
  
  if (filename != ''){
    out <- writeStart(out, filename, ...)
    todisk <- TRUE
  } else {
    todisk <- FALSE
  }
  
  # Cell indexes ----
  # first focal square
  cells.c1 = rep(1:fact,fact) + rep(ncsud * (1:fact - 1), each = fact)
  # list for focal squares col = 2:ncout
  cells.cs = lapply(1:(ncout-1), function(col)(cells.c1 + col*fact))
  # combine lists to get cell index for one row
  cells = c(list(cells.c1), cells.cs)
  # leftover when aggregate by fact
  remainder = ncsud %% fact
  
  if(remainder != 0){ 
    m = matrix(cells[[ncout]], fact, fact, byrow = TRUE) 
    m <- m[,1:remainder] 
    cells[[ncout]] <- as.vector(m)[order(m)]
  }

  # function to sum and get NA if all NA (instead of 0)
  mysum <- function(x) if(all(is.na(x))) NA_real_ else sum(x, na.rm=T)
  
  # Function to be applied to each aggregated pixel and its sample units
  truealpha <- function(sud, sun, totn){
    exp(sum(log(sud) * sun, na.rm = TRUE) / totn)
  }
  
  # starting row of each loop
  sr <- fact * (1:nrout - 1) + 1 
  
  # Focal Loop ----
  pb <- pbCreate(nrout, progress = "text") 
  
  if(todisk){
    for(r in 1:nrout){
      sudval <- getValues(sud, sr[r], fact)
      sunval <- getValues(sun, sr[r], fact)
      totval <- getValues(totn, r, 1)
      
      # apply alpha diversity estimation by cell indexes
      val = mapply(function(i, j) truealpha(sud = sudval[i], 
                                            sun = sunval[i], 
                                            totn = totval[j]),
                   i = cells, j = 1:ncout)
      out <- writeValues(out, val, r)
      pbStep(pb,r)
    }
    out <- writeStop(out)
    pbClose(pb)
    
  } else {
    vals <- vector(length = nrout, mode = 'list')
    for(r in 1:nrout){
      sudval <- getValues(sud, sr[r], fact)
      sunval <- getValues(sun, sr[r], fact)
      totval <- getValues(totn, r, 1)
      
      # mapply alpha diversity estimation by cell indexes
      vals[[r]] = mapply(function(i, j) truealpha(sud = sudval[i], 
                                                  sun = sunval[i], 
                                                  totn = totval[j]),
                         i = cells, j = 1:ncout)
    }
    out <- setValues(out,unlist(vals))
  }
  return(out)
}

