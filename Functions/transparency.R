# transparency retrieves a color code to be used in points(bw=) to get points with a given alpha depending on n
transparency = function(n, black = TRUE, cluster = TRUE){
  if(n>50000) t = 10
  else if (n > 20000) t = 15
  else if (n > 10000) t = 20
  else if (n > 5000) t = 40
  else if (n > 2000) t = 60
  else if (n > 1000) t = 70
  else if (n > 500) t = 80
  else t = 99
  if(cluster) t = t*.8
  if(black){
    return(paste0("#000000",formatC(t, width = 2, format = "d", flag = "0")))
  } else {
    return(t/100)
  }
}
