# function to compute diversity from crop frequencies as in ENCS 
diver <- function(x){
  p <- x / sum(x)
  H <- -sum(p * log(p))
  return(exp(H))
}
