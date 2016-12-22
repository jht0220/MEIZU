
mean1 <- function(x){
  if(!is.numeric(x)){
    stop("'x' must be numeric")
  }
  sum(x)/length(x)
}

cov1 <- function(x, y){
  
}