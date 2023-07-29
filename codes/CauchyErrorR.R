CauchyError <- function(x, y, ret_val = 1 ) {

  if(length(x) > 2) {
    y <- x
    t <- 1:length(y)
    y=y/sum(y)
  } else{
    t <- 1:length(y)
    y=y/sum(y)
    gamma <- x[1]
    delta <- x[2]
  }
  
  
  
  
  ytheory <- (gamma * (1 + (t / gamma)^2))^(-1)
  ytheory <- ytheory / sum(ytheory)
  err <- sum((ytheory[1:length(ytheory)] - y[1:length(y)])^2)
  
  yout <- y
  
  if (ret_val == 1){
    
    return( err )
  }else{
    return(list(err = err, ytheory = ytheory, yout = yout)) 
  }
}
