CauchyErrorQuantile <- function(x, t, y, tau, ret_val = 0) {
  

  gamma <- x[1]
  scale <- x[2]
  t0 <- 1:100
  ytheory <- (gamma * (1 + (t0 / gamma)^2))^(-1)
  ytheory <- ytheory / sum(ytheory)
  ytheory <- ytheory * scale
  
  ymodel <- numeric(length(t))
  
  for (m in 1:length(t)) {
    this_t <- t[m]
    j <- which(this_t == t0)
    ymodel[m] <- ytheory[j]
  }
  
  err <- sum((ymodel - y)^2) / length(y)
  
  TAU <- tau
  YVALS <- y
  WVALS <- rep(1, length(y))
  
  ii <- ymodel < YVALS
  jj <- ymodel >= YVALS
  E <- TAU * sum(abs(ymodel[ii] - YVALS[ii])^2) + 
    (1 - TAU) * sum(abs(ymodel[jj] - YVALS[jj])^2)
  
  maxdebug <- 0
  if (maxdebug == 1) {
    plot(t0, ytheory, type = "l")
    points(t, y, pch = 19)
    title(paste("ERROR =", E, "x", x[1], x[2]))
  }
  
  err <- E
  yout <- y
  
  if (ret_val == 1){
    return( err )
  } else{
    return(list(err = err, ytheory = ytheory, yout = yout))
  }
  
}