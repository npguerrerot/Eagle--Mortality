CauchyErrorQuantile <- function(x, t, y, tau, ret_val = 0) {
# function CauchyErrorQuantile - 
#    Calculates an error quantile so fminsearch can be used to determine a quantile fit to a distribution
#    when written in Matlab, this code used persistent variables to store the data and allow for multiple 
#    calling syntaxes, one which is for fminsearch, one which returns the distribution.   Since R does
  #    not use persistent variables that is no longer a feature of this function.  However, there are still
  #    multiple syntaxes based on the use of a default third parameter ret_val (i.e. it is called by fminsearch
  #    which requires one behavior, and then called again to get the function form once fminsearch has 
  #    determined the parameters of the function.) 
  #
# if trying to figure out this code, I suggest you first figure out the CauchyErrorR.R part, since that 
  # is an ordinary least squares (OLS) type of a calculation.  (Again, it's a bit strange because it was
  # written in matlab and then translated to R, but it's calculating something relatively common).
  #
  # Then, consider this function as a modification of that ... it's doing a quantile calculation.  This error 
  # condition is based on equation (3) of Meinshausen and Ridgeway, Quantile Regression Forests.  


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

## documentation from the Matlab version:
#
#% CauchyErrorQuantile - determine error for a cauchy distribution
#%
#%  Syntax
#%  [err, yth,yout]=CauchyErrorQuantile(t,y,tau);   % initialization syntax
#%  [err, yth,yout]=CauchyErrorQuantile(x);
#%
#%   Difference between first two syntaxes is number of elements in the
#%   argument to the function.
#%
#%   err = OLS error between the cauchy distribution specified by gamma=x
#%   and the initial dataset
#%
#%  Note that this can be called as follows:
#%   x=fminsearch('CauchyErrorQuantile',[1])
