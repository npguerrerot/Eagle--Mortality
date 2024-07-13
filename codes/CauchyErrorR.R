CauchyError <- function(x, y, ret_val = 1 ) {

  # function CauchyError - 
  #    Calculates an error so fminsearch can be used to determine a fit to a distribution
  #    when written in Matlab, this code used persistent variables to store the data and allow for multiple 
  #    calling syntaxes, one which is for fminsearch, one which returns the distribution.  It's a bit clunky here. 
  #
  # original documentation below
  
  
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

# original documentation from the matlab version
#
#% CauchyError - determine error for a cauchy distribution
#%
#%  Syntax
#%  [err, yth,yout]=CauchyError(initialdataset);
#%  [err, yth,yout]=CauchyError(x);
#%
#%   Difference between first two syntaxes is number of elements in the
#%   argument to the function.
#%
#%   err = OLS error between the cauchy distribution specified by gamma=x
#%   and the initial dataset
#%
#%  Note that this can be called as follows:
#  %   x=fminsearch('CauchyError',[1])