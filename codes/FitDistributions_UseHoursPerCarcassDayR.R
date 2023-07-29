FitDistributions_UseHoursPerCarcassDay <- function(datafit = "goldeneaglescomplete", ret_val = 1){

  res <- getEagleUseHourData("goldeneaglescomplete")
  carcassdays <- res$carcassdays
  usehours <- res$usehours
  CamID <- res$CamID
  tau = 0.5
  library(pracma)
  res1 <- fminsearch(CauchyErrorQuantile, c(1, 1), t = carcassdays, 
                     y = usehours, tau = tau, ret_val = 1, method="Nelder-Mead")
  res <- CauchyErrorQuantile(res1$xmin, carcassdays, usehours, tau )
  err <- res$err
  yth <- res$yth
  yout <- res$yout 
  
  t <- 1:length(yth)
  
  if (ret_val == 1 ){
    x_seq <- 1:50
    
    # Plot yth for the first 50 points
    plot(x_seq, yth[1:50], type = 'l', xlab = 'days', ylab = '', xlim = c(1, 50),
         main = paste('use hours per carcass day, Cauchy dist, OLS fit error =', err))
    
    # Add data points for carcassdays and usehours
    points(carcassdays, usehours, pch = 'o')
    
    # Add xlabel at the bottom of the plot
    text(25, -3, 'days', pos = 1)
    
    # Assuming you have 'yth' vector calculated from your data
    
    
    
    
    
    # Assuming you have 'carcassdays' and 'usehours' vectors with data
    
    # Plot 'carcassdays' against 'usehours' in black ('o' represents circles)
    plot(carcassdays, usehours, pch = 'o', col = 'black', xlab = 'days', ylab = 'hours',
         xlim = c(0, 25), main = 'Use hours per carcass day')
    
    # Add xlabel at the bottom of the plot
    text(25, -1, 'days', pos = 1)
    
    # Add ylabel on the left side of the plot
    text(-1, max(usehours), 'hours', pos = 2, srt = 90)
    
    # Note: There is no direct equivalent for 'zeroxlim' or 'reallyreallyfattenplot' functions in R. 
    # You might need to adjust the plot appearance based on your specific requirements.
    
    
    
    
    # Create a sequence from 1 to 50 for plotting
    x_seq <- 1:50
    
    # Plot yth for the first 50 points in black color ('k')
    plot(x_seq, yth[1:50], type = 'l', col = 'black', xlab = 'days', ylab = 'hours',
         xlim = c(0, 25), ylim = c(0, 3), main = 'Use hours per carcass day')
    
    # Draw the patch
    polygon(c(3, 3, 5, 5), c(0.5, 2.6, 2.6, 0.5), col = 'gray')
    
    # Add xlabel at the bottom of the plot
    text(25, -0.5, 'days', pos = 1)
    
    # Add ylabel on the left side of the plot
    text(-1.5, 3, 'hours', pos = 2, srt = 90)
    
    # Add legend
    legend('topright', legend = c('HWI data', 'Expert'), 
           col = c('black', 'gray'), lty = c(1, 0), lwd = c(1, 10))
    
    # Note: The 'reallyreallyfattenplot' function in MATLAB is not a standard function in R. You might need to adjust the plot appearance based on your specific requirements.
    
    
    # Assuming you have 'carcassdays', 'usehours', and 'yth' vectors with data
    
    # Create a sequence from 1 to 30 for plotting
    x_seq <- 1:30
    
    # Plot 'carcassdays' against 'usehours' in black ('o' represents circles)
    plot(carcassdays, usehours, pch = 'o', col = 'black', xlab = 'Carcass Days', ylab = 'Hours',
         xlim = c(0, 30), ylim = c(0, max(usehours, yth[1:30])), main = '')
    
    # Add 'yth' data for the first 30 points in black color ('k')
    lines(x_seq, yth[1:30], col = 'black')
    
    # Add xlabel at the bottom of the plot
    text(30, -1, 'Carcass Days', pos = 1)
    
    # Add ylabel on the left side of the plot
    text(-1, max(usehours, yth[1:30]), 'Hours', pos = 2, srt = 90)
    
    # Add legend
    legend('topright', legend = c('data', 'model'), col = c('black', 'black'), pch = c('o', '-'))
    
    
    
  } else {
    dayvect <- 1:100
    probavect <- yth
    dcpdata <- yout
    return(list(dayvect = dayvect, probavect = probavect, dcpdata = dcpdata))
  }
}
