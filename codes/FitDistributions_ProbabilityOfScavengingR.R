FitDistributions_ProbabilityOfScavenging <- function(datafit = "goldeneaglescomplete", ret_val = 1){
  
  res <- getEagleUseHourData("goldeneaglescomplete")
  carcassdays <- res$carcassdays
  usehours <- res$usehours
  CamID <- res$CamID
  
  tmax <- max(carcassdays) + 5
  
  N <- hist(carcassdays, breaks = seq(0.5, tmax + 0.5, by = 1), plot = FALSE)$counts
  edges <- seq(0.5, tmax + 0.5, by = 1)
  
  t <- 1:tmax
  y <- N/length(unique(CamID))
  
  tau <- 0.5
  
  library(pracma)
  res1 <- fminsearch(CauchyErrorQuantile, c(1, 1), t = t, 
                     y = y, tau = tau, ret_val = 1, method="Nelder-Mead")
  res <- CauchyErrorQuantile(res1$xmin, t, y, tau )
  err <- res$err
  yth <- res$yth
  yout <- res$yout 
  
  t <- 1:length(yth)
  
  if (ret_val == 1){
    
    # Assuming you have 'yth' vector calculated from your data
    # Assuming you have 'yout' vector calculated from your data
    
    # Create a sequence from 1 to 30 for plotting
    x_seq <- 1:30
    
    # Plot 'yth' for the first 30 points as a line plot
    plot(x_seq, yth[1:30], type = 'l', xlab = 'Carcass Days', ylab = '', xlim = c(1, 50),
         ylim = c(0, 0.5),
         main = 'deer carcass scavenging likelihood')
    
    # Add 'yout' as a bar plot
    barplot(yout, col = 'gray', add = TRUE)
    
    # Add xlabel at the bottom of the plot
    text(15, -0.5, 'Carcass Days', pos = 1)
    
    # Note: There is no direct equivalent for 'fattenplot' or 'title' functions in R. 
    # You might need to adjust the plot appearance based on your specific requirements.
    
    
    # Assuming you have 'yth' vector calculated from your data
    # Assuming you have 'yout' vector calculated from your data
    
    # Create a sequence from 1 to the length of 'yout' for plotting
    x_seq_yout <- 1:length(yout)
    
    # Plot 'yout' as black crosses ('k+')
    plot(x_seq_yout, yout, pch = '+', col = 'black', 
         xlab = 'Carcass Days', ylab = 'Scavenging Probability',
         xlim = c(0, 30), ylim = c(0, max(yout, yth[1:50])), main = '')
    
    # Create a sequence from 1 to 50 for plotting 'yth'
    x_seq_yth <- 1:50
    
    # Plot 'yth' for the first 50 points in black color ('k')
    lines(x_seq_yth, yth[1:50], col = 'black')
    
    # Add xlabel at the bottom of the plot
    text(30, -0.02, 'Carcass Days', pos = 1)
    
    # Add ylabel on the left side of the plot
    text(-1, max(yout, yth[1:50]), 'Scavenging Probability', pos = 2, srt = 90)
    
    # Add legend
    legend('topleft', legend = c('data', 'model'), 
           col = c('black', 'black'), lty = c(0, 1), pch = c('+', '-'))
    
    # Note: There is no direct equivalent for 'zeroxlim' or 'reallyreallyfattenplot' functions in R. 
    # You might need to adjust the plot appearance based on your specific requirements.
    
    
  } else {
    dayvect <- t
    probavect <- yth
    dcpdata <- yout
    return(list(dayvect = dayvect, probavect = probavect, dcpdata = dcpdata))
  }
}

FitDistributions_ProbabilityOfScavenging()