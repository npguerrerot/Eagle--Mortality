FitDistributions_DeerCarcassPersistence <- function( ret_val = 1){

  res <- getDeerCarcassData()
  y <- res$data
  
  y[100] <- 0
  y[is.na(y)] <- 0
  
  library(pracma)
  res1 <- fminsearch(CauchyError, c(1,1), y = y, ret_val = 1, method="Nelder-Mead")
  res <- CauchyError(res1$xmin, y, ret_val = 0 )
  err <- res$err
  ytheory <- res$ytheory
  yout <- res$yout
  
  t <- 1:length(yth)
  
  if ( ret_val == 1){
    # Assuming you have t, yth, and yout vectors already defined
    
    # Load necessary libraries
    library(ggplot2)
    
    # Create a data frame with t, yth, and yout
    data <- data.frame(days = t, yth = yth, yout = yout)
    
    # Create the plot using ggplot2
    p <- ggplot(data, aes(x = days)) +
      geom_line(aes(y = yth)) +         # Line plot for yth
      geom_bar(aes(y = yout), stat = "identity", alpha = 0.5) +  # Bar plot for yout
      labs(x = "days",                 # x-axis label
           y = "Count",                # y-axis label
           title = "Deer Carcass Persistence") +  # Plot title
      theme_minimal()
    
   
    
   
    # Add x and y-axis labels
    xlabel <- "days"
    ylabel <- "Carcass persistence"
    title <- "Carcass persistence interval (75 total carcasses)"
    hp <- plot(t, y, pch = 16, col = "black", cex = 1.5, main = title, xlab = xlabel, ylab = ylabel)
    
    
    
    
    
    # Assuming you have t and yth vectors already defined
    
    # Calculate the yth values as percentages of the maximum value
    yth_percent <- yth / max(yth) * 100
    
    # Create the plot with the calculated percentages
    plot(t, yth_percent, type = "l", col = "black", yaxt="n",xlab = "days", ylab = "Carcass persistence probability", main = "Carcass persistence probability")
    
    # Set the x-axis limit between 0 and 40
    xlim <- c(0, 40)
    
    # Customize the y-axis labels to show percentages from 0% to 100%
    y_ticks <- seq(0, 100, by = 20)
    y_labels <- paste0(y_ticks, "%")
    axis(2, at = y_ticks, labels = y_labels)
    
    
    
    
    
    
    # Assuming you have t and yth vectors already defined
    
    # Calculate the yth values as percentages of the maximum value
    yth_percent <- yth / max(yth) * 100
    
    # Generate a vector for the "Expert" data
    expert_data <- c(100, 100, 100, 50, 0, rep(0, 95))
    
    # Create the plot with both datasets
    plot(t, yth_percent, type = "l", col = "black", yaxt="n", xlab = "days", ylab = "Carcass persistence probability", main = "Carcass persistence probability")
    lines(t, expert_data, col = "black", lty = 2) # Add the "Expert" data as a dashed line
    
    # Set the x-axis limit between 0 and 40
    xlim <- c(0, 40)
    
    # Add a legend to distinguish the two datasets
    legend("topright", legend = c("HWI data", "Expert"), lty = c(1, 2), col = "black")
    
    # Customize the y-axis labels to show percentages from 0% to 100%
    
    y_ticks <- seq(0, 100, by = 20)
    y_labels <- paste0(y_ticks, "%")
    axis(2, at = y_ticks, labels = y_labels)
    
    
    
    
    
    # Assuming you have t, yout, and yth vectors already defined
    
    # Create the plot with black circles as markers for yout
    hp <- plot(t, yout, pch = 16, col = "black", xlab = "Carcass Days", ylab = "Persistance Probability")
    points(t, yth, col = "black")  # Add the yth data as black circles
    # Set the x-axis limit between 0 and 30
    xlim <- c(0, 30)
    
    
    # Add a legend to distinguish the "data" and "model"
    legend("topright", legend = c("data", "model"), pch = c(16, 1), col = "black")
    
    # Add labels for y-axis and x-axis
    ylabel <- "Persistance Probability"
    xlabel <- "Carcass Days"
    title <- "Carcass Persistence Probability"
    title(main = title, xlab = xlabel, ylab = ylabel)
    
    
  }else{
    dayvect <- t
    probavect <- ytheory
    dcpdata <- y
    return(list(dayvect=dayvect, probavect=probavect, dcpdata=dcpdata))
  }
}
