# First, load the constants file
source(file.path(codes_dir, "GOEAVCMconstantsfileR.R"))

# Script to call EagleMortality

fidout <- file("EagleMortalityOutputFilename.txt", open = "w")


#[!,!,CountyName] <- GetWyomingCountyInfo
# "~" syntax tells matlab you don't need the output/makes cleaner code.

source(file.path(codes_dir, "GetWyomingCountyInfoR.R"))
CountyName

# Write header row to output file
cat('CollisionMortality,k,ScalarMultiplier,RemovalInterval,code,deathsWY,mortalityWY', file = fidout, append = TRUE)

# Loop over CountyName vector and write data to output file
for (j in seq_along(CountyName)) {
  cn <- CountyName[j]
  cat(paste0(',', cn, '_totalEagles,', cn, '_SU,', cn, '_SUremoved50p,', cn, '_GEdeathNR50,', cn, '_GEdeathWR50,', cn, '_GEdeathNR20,', cn, '_GEdeathWR20,', cn, '_GEdeathNR80,', cn, '_GEdeathWR80'), file = fidout, append = TRUE)
}
cat('\n', file = fidout, append = TRUE)

# Clear minval (not used in this code block)
rm(minval)


# Since the three parameters correspond to the numerical expressions in the
# paper, I'm keeping this syntax for EagleMortality.
minval <- matrix(0, nrow = length(ksteps), ncol = length(linearspacedJP) )
deaths <- matrix(0, nrow = length(ksteps), ncol = length(linearspacedJP) )
mortality<- matrix(0, nrow = length(ksteps), ncol = length(linearspacedJP) )
for (m in seq_along(linearspacedJP)) {
  for (j in seq_along(ksteps)) {
    
    AvgAdMortality <- linearspacedJP[m] # actually this is the collision rate
    Usteps <- 5
    
    # Call EagleMortality function and save outputs
    res <- EagleMortality(c(ksteps[j], Usteps, AvgAdMortality))
    minval[j, m] <- res[[1]]
    EagleMortalityPerCounty <- res[[2]]
    EaglesPerCounty <- res[[3]]
    CountyMortalityStructure <- res[[4]]
    deaths[j, m] <- sum(EagleMortalityPerCounty)
    mortality[j, m] <- sum(EagleMortalityPerCounty) / sum(EaglesPerCounty)
    
    RIV <- CountyMortalityStructure[[1]]$RemovalIntervalVector
    code <- paste0(AvgAdMortality, '_', ksteps[j], '_', Usteps)
    
    for (jRI in seq_along(RIV)) {
      RI <- RIV[jRI]
      
      # Calculate...
      
      cat(sprintf('%f,%f,%f,%f,%s,%f,%f', 
                  AvgAdMortality, ksteps[j], 
                  Usteps, RI, code, 
                  deaths[j, m], mortality[j, m]), file = fidout, append = TRUE)
      
      for (jcty in seq_along(CountyMortalityStructure)) {
        CMS <- CountyMortalityStructure[[jcty]]
        
        TotalEagles <- EaglesPerCounty[jcty]
        SU <- CMS$NumDeerThisCounty
        SUremoved <- CMS$CountySURemovals[jRI]
        
        GEdeathNR50 <- CMS$CountyMortality[1, 2]
        GEdeathNR20 <- CMS$CountyMortality[1, 1]
        GEdeathNR80 <- CMS$CountyMortality[1, 3]
        GEdeathWR50 <- CMS$CountyMortality[jRI, 2]
        GEdeathWR20 <- CMS$CountyMortality[jRI, 1]
        GEdeathWR80 <- CMS$CountyMortality[jRI, 3]
        
        cat(sprintf(',%f,%f,%f,%f,%s,%f,%f', 
                    TotalEagles, SU, SUremoved, 
                    GEdeathNR50, GEdeathWR50, GEdeathNR20, 
                    GEdeathWR20, GEdeathNR80, GEdeathWR80), file = fidout, append = TRUE)
      }
      cat('\n', file = fidout, append = TRUE)
    }
  }
}


# Example matrix (replace this with your actual matrix data)
matrix_values <- minval

# Define custom x and y axis labels
x_labels <- ksteps
y_labels <- linearspacedJP

# Create a data frame for plotting
df <- data.frame(x = rep(x_labels, each = length(y_labels)),
                 y = rep(y_labels, times = length(x_labels)),
                 value = as.vector(matrix_values))

# Create the plot (grid of matrix values)
ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  labs(x = "k",
       y = "Multi-eagle collision proba impact",
       title = " Params where WY eagle mortality = 0.01. '") +
  scale_fill_gradient(low = "blue", high = "yellow") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(x_labels), max(x_labels), by = 20), 
                     labels = seq(min(x_labels), max(x_labels), by = 20) )



close(fidout)



# Example matrix (replace this with your actual matrix data)
matrix_values <- t(minval)

# Define custom x and y axis labels
x_labels <- ksteps
y_labels <- linearspacedJP * 1000

# Create a data frame for plotting
df <- data.frame(x = rep(x_labels, each = length(y_labels)),
                 y = rep(y_labels, times = length(x_labels)),
                 value = as.vector(matrix_values))

# Create the plot (grid of matrix values)

ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_raster(aes(fill = value), interpolate=TRUE) +
  scale_fill_gradient2(low="blue", mid="green", high="yellow", midpoint = 0.92, 
                       limits=range(df$value)) + 
  labs(x = "Traffic Avoidance Parameter ",
       y = "Collision Likelihood Parameter (*1000)",
       title = " WY eagle mortality agreement") +
  theme(
    text = element_text(size = 25, face = "bold"),
    axis.text = element_text(size = 25, face = "bold"),
    axis.title = element_text(size = 25, face = "bold"),
    axis.line = element_line(size = 3),
    axis.ticks = element_line(size = 3),
    panel.grid.major = element_line(size = 3),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(x_labels), max(x_labels), by = 20), 
                     labels = seq(min(x_labels), max(x_labels), by = 20) )












# Example matrix (replace this with your actual matrix data)
matrix_values <- t(mortality)

# Define custom x and y axis labels
x_labels <- ksteps
y_labels <- linearspacedJP * 1000

# Create a data frame for plotting
df <- data.frame(x = rep(x_labels, each = length(y_labels)),
                 y = rep(y_labels, times = length(x_labels)),
                 value = as.vector(matrix_values))

# Create the plot (grid of matrix values)
color_breaks <- c(0, 0.025, 0.04)  # Define the breaks for the color scale
color_values <- c("blue", "green", "yellow")  # Define the colors for each break
names(color_values) <- color_breaks

ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_raster(aes(fill = value), interpolate=TRUE) +
  scale_fill_gradientn(colors = color_values, limits = c(0, 0.04), 
                       na.value = "yellow") + 
  labs(x = "Traffic Avoidance Parameter ",
       y = "Collision Likelihood Parameter (*1000)",
       title = " WY eagle mortality") +
  theme(
    text = element_text(size = 25, face = "bold"),
    axis.text = element_text(size = 25, face = "bold"),
    axis.title = element_text(size = 25, face = "bold"),
    axis.line = element_line(size = 3),
    axis.ticks = element_line(size = 3),
    panel.grid.major = element_line(size = 3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    panel.grid = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(x_labels), max(x_labels), by = 20), 
                     labels = seq(min(x_labels), max(x_labels), by = 20) ) +
  coord_cartesian(xlim = c(min(x_labels), max(df$x)), ylim = c(0, max(df$y)))



