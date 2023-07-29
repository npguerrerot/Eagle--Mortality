################################################################################
# R code:
################################################################################
# Set up the directory path for EVSPhotoDataBaseDir
EVSPhotoDataBaseDir <- file.path("datafiles")

# Set the output filename for EagleMortalityOutputFilename
EagleMortalityOutputFilename <- "EagleMortalityOutput.csv"

# Set the values for linearspacedJP and ksteps
linearspacedJP <- seq(0.1, 2, by = 0.4) * 0.0008
ksteps <- seq(10, 150, by = 30)



