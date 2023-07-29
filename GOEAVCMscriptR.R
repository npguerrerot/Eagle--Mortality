# This is a script which will -in theory- run the codes necessary to
# reproduce the results which are presented in Lonsdorf et al [currently in
# review at JWM]

# Note that while we are here sharing the codes, we are sharing
# the full set of data only to researchers who put in a request.  We are
# doing this so that we can know who has the data.  Requests can be made
# via e-mail to sslater@hawkwatch.org


# note that if a new set of data is incorporated, getDeerCarcassData.m will
# need to be updated.  See the comments in that file and the end of
# getEagleUseHourData for an explanation of why and how.


# set up the paths
# four directories:
#        codes/  are the codes written for this research program
#    utilcodes/  are codes that are necessary for codes/ to run
#    datafiles/  data files which codes/ will look for
#         util/  some usefile files

setwd("C:/Users/nguer/Dropbox/Matlab to R/Mitigation")

# Set up the paths
basedir <- getwd()
codes_dir <- file.path(basedir, "codes")
utilcodes_dir <- file.path(basedir, "utilcodes")
datafiles_dir <- file.path(basedir, "datafiles")
util_dir <- file.path(basedir, "util")

# Constants files. Contains locations of datafiles, and some things users may want to override.
source(file.path(codes_dir, "GOEAVCMconstantsfile.R"))

# Helper function that calls the main function
source(file.path(codes_dir, "callEagleMortalityR.R"))

# Post-processing

# These functions, if called with no output arguments, will make figures.
source(file.path(codes_dir, "FitDistributions_DeerCarcassPersistenceR.R"))
source(file.path(codes_dir, "FitDistributions_ProbabilityOfScavengingR.R"))
source(file.path(codes_dir, "FitDistributions_UseHoursPerCarcassDayR.R"))

# Additional figures in the util/ directory
source(file.path(util_dir, "figure1.R"))
source(file.path(util_dir, "figure2.R"))
source(file.path(util_dir, "figure3.R"))
# Add more source() calls for other figure files in the util/ directory as needed.

