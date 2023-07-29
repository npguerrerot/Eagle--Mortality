# Constants files. Contains locations of datafiles, and some things users may want to override.
source(file.path(codes_dir, "GOEAVCMconstantsfileR.R"))

# Helper function that calls the main function
source(file.path(codes_dir, "callEagleMortalityR.R"))

# Post-processing

# These functions, if called with no output arguments, will make figures.
FitDistributions_DeerCarcassPersistence()
FitDistributions_ProbabilityOfScavenging()
FitDistributions_UseHoursPerCarcassDay()


