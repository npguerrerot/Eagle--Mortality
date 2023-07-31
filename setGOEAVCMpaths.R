# Get the path of the current script
library("rstudioapi")  
setwd(dirname(getActiveDocumentContext()$path))  
codes_dir <- "codes"
for ( code_r in c("GetWyomingCountyInfoR.R", 
"CarcassOnRoadsR.R", 
"getDeerCarcassDataR.R", 
"getEagleUseHourDataR.R", 
"CauchyErrorQuantileR.R", 
"FitDistributions_UseHoursPerCarcassDayR.R", 
"FitDistributions_ProbabilityOfScavengingR.R", 
"CauchyErrorR.R", 
"FitDistributions_DeerCarcassPersistenceR.R", 
"EagleMortalityR.R"
)){
  source( file.path("codes", code_r ) )
}
library(ggplot2)
