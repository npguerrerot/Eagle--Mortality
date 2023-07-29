filepath <- system.file("setGOEAVCMpathsR.R")
basedir <- dirname(filepath)
b <- basedir
c <- basedir

library(tools)
add_path <- function(path) {
  path <- file.path(basedir, path)
  .libPaths(path)
}

add_path('')
add_path(file.path('codes'))
add_path(file.path('utilcodes'))
add_path(file.path('datafiles'))
