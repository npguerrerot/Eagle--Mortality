CarcassOnRoads <- function(countyFIPS) {
  # read in carcass on roads data, re-format
  # written by Deepak Ray
  
  #open and read in the vph file
  fidinr <- readLines('datafiles/WY_road_VPH_cty.csv')
  fidind <- readLines('datafiles/WY_deercarcass_VHP_cty.csv')
  
  headerr <- fidinr[1]
  headerd <- fidind[1]
  
  ccr <- gregexpr(",", headerr)[[1]]
  
  for (i in 1:(length(ccr)-1)) {
    countyfips <- as.numeric(substr(headerr, ccr[i]+1, ccr[i+1]-1))
    
    if (countyfips == countyFIPS) {
      county_index <- i
    }
  }
  
  km_road <- rep(0, 31)
  deer_road <- rep(0, 31)
  
  #read in the raw km_road for this county
  for (i in 1:31) {
    oneLiner <- fidinr[i+1]
    ccr <- gregexpr(",", oneLiner)[[1]]
    km_road[i] <- as.numeric(substr(oneLiner, ccr[county_index]+1, ccr[county_index+1]-1))
    
    oneLined <- fidind[i+1]
    ccd <- gregexpr(",", oneLined)[[1]]
    deer_road[i] <- as.numeric(substr(oneLined, ccd[county_index]+1, ccd[county_index+1]-1))
  }
  
  vph <- c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135, 145, 155, 165, 175, 185, 195, 250, 350, 450, 550, 650, 750, 850, 950, 1050, 1150, 1200)
  
  ii <- which(km_road == 0)
  n <- length(ii)
  
  #make a smaller array removing those km_road and corresponding vph
  road_types <- 31 - n
  
  actual_vph <- rep(0, road_types)
  actual_km_road <- rep(0, road_types)
  actual_deer_road <- rep(0, road_types)
  
  n <- 1
  for (i in 1:31) {
    if (deer_road[i] > 0) {
      #only study roads that have deer carcasses
      actual_vph[n] <- vph[i]
      actual_km_road[n] <- km_road[i]
      actual_deer_road[n] <- deer_road[i]
      n <- n + 1
    }
  }
  
  #now swap back
  deer_road <- actual_deer_road
  km_road <- actual_km_road
  vph <- actual_vph
  
  return(list(deer_road = deer_road, km_road = km_road, vph = vph, road_types = road_types))
}