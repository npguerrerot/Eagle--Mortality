EagleMortality <- function(paramvector = c(30, 2, 0.0008) ) {

  MakeFile <- 0
  lengthofseason <- 180
  ReferenceEagleDensity <- 0.03
  Nrealizations <- 10
  RemovalIntervalVector <- c(0, 1) # 3, 7, 14, 30]
  TestCountyFlag <- 0
  
  codes_dir <- "codes"
  switch(TestCountyFlag,
         0, {
           county_info <- source(file.path(codes_dir, "GetWyomingCountyInfoR.R"))
         },
         1, {
           warning('Test Counties: 100; 10000 km^2, 10 deer on 25vph roads')
           FIPS <- c(101, 103)
           sq_km <- c(100, 10000)
           CountyName <- c('TestCounty1', 'TestCounty2')
         },
         2, {
           warning('Test Counties, vph changes, 10 deer per road, 10000 km^2 per county')
           vphvect <- seq(10, 100, by = 5)
           
           FIPS <- numeric(length(vphvect))
           sq_km <- rep(10000, length(vphvect))
           CountyName <- paste0('TestCountyVPH', vphvect)
           
           for (j in seq_along(vphvect)) {
             FIPS[j] <- (199 + j * 2)
           }
         }
  )

# Initialization steps
IS <- list()

# Key parameters: default values
IS$collisionperVxE <- 0.001
IS$k <- (10 + 35) / 2.0 # 50 # disturbed
IS$UHscale <- 1 # reference density correction

IS$k <- paramvector[1]
IS$UHscale <- paramvector[2]
IS$collisionperVxE <- paramvector[3]

# do carcass day model fit
res1 <- FitDistributions_UseHoursPerCarcassDay(ret_val = 0 )
dayvect2 <- res1$dayvect
avg_tot_usehr_percarcday_distribution1 <- res1$probavect
dcpdata2 <- res1$dcpdata

res1 <- FitDistributions_ProbabilityOfScavenging(ret_val = 0 )
dayvect1 <- res1$dayvect
ProbabilityOfScavenging <- res1$probavect
dcpdata1 <- res1$dcpdata

res1 <- FitDistributions_DeerCarcassPersistence(ret_val = 0 )
dayvect <- res1$dayvect
DeerCarcPersistenceProbaVect <- res1$probavect
dcpdata <- res1$dcpdata

# even though we have 1-88 days and need to determine probability of a
DCPcum <- cumsum(DeerCarcPersistenceProbaVect)



#Cycle through each county
NumDeerThisCounty <- c()
CountyMortalityStructure <- list()

for (jcounty in 1:length( CountyName ) ){
  
  Nmax <- 1e7
  listoferfinvs <- erfinv( runif( Nmax ) )
  erfinvcounter <- 0
  
  cat(sprintf('Evaluating %s FIPS:%d\n', CountyName[jcounty], FIPS[jcounty]))
  
  # Different counties in Wyoming have a different set of roads (in terms
  # of VPH and road length and number of deer carcasses observed in each
  # road segment. So,
  # determine the observed small ungulates on roads in the county and the
  # vehicles per hour on those roads.
  
  if (TestCountyFlag == 0){
    res1 <- CarcassOnRoads(FIPS[jcounty])
    deer_road <- res1$deer_road
    km_road <- res1$km_road
    vph <- res1$vph
    road_types <- res1$road_types
  } else if (TestCountyFlag == 1){
    deer_road <- c(0, 1.5, 2)
    vph <- c(15, 25, 35)
  } else if (TestCountyFlag == 2){
    deer_road <- c(25, 25)
    vph <- c(1, 1) * vphvect[jcounty]
  }
  
    
    M <- array(0, dim = c( 2, 10, 500 ) )
    RemovalMat <- array(0, dim = c( 2, 10, 500 ) )
    i <- 1
    
    jv_vec <- c()
    for (jreal in 1:Nrealizations){
      
      
      # Construct a Carcass Structure "CS"
      CSvector <- list()
      counter <- 0 # this will count up the number of carcasses in this realization of this county.
      Ndeer <- rep(0, length(vph))
      for (jrt in 1:length(vph)) { 
        numDeerOnThisRoadType <- deer_road[jrt]
        
        f <- numDeerOnThisRoadType - floor(numDeerOnThisRoadType)
        
        x <- runif(n=1)
        
        if (x > f) {
          numDeerOnThisRoadTypeInteger <- floor(numDeerOnThisRoadType)
        } else {
          numDeerOnThisRoadTypeInteger <- ceiling(numDeerOnThisRoadType)
        }
        
        Ndeer[jrt] <- numDeerOnThisRoadTypeInteger
        
        if (numDeerOnThisRoadTypeInteger >= 1){
          
          for ( j in 1:numDeerOnThisRoadTypeInteger ){
            
            CS <- list()
            
            CS$VPH <- vph[jrt]
            CS$RemovalInterval <- RemovalIntervalVector
            
            counter <- counter + 1
            CSvector[[counter]] <- CS
            
          }
          
        }
      }
      # end of road type loop
      NumDeerThisCounty[jreal] <- sum(Ndeer)
      
      jv_vec <- c(jv_vec, length(CSvector) ) 
      for (jv in 1:length(CSvector)) {
        
        CS <- CSvector[[jv]]
        
        # how many days for this particular carcass x this particular realization?
        NumDays <- min(which(DCPcum > 0.5))
        # NumDays <- 4
        
        usehoursrealization <- rep(0, NumDays)
        for (jd in 1:NumDays) {
          meanusehours <- avg_tot_usehr_percarcday_distribution1[jd] * ProbabilityOfScavenging[jd]
          
          erfinvcounter <- erfinvcounter + 1
          
          sigma <- sqrt(pi) / sqrt(2) * meanusehours
          
          usehoursrealization[jd] <- sigma * sqrt(2) * listoferfinvs[erfinvcounter]
          
          # end
        }
      
          
        for (jRI in 1:length(CSvector[[jv]]$RemovalInterval)) {
          
          RI <- CSvector[[jv]]$RemovalInterval[jRI]
  
            if (RI == 0 || is.infinite(RI)) {
              # this is a flag for no removal
              RemovalFlag <- 0
              Ndays <- length(usehoursrealization) # confusing code, sorry ... how many days here?
            } else {
              
              
              DOI <- sample(1:RI, size = 1)
              if (DOI < length(usehoursrealization)) {
                # this is a removal!!
                Ndays <- DOI
                RemovalFlag <- 1
              } else if (DOI == length(usehoursrealization)) {
                # this is a removal!!
                Ndays <- DOI
                usehoursrealization[Ndays] <- usehoursrealization[Ndays] / 2
                RemovalFlag <- 1
              } else {
                # no removal
                RemovalFlag <- 0
                Ndays <- length(usehoursrealization)
              }
            }
            
          
            k <- IS$k
            UHscale <- IS$UHscale
            t <- CSvector[[jv]]$VPH
            theta <- t^2 / (t^2 + k^2)
            
            H <- (1 - theta) * UHscale * sum(usehoursrealization[1:Ndays])
            VH <- CSvector[[jv]]$VPH * H
            mu <- IS$collisionperVxE
            
            M[jRI, jreal, jv] <- (1 - (1 - mu)^VH)
            RemovalMat[jRI,jreal,jv] <- RemovalFlag
            
            i <- i + 1
        }## End of removal interval loop.
      }# end of loop over 'jv' number of carcasses on roads in this simulation
      # how many days until this carcass is removed?
      #    CSvector(jv).Mortality=quantile(M,[.2 .5 .8],2);
      #    CSvector(jv).Removals=mean(RemovalMat,2);
    }# number of realizations
    
    # now have done all of the realizations and need to figure out some
    # statistics.  Everything is in M.
    M <- M[,,1:max(jv_vec)]
    RemovalMat <-  RemovalMat[,,1:max(jv_vec)]
    tmp <- apply(M, MARGIN = c(1, 2), FUN = sum)
    Mortality <- t(apply(tmp, c( 1 ), quantile, probs = c(0.2, 0.5, 0.8)))
    
    # dimensions of Mortality:  [removal intervals ] x 3, where 3 is
    # quantiles.

    tmp <- apply(RemovalMat, MARGIN = c(1, 2), FUN = sum)
    Removals <- rowMeans(tmp)
    
    
    res1 <- list()
    res1$CountyMortality <- Mortality
    res1$CountyName <- CountyName[jcounty]
    res1$RemovalIntervalVector <- RemovalIntervalVector
    res1$CountySURemovals <- Removals
    res1$SU <- RemovalIntervalVector
    res1$NumDeerThisCounty <- mean(NumDeerThisCounty)
    CountyMortalityStructure[[jcounty]] <- res1
    
}# end of jcounty loop
# calculate total mortality

CountyMortalityNoRemovals <- c()
EaglesPerCounty <- c()
for (j in 1:length(FIPS)) {
  CountyMortalityNoRemovals[j] <- CountyMortalityStructure[[j]]$CountyMortality[1, 2]
  EaglesPerCounty[j] <- sq_km[j] * ReferenceEagleDensity
}

EagleMortalityPerCounty <- CountyMortalityNoRemovals

# now we calculate "x" this is something we might want to minimize so it
# can take advnatage of some built-in optimization functions.
x <- (1 - sum(CountyMortalityNoRemovals) / sum(EaglesPerCounty))^2


return( list( x = x, 
              EagleMortalityPerCounty = EagleMortalityPerCounty, 
              EaglesPerCounty = EaglesPerCounty, 
              CountyMortalityStructure = CountyMortalityStructure ) )

}

