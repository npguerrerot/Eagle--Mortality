getEagleUseHourData <- function(dataoptions){

EVSPhotoDataBaseDir <- "datafiles"

if (!exists("EVSPhotoData")){

  # Load necessary library
  library(readr)
  
  colnames <- c("Reviewer",
  "StudyArea", 
  "Year", 
  "Camera", 
  "CamID", 
  "CamStartTime", 
  "CamEndTime", 
  "CarcassRemovedbeforeCamEnd", 
  "CarcassLastPhoto", 
  "CarcassAvailTime", 
  "MethodOfRemoval", 
  "CarcassSpecies", 
  "DistanceToRoad", 
  "DistRdRound", 
  "Folder", 
  "Subfolder", 
  "ObsNumber3", 
  "PhotoStart", 
  "Species", 
  "Date", 
  "ArrivalTime", 
  "Weather", 
  "Temperature", 
  "SnowCover", 
  "DepartureTime", 
  "TotTime", 
  "Age", 
  "CompleteSequence", 
  "ReasonForDeparture", 
  "EagleInteractions", 
  "VehiclePresent", 
  "PhotoEnd", 
  "Quality", 
  "Comments"
  )
  
  # Specify column types
  col_types <- cols(
    Reviewer = col_factor(),
    StudyArea = col_factor(),
    Year = col_double(),
    Camera = col_factor(),
    CamID = col_double(),
    CamStartTime = col_datetime(format = "%m/%d/%y %H:%M"),
    CamEndTime = col_datetime(format = "%m/%d/%y %H:%M"),
    CarcassRemovedbeforeCamEnd = col_factor(),
    CarcassLastPhoto = col_datetime(format = "%m/%d/%y %H:%M"),
    CarcassAvailTime = col_character(),
    MethodOfRemoval = col_character(),
    CarcassSpecies = col_factor(),
    DistanceToRoad = col_double(),
    DistRdRound = col_double(),
    Folder = col_factor(),
    Subfolder = col_factor(),
    ObsNumber3 = col_double(),
    PhotoStart = col_double(),
    Species = col_factor(),
    Date = col_date(format = "%m/%d/%y"),
    ArrivalTime = col_time(format = "%H:%M:%S"),
    Weather = col_factor(),
    Temperature = col_double(),
    SnowCover = col_double(),
    DepartureTime = col_time(format = "%H:%M:%S"),
    TotTime = col_time(format = "%H:%M:%S"),
    Age = col_factor(),
    CompleteSequence = col_factor(),
    ReasonForDeparture = col_factor(),
    EagleInteractions = col_double(),
    VehiclePresent = col_factor(),
    PhotoEnd = col_double(),
    Quality = col_double(),
    Comments = col_factor()
  )
  
  # Import the data
  EVSPhotoData <- read_csv(paste0(EVSPhotoDataBaseDir, "/EVSdata/EVSPhotoDataSubset.csv"), 
                           col_names = colnames,
                           col_types = col_types, skip = 1)
}
#####################
#  End Import Code  #
#####################
CamStartTime <- as.numeric(EVSPhotoData$CamStartTime)
CamID <- EVSPhotoData$CamID
Species <- EVSPhotoData$Species
TotTime <- as.numeric(EVSPhotoData$TotTime)
CamIDlist <- sort(unique(CamID))
CalculatedTotTime <- EVSPhotoData$DepartureTime - EVSPhotoData$ArrivalTime

# EagleEventDate
Date <- as.POSIXct(EVSPhotoData$Date)
EagleLandTime <- as.POSIXct(paste(EVSPhotoData$Date , EVSPhotoData$ArrivalTime), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")


iige <- Species == 'Golden Eagle'
iibe <- Species == 'Bald Eagle'
iiuk <- Species == 'Unknown'

iiComplete <- EVSPhotoData$CompleteSequence == 'Yes - captured arrival and departure'

# Select rules for which eagle events to keep
iiBird <- iige | iibe
iiKeepBird <- switch(dataoptions,
                     'alleaglescomplete' = iiComplete & iiBird,
                     'alleaglesallsightings' = iiBird,
                     'goldeneaglescomplete' = iiComplete & iige,
                     stop("don't recognize this data option"))
thisCamID <- 61
D <- list()
CarcassDays_vec <- c()
EagleHours_vec <- c()
CameraID_vec <- c()
count <- 0
for (jCam in seq_along(CamIDlist)) {
  thisCamID <- CamIDlist[jCam]
  iiCam <- thisCamID == CamID
  DateList <- unique(Date[iiKeepBird & iiCam])
  if (length(DateList) == 0) {
    message(paste(' no adequate events for camera ', thisCamID))
  } else {
    for (jDate in seq_along(DateList)) {
      iiDate <- Date == DateList[jDate]
      BCInteractionList <- which(iiKeepBird & iiDate & iiCam)
      if (length(BCInteractionList) > 0) {
        CarcassDays <- (as.numeric(EagleLandTime[BCInteractionList]) - CamStartTime[BCInteractionList])/(3600*24)
        if (sum(unique(CarcassDays) < 100 & unique(CarcassDays) > 0) > 0 ) {
          count <- count + 1
          CarcassDays_vec[count] <- unique(ceiling(CarcassDays + 0.5))
          EagleHours_vec[count] <- sum(as.numeric(CalculatedTotTime[BCInteractionList])) / 3600
          CameraID_vec[count] <- thisCamID
        }
      }
    }
  }
}

carcassdays <- CarcassDays_vec
usehours <- EagleHours_vec
CamID <- CameraID_vec



return( list( carcassdays = carcassdays, 
              usehours = usehours, 
              CamID = CamID ) )

}


res <- getEagleUseHourData("goldeneaglescomplete")

res


