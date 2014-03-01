BikeshareData <- setClass("BikeshareData",
         slots = c(location   = "character",
             startTime  = "POSIXt",
             endTime    = "POSIXt",
             startLoc   = "integer",
             endLoc     = "integer",
             bike       = "integer",
             memberType = "character"))

path = "data/was/2010-4th-quarter.csv"
readTripData <-  function(path){
}
