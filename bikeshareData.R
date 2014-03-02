BikeshareData <- setClass("BikeshareData",
         slots = c(location   = "character",
             startTime  = "POSIXt",
             endTime    = "POSIXt",
             startLoc   = "integer",
             endLoc     = "integer",
             bike       = "integer",
             memberType = "integer",
             stationNames = "character"))

path = "data/was/2010-4th-quarter.csv"
readTripData <-  function(path, city=WAS){
    # Allow user to enter String format for city
    if(class(city) == "character"){
        city = get(str)
        if(class(city) != "City"){
            stop("City must be a valid Bikeshare city")
        }
    }
    
    data = read.table(path, sep=city@delim, header=city@header, stringsAsFactors=FALSE, quote="\"")

    # Rip out columns that we want
    data = data[, city@keepCols]
    colnames(data) = c("startTime","endTime","startLoc","endLoc", "bike","memberType")
    # Convert dates/times to POSIX
    startTime = as.POSIXlt(data$startTime, format=city@timeFormat)
    endTime   = as.POSIXlt(data$endTime, format=city@timeFormat)

    # Pull station data
    stationNames = union(unique(data$startLoc), unique(data$endLoc))

    startLoc = sapply(data$startLoc, FUN = function(x){which(stationNames == x)}, USE.NAMES=FALSE)
    endLoc = sapply(data$endLoc, FUN = function(x){which(stationNames == x)}, USE.NAMES=FALSE)

    # Pull bike data
    bikeNames = unique(data$bike)
    bike = sapply(data$bike, FUN = function(x){which(bikeNames == x)}, USE.NAMES=FALSE)

    # Member types
    memberType = sapply(data$memberType, FUN=function(x){which(city@memberTypes == x)}, USE.NAMES=FALSE)
    bd = BikeshareData(location = city@name, startTime=startTime, endTime=endTime, startLoc = startLoc, endLoc = endLoc, bike=bike, memberType=memberType, stationNames = stationNames)
    bd
}
