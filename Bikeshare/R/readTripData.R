readTripData <-  function(path, city, stations){
    library(stringr)
    # Allow user to enter String format for city
    if(class(city) == "character"){
        city = get(city)
        if(class(city) != "BikeshareCity"){
            stop("City must be a valid Bikeshare city")
        }
    }
    
    data = read.table(path, sep=city@delim, header=city@header, stringsAsFactors=FALSE, quote="\"")
    
    # Rip out columns that we want
    data = data[, city@keepCols]
    colnames(data) = c("startTime","endTime","startLoc","endLoc", "bike","memberType")
    
    # Do city-level cleaning
    if(identical(city, WAS)){
        # Capital Bikeshare data comes in with station id in station name.  Strip it out
        data$startLoc = as.integer(gsub("[\\(\\)]","",sapply(data$startLoc, function(x){str_extract_all(x, "\\(([0-9]+)\\)")[[1]]})))
        data$endLoc   = as.integer(gsub("[\\(\\)]","",sapply(data$endLoc, function(x){str_extract_all(x, "\\(([0-9]+)\\)")[[1]]})))
    }
    # Similarly deal with other start/end locs
    
    # Convert dates/times to POSIX
    startTime = as.POSIXlt(data$startTime, format=city@timeFormat)
    endTime   = as.POSIXlt(data$endTime, format=city@timeFormat)
               
    # Pull bike data
    bikeNames = unique(data$bike)
    bike = sapply(data$bike, FUN = function(x){which(bikeNames == x)}, USE.NAMES=FALSE)
    
    # Member types
    memberType = sapply(data$memberType, FUN=function(x){which(city@memberTypes == x)}, USE.NAMES=FALSE)    
    bd = BikeshareData(location = city@name, startTime=startTime, endTime=endTime, startLoc = data$startLoc, endLoc = data$endLoc, bike=bike, memberType=memberType, stations = stations)
}
