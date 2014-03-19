readTripData <-  function(path, city, stations){
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
    
    colnames(data)[1:6] = c("startTime","endTime","startLoc","endLoc", "bike","memberType")

    
    # Do city-level cleaning
    if(identical(city, .cities()$WAS)){        
        # Capital Bikeshare data comes in with station id in station name.  Strip it out
        data$startLoc = suppressWarnings(as.integer(sapply(data$startLoc, function(x){substr(x, nchar(x)-5, nchar(x)-1)}, USE.NAMES=FALSE)))
        data$endLoc = suppressWarnings(as.integer(sapply(data$endLoc, function(x){substr(x, nchar(x)-5, nchar(x)-1)}, USE.NAMES=FALSE)))
        # Kill the rows that don't have start/end locs
        data = subset(data,!(is.na(startLoc) | is.na(endLoc)))
    }
    if(identical(city, .cities()$LON)){
        data$memberType = rep("", nrow(data))
        data$startTime = paste(data$startTime, data$Start.Time)
        data$endTime = paste(data$endTime, data$End.Time)
        data=data[,1:6]
    }
    
    
    # Convert dates/times to POSIX
    startTime = as.POSIXlt(data$startTime, format=city@timeFormat)
    endTime   = as.POSIXlt(data$endTime, format=city@timeFormat)
               
    # Pull bike data
    bikeNames = unique(data$bike)
    bike = sapply(data$bike, FUN = function(x){which(bikeNames == x)}, USE.NAMES=FALSE)
    
    # Member types
    memberType = sapply(data$memberType, FUN=function(x){which(city@memberTypes == x)}, USE.NAMES=FALSE)
    # London doesn't do member types
    if(identical(city, .cities()$LON)){
        memberType = rep(-1L, nrow(data))
    }
    bd = BikeshareData(location = city@name, startTime=startTime, endTime=endTime, startLoc = data$startLoc, endLoc = data$endLoc, bike=bike, memberType=memberType, stations = stations)
}
