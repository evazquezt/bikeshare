readStationData = function(stationPath, city){
    if(identical(city,WAS)){
        doc = xmlTreeParse(file=stationPath)
        stationNodes = doc$doc$children$stations
        
        stations = xmlApply(X=stationNodes, FUN=function(station){
            BikeshareStation(name = xmlValue(station[["name"]]),
                             lat  = as.numeric(xmlValue(station[["lat"]])),
                             long = as.numeric(xmlValue(station[["long"]])),
                             installDate  = as.POSIXlt(as.numeric(xmlValue(station[["installDate"]]))/1000, origin="1970-01-01"),
                             removalDate  = as.POSIXlt(as.numeric(xmlValue(station[["removalDate"]]))/1000, origin="1970-01-01"),
                             stationId = as.numeric(xmlValue(station[["terminalName"]])),
                             numBikes = as.numeric(xmlValue(station[["nbBikes"]])) + as.numeric(xmlValue(station[["nbEmptyDocks"]])))})        
        return(stations)
    }
}

