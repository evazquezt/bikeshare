readStationData = function(stationPath, city){
    if(identical(city,.cities()$WAS) || identical(city,.cities()$LON)){
        doc = xmlTreeParse(file=stationPath)
        stationNodes = doc$doc$children$stations
        
        stations = xmlApply(X=stationNodes, FUN=function(station){
            BikeshareStation(name = xmlValue(station[["name"]]),
                             lat  = as.numeric(xmlValue(station[["lat"]])),
                             long = as.numeric(xmlValue(station[["long"]])),
                             installDate  = as.POSIXlt(as.numeric(xmlValue(station[["installDate"]]))/1000, origin="1970-01-01"),
                             removalDate  = as.POSIXlt(ifelse(is.null(xmlValue(station[["removalDate"]])), NA,as.numeric(xmlValue(station[["removalDate"]]))/1000), origin="1970-01-01"),
                             stationId = as.numeric(xmlValue(station[["terminalName"]])),
                             numBikes = as.numeric(xmlValue(station[["nbBikes"]])) + as.numeric(xmlValue(station[["nbEmptyDocks"]])))})        
        return(stations)
    }
    if(identical(city,.cities()$CHI)){
        data = read.table(stationPath, header=TRUE,stringsAsFactors=FALSE, quote="\"", sep=",")
        
        stations = apply(X=data,MARGIN=1, FUN=function(x){
            BikeshareStation(name        = x["name"],
                             lat         = as.numeric(x["latitude"]),
                             long        = as.numeric(x["longitude"]),
                             installDate = as.POSIXlt(paste(x["online.date"]), format="%m/%d/%Y"),
                             removalDate = as.POSIXlt(NA),
                             stationId   = as.numeric(x["id"]),
                             numBikes    = as.numeric(x["dpcapacity"]))})        
        return(stations)
    }
    if(identical(city, .cities()$BOS)){
        data = read.csv(stationPath, stringsAsFactors=FALSE, quote="\"")
        stations = apply(X=data,MARGIN=1, FUN=function(x){
            BikeshareStation(name        = x["station"],
                             lat         = as.numeric(x["lat"]),
                             long        = as.numeric(x["lng"]),
                             installDate = as.POSIXlt(paste(x["install_date"]), format="%m/%d/%Y"),
                             removalDate = as.POSIXlt(NA),
                             stationId   = as.numeric(x["id"]),
                             numBikes    = as.numeric(x["nb_docks"]))})        
        return(stations)
    }
}

