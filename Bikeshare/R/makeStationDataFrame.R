makeStationDataFrame <- function(stations){
    if(length(stations) > 0){
        if(class(stations[[1]]) != "BikeshareStation"){
            stop("Stations must be a list of BikeshareStation objects")
        }else{
            df = data.frame(name = unname(sapply(stations,function(x){x@name})),
                     lat         = unname(sapply(stations,function(x){x@lat})),
                     long        = unname(sapply(stations,function(x){x@long})),
                     stationId   = unname(sapply(stations,function(x){x@stationId})),
                     numBikes    = unname(sapply(stations,function(x){x@numBikes})))

            installDate = rep(stations[[1]]@installDate, length(stations))
            removalDate = rep(stations[[1]]@removalDate, length(stations))
            for(i in 1:length(stations)){
                if(length(stations[[i]]@removalDate) == 0){
                    removalDate[i] = NA
                }else{
                    removalDate[i] = stations[[i]]@removalDate
                }
                installDate[i] = stations[[i]]@installDate
            }
            df$installDate = installDate
            df$removalDate = removalDate
            df
        }
    }else{
        stop("Empty list of stations")
    }
}
