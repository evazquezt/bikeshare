buildDestinationMatrix <-  function(bikeshareData){
    if(class(bikeshareData) != "BikeshareData"){
        stop("Argument must be of class BikeshareData")
    }
    
    bd.df = as.data.frame(bikeshareData)
    bd.df$trip = rep(1, nrow(bd.df))
    stations.df = makeStationDataFrame(bikeshareData@stations)
    n = nrow(stations.df)

    # Melt data by starting and ending stations
    melted = melt(data=bd.df, id.vars = c("startLoc","endLoc"), measure.vars="trip")
    # Aggregate by trip
    counts = cast(data=melted,  startLoc+endLoc ~.  , fun.aggregate=count)

    # Build a matrix
    destinationMatrix = matrix(rep(0, n*n), n, n)
    stationIds = stations.df$stationId
    colnames(destinationMatrix) = stationIds
    rownames(destinationMatrix) = stationIds
    # Tally all of the counts
    for(i in 1:nrow(counts)){
        destinationMatrix[stationIds == counts[i, "startLoc"], stationIds == counts[i, "endLoc"]] = counts[i, "freq"]
    }

    destinationMatrix
}
