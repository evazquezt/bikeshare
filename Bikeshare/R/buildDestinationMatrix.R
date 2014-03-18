buildDestinationMatrix <-  function(bikeshareData,trip.subset=TRUE,station.subset=TRUE){
    if(class(bikeshareData) != "BikeshareData"){
        stop("Argument must be of class BikeshareData")
    }
    
    ## Subset data per argument
	if(typeof(trip.subset)=="character"){
   	 	bd.df = subset(as.data.frame(bikeshareData),eval(parse(text=trip.subset)))
   	}
   	if(typeof(trip.subset)=="logical"){
   	 	bd.df = subset(as.data.frame(bikeshareData),trip.subset)
	}
	if(typeof(station.subset)=="character"){
   		stations.df = subset(makeStationDataFrame(bikeshareData@stations),eval(parse(text=station.subset)))
   	}
   	if(typeof(station.subset)=="logical"){
   		stations.df = subset(makeStationDataFrame(bikeshareData@stations),station.subset)	
   	}   	
	
	bd.df$trip = rep(1, nrow(bd.df))
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
    # Tally all of the counts into matrix
    for(i in 1:nrow(counts)){
        destinationMatrix[stationIds == counts[i, "startLoc"], stationIds == counts[i, "endLoc"]] = counts[i, "freq"]
    }

    destinationMatrix
}
