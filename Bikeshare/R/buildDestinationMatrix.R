buildDestinationMatrix <-  function(bikeshareData,tripSubset=TRUE,stationSubset=TRUE){
    if(class(bikeshareData) != "BikeshareData"){
        stop("Argument must be of class BikeshareData")
    }
    
    ## Subset data per argument
  	if(typeof(tripSubset)=="character"){
     	 	bdDF = subset(as.data.frame(bikeshareData),eval(parse(text=tripSubset)))
     	}
    if(typeof(tripSubset)=="logical"){
     	 	bdDF = subset(as.data.frame(bikeshareData),tripSubset)
  	}
  	if(typeof(stationSubset)=="character"){
     		stationDF = subset(makeStationDataFrame(bikeshareData@stations),eval(parse(text=stationSubset)))
     	}
    if(typeof(stationSubset)=="logical"){
     		stationDF = subset(makeStationDataFrame(bikeshareData@stations),stationSubset)	
     	}   	
  	
  	bdDF$trip = rep(1, nrow(bdDF))
  	n = nrow(stationDF)
	
    # Melt data by starting and ending stations
    melted = melt(data=bdDF, id.vars = c("startLoc","endLoc"), measure.vars="trip")
    # Aggregate by trip
    counts = cast(data=melted,  startLoc+endLoc ~.  , fun.aggregate=count)

    # Build a matrix
    destinationMatrix = matrix(rep(0, n*n), n, n)
    stationIds = stationDF$stationId
    colnames(destinationMatrix) = stationIds
    rownames(destinationMatrix) = stationIds
    # Tally all of the counts into matrix
    for(i in 1:nrow(counts)){
        destinationMatrix[stationIds == counts[i, "startLoc"], stationIds == counts[i, "endLoc"]] = counts[i, "freq"]
    }

    destinationMatrix
}
