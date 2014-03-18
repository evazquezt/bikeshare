getTotalVisits <- function(bikeshareData,tripSubset=TRUE,stationSubset=TRUE,includeSelf=TRUE){
	
  ## Get total number of trips in tripSubset between each station pair in stationSubset
  stationPairs <- getTotalTrips(bikeshareData,tripSubset,stationSubset)
  
	## Drop pairs with 0 trips
	stationPairs <- stationPairs[stationPairs$trips>0,]
  
  ## If includeSelf==FALSE, drop trips from a station to itself
  if(includeSelf==FALSE){stationPairs <- stationPairs[stationPairs[,1]!=stationPairs[,2]]}
	
	## Sum up visits for each station
	departures <- sqldf("select X1 station, sum(trips) d from stationPairs group by X1")
	arrivals <- sqldf("select X2 station, sum(trips) a from stationPairs group by X2")
	totalVisits <- merge(departures,arrivals)
	totalVisits <- data.frame(station=totalVisits$station,visits=
	                            as.numeric(as.character(totalVisits$a))+as.numeric(as.character(totalVisits$d)))
  
	return(totalVisits)
}