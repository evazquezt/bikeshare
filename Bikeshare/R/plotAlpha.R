## Function plotting all routes taken between stations in a given subset as an alpha-blended map
## Ignores bikes that are returned to the same station, since that won't show up on the map

plotAlpha <- function(bikeshareData,tripSubset=TRUE,stationSubset=TRUE,zoom=13,col="red",alpha=.2,apiKey=NULL){

	## Get total number of trips in tripSubset between each station pair in stationSubset, 
  ## not including trips from a station to itself
	stationPairs <- getTotalTrips(bikeshareData,tripSubset,stationSubset)
	## Drop pairs with 0 trips and pairs from a station to itself
	stationPairs <- stationPairs[stationPairs$trips>0,]
	stationPairs <- stationPairs[stationPairs[,1]!=stationPairs[,2],]
	
	## Look up latitude and longitude of stations
	coords <- subset(makeStationDataFrame(bikeshareData@stations),
					eval(parse(text=stationSubset)))[,c("stationId","lat","long")]
	
	getLat <- function(id){return(coords$lat[coords$stationId==id])}
	getLong <- function(id){return(coords$long[coords$stationId==id])}
	
	stationPairs$lat1 <- sapply(stationPairs$X1,getLat)
	stationPairs$long1 <- sapply(stationPairs$X1,getLong)
	stationPairs$lat2 <- sapply(stationPairs$X2,getLat)
	stationPairs$long2 <- sapply(stationPairs$X2,getLong)
	
	## Get background map
	center <- c(mean(c(stationPairs$long1, stationPairs$long2)),
				mean(c(stationPairs$lat1, stationPairs$lat2)))
	map <- get_map(location=center,zoom=zoom,scale = "auto",source="google",api_key=apiKey)
	b <- ggmap(map)
	
	## Plot alpha-blended segments
	b + geom_segment(data=stationPairs,mapping=aes(x=long1, y = lat1, 
			xend = long2, yend = lat2),color=col,alpha=alpha)
}