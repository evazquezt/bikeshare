## Function plotting all routes taken between stations in a given subset as an alpha-blended map
## Ignores bikes that are returned to the same station, since that won't show up on the map

plotAlpha <- function(bikeshareData,trip.subset=TRUE,station.subset=TRUE,zoom=13,col="red",alpha=.2,api_key=NULL){
	
	## Get destination matrix
	destMat <- buildDestinationMatrix(bikeshareData,trip.subset,station.subset)
	
	## Fill in total trips each station pair
	station.pairs <- data.frame(t(combn(rownames(destMat),2)))
	get.trips <- function(stat1,stat2){
		return(destMat[as.character(stat1),as.character(stat2)] +
				destMat[as.character(stat2),as.character(stat1)])
	}
	station.pairs$trips <- mapply(get.trips,station.pairs[,1],station.pairs[,2])
	
	## Drop pairs with 0 trips
	station.pairs <- station.pairs[station.pairs$trips>0,]
	
	## Look up latitude and longitude of stations
	coords <- subset(makeStationDataFrame(bikeshareData@stations),
					eval(parse(text=station.subset)))[,c("stationId","lat","long")]
	
	get.lat <- function(id){return(coords$lat[coords$stationId==id])}
	get.long <- function(id){return(coords$long[coords$stationId==id])}
	
	station.pairs$lat1 <- sapply(station.pairs$X1,get.lat)
	station.pairs$long1 <- sapply(station.pairs$X1,get.long)
	station.pairs$lat2 <- sapply(station.pairs$X2,get.lat)
	station.pairs$long2 <- sapply(station.pairs$X2,get.long)
	
	## Get background map
	center <- c(mean(c(station.pairs$long1, station.pairs$long2)),
				mean(c(station.pairs$lat1, station.pairs$lat2)))
	map <- get_map(location=center,zoom=zoom,scale = "auto",source="google",api_key=api_key)
	b <- ggmap(map)
	
	## Plot alpha-blended segments
	b + geom_segment(data=station.pairs,mapping=aes(x=long1, y = lat1, 
			xend = long2, yend = lat2),color=col,alpha=alpha)
}