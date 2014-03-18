## Function plotting frequency of travel between stations in a given subset
## More frequently used routes are darker in color and have the highest overlay
## Ignores bikes that are returned to the same station

plotTrips <- function(bikeshareData,trip.subset=TRUE,station.subset=TRUE,zoom=13,col="red",alpha=.2,api_key=NULL){
	
	## Get destination matrix
	destMat <- buildDestinationMatrix(bikeshareData,trip.subset,station.subset)
	
	## Fill in total trips each station pair
	station.pairs <- data.frame(t(combn(rownames(destMat),2)))
	get.trips <- function(stat1,stat2){
		return(destMat[as.character(stat1),as.character(stat2)] +
				destMat[as.character(stat2),as.character(stat1)])
	}
	station.pairs$trips <- mapply(get.trips,station.pairs[,1],station.pairs[,2])
	
	## Sort station pairs by number of trips and drop pairs with 0 trips
	station.pairs <- station.pairs[order(station.pairs$trips),]	
	station.pairs <- station.pairs[station.pairs$trips>0,]
	
	## Look up latitude and longitude of stations
	if(typeof(station.subset)=="character"){
	coords <- subset(makeStationDataFrame(bikeshareData@stations),
					eval(parse(text=station.subset)))[,c("stationId","lat","long")]
	}
	if(typeof(station.subset)=="logical"){
	  coords <- subset(makeStationDataFrame(bikeshareData@stations),
	                   station.subset)[,c("stationId","lat","long")]
	}
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
	
	## Define color palette
	pal0 = colorRampPalette(c("white",col)) 
	col0 = pal0(5)[2] # pick starting color for the final palette, based on user-supplied col
	
	pal <- colorRampPalette(c(col0,col)) # now create palette for final map
	trips <- unique(station.pairs$trips)
	colors <- pal(length(trips))
	
	## Iterate through unique trip counts, updating plot as we go
	for(i in 1:length(trips)){
		data = station.pairs[station.pairs$trips==trips[i],]
		b <- b + geom_segment(data=data,mapping=aes(x=long1, y = lat1, 
					xend = long2, yend = lat2),color=colors[i],alpha=alpha)
	}
	
	## Plot final result
	b
}