## Function plotting frequency of travel between stations in a given subset
## More frequently used routes are darker in color and have the highest overlay
## Ignores bikes that are returned to the same station

plotTrips <- function(bikeshareData,tripSubset=TRUE,stationSubset=TRUE,zoom=13,col="red",alpha=.2,apiKey=NULL){
	
	## Get total number of trips in tripSubset between each station pair in stationSubset, 
  ## not including trips from a station to itself
	stationPairs <- getTotalTrips(bikeshareData,tripSubset,stationSubset)
  
	## Sort station pairs by total number of trips and drop pairs with 0 trips and pairs from a station to itself
	stationPairs <- stationPairs[order(stationPairs$trips),]	
	stationPairs <- stationPairs[stationPairs$trips>0,]
	stationPairs <- stationPairs[stationPairs[,1]!=stationPairs[,2],]
	
	## Store latitude, longitude, and number of bikes at stations in a coordinates ("coords") data frame
	if(typeof(stationSubset)=="character"){
	coords <- subset(makeStationDataFrame(bikeshareData@stations),
					eval(parse(text=stationSubset)))[,c("stationId","lat","long")]
	}
	if(typeof(stationSubset)=="logical"){
	  coords <- subset(makeStationDataFrame(bikeshareData@stations),
	                   stationSubset)[,c("stationId","lat","long")]
	}
	## Define functions for looking up latitude and longitude
	getLat <- function(id){return(coords$lat[coords$stationId==id])}
	getLong <- function(id){return(coords$long[coords$stationId==id])}
	
	## Fill in latitude and longitude for each pair in stationPairs
	stationPairs$lat1 <- sapply(stationPairs$X1,getLat)
	stationPairs$long1 <- sapply(stationPairs$X1,getLong)
	stationPairs$lat2 <- sapply(stationPairs$X2,getLat)
	stationPairs$long2 <- sapply(stationPairs$X2,getLong)
	
	## Get background map
	center <- c(mean(c(stationPairs$long1, stationPairs$long2)),
				mean(c(stationPairs$lat1, stationPairs$lat2)))
	map <- get_map(location=center,zoom=zoom,scale = "auto",source="google",api_key=apiKey)
	b <- ggmap(map)
	
	## Define color palette
	pal0 = colorRampPalette(c("white",col)) 
	col0 = pal0(5)[2] # pick starting color for the final palette, based on user-supplied col
	
	pal <- colorRampPalette(c(col0,col)) # now create palette for final map
	trips <- unique(stationPairs$trips)
	colors <- pal(length(trips))
	
	## Iterate through unique trip counts, updating plot as we go
	for(i in 1:length(trips)){
		data = stationPairs[stationPairs$trips==trips[i],]
		b <- b + geom_segment(data=data,mapping=aes(x=long1, y = lat1, 
					xend = long2, yend = lat2),color=colors[i],alpha=alpha)
	}
	
	## Plot final result
	b
}