## Function to plot station locations on a map

plotStations <- function(station.data.object,sbst=TRUE,zoom=13,col="red",api_key){
	
	## convert data object to a dataframe
	stations.df <- subset(makeStationDataFrame(station.data.object),sbst)
	
	locations <- unique(stations.df[,c("lat","long")])
	center <- c(mean(locations$long),mean(locations$lat))
	
	## plot station locations
	map <- get_map(location=center,zoom=zoom,scale = "auto",source="google",api_key)
	mapbg <- ggmap(map)
	
	mapbg + geom_point(data = locations, aes(x = long, y = lat), colour = col, size = 2)
}

