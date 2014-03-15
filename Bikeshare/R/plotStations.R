## Function to plot station locations on a map

plotStations <- function(stationData,data.subset=TRUE,zoom=13,col="red",api_key=NULL){
	
	## convert data object to a dataframe
	stations.df <- subset(makeStationDataFrame(stationData),data.subset)
	
	locations <- unique(stations.df[,c("lat","long")])
	center <- c(mean(locations$long),mean(locations$lat))
	
	## plot station locations
	map <- get_map(location=center,zoom=zoom,scale = "auto",source="google",api_key)
	mapbg <- ggmap(map)
	
	mapbg + geom_point(data = locations, aes(x = long, y = lat), colour = col, size = 2)
}

