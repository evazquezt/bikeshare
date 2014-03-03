## Function to plot station location on a map

plotStations <- function(data,city,zoom=13){
	
	stations <- readStationData(data,city)
	stations.df <- makeStationDataFrame(stations)
	
	locations <- unique(stations.df[,c("lat","long")])
	center <- c(mean(locations$long),mean(locations$lat))
	
	map <- get_map(location=center,zoom=zoom,source="google")
	mapbg <- ggmap(map,fullpage=TRUE)
	
	mapbg + geom_point(data = locations, aes(x = long, y = lat), colour = "red", size = 2)
}

