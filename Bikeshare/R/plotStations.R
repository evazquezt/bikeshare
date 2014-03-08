## Function to plot station location on a map

####### need to update Rd file

plotStations <- function(station.data.object,sbst=TRUE,zoom=13,col="red"){
	
	## convert data object to a dataframe
	stations.df <- subset(makeStationDataFrame(station.data.object),sbst)
	
	locations <- unique(stations.df[,c("lat","long")])
	center <- c(mean(locations$long),mean(locations$lat))
	
	## plot station locations
	map <- get_map(location=center,zoom=zoom,source="google")
	mapbg <- ggmap(map,fullpage=TRUE)
	
	mapbg + geom_point(data = locations, aes(x = long, y = lat), colour = col, size = 2)
}

