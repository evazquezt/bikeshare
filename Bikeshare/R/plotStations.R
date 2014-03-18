## Function to plot station locations on a map

plotStations <- function(stationData,dataSubset=TRUE,zoom=13,col="red",apiKey=NULL){
	
	## convert data object to a dataframe
  if(typeof(dataSubset)=="character"){
    stationsDF <- subset(makeStationDataFrame(stationData),eval(parse(text=dataSubset)))
  }
  if(typeof(dataSubset)=="logical"){
    stationsDF <- subset(makeStationDataFrame(stationData),dataSubset)
  }
	
	locations <- unique(stationsDF[,c("lat","long")])
	center <- c(mean(locations$long),mean(locations$lat))
	
	## plot station locations
	map <- get_map(location=center,zoom=zoom,scale = "auto",source="google",api_key=apiKey)
	mapbg <- ggmap(map)
	
	mapbg + geom_point(data = locations, aes(x = long, y = lat), colour = col, size = 2)
}

