## Function plotting popularity of stations
## Popularity is determined by how many times that station was visited in the dataset, either to pick up or drop off a bike.  (When a bike was picked up AND dropped off at the same station, that station earns +2 visits rather than +1)

plotBubbles <- function(bikeshareData,trip.subset=TRUE,station.subset=TRUE,zoom=13,col="red",alpha=.2,api_key=NULL){
	
	## Get destination matrix
	destMat <- buildDestinationMatrix(bikeshareData,trip.subset,station.subset)

	## Fill in total trips each station pair
	stationPairs <- data.frame(t(combn(rownames(destMat),2)))
	stationPairs <- rbind(stationPairs,data.frame(X1=rownames(destMat),X2=rownames(destMat)))
	
	get.trips <- function(stat1,stat2){
		if(stat1==stat2){return(destMat[as.character(stat1),as.character(stat2)])}
		else{return(destMat[as.character(stat1),as.character(stat2)] +
				destMat[as.character(stat2),as.character(stat1)])}
	}
	stationPairs$trips <- mapply(get.trips,stationPairs[,1],stationPairs[,2])
	
	## Drop pairs with 0 trips
	stationPairs <- stationPairs[stationPairs$trips>0,]
	
	## Sum up visits for each station
	departures <- sqldf("select X1 station, sum(trips) d from stationPairs group by X1")
	arrivals <- sqldf("select X2 station, sum(trips) a from stationPairs group by X2")
	total.visits <- merge(departures,arrivals)
	total.visits <- data.frame(station=total.visits$station,visits=
		as.numeric(as.character(total.visits$a))+as.numeric(as.character(total.visits$d)))

	## Look up latitude and longitude of stations
	coords <- subset(makeStationDataFrame(bikeshareData@stations),
					eval(parse(text=station.subset)))[,c("stationId","lat","long")]
					
	get.lat <- function(id){return(coords$lat[coords$stationId==id])}
	get.long <- function(id){return(coords$long[coords$stationId==id])}
	
	total.visits$lat <- sapply(total.visits$station,get.lat)
	total.visits$long <- sapply(total.visits$station,get.long)
	
	## Get background map
	center <- c(mean(total.visits$long),mean(total.visits$lat))
	map <- get_map(location=center,zoom=zoom,scale = "auto",source="google",api_key=api_key)
	b <- ggmap(map)
	
	## Plot results
	b + geom_point(data=total.visits, aes(x=long,y=lat,size=visits),colour=col,alpha=alpha)
}
