## Function plotting popularity of stations
## Popularity is determined by how many times that station was visited in the dataset, either to pick up or drop off a bike.  (When a bike was picked up AND dropped off at the same station, that station earns +2 visits rather than +1)

plotBubbles <- function(bikeshareData,tripSubset=TRUE,stationSubset=TRUE,weighted=FALSE,zoom=13,col="red",alpha=.2,apiKey=NULL){
	
	## Get total number of visits in tripSubset to each station in stationSubset
  totalVisits <- getTotalVisits(bikeshareData,tripSubset,stationSubset,TRUE)
  
	## Store latitude, longitude, and number of bikes at stations in a coordinates ("coords") data frame
	if(typeof(stationSubset)=="character"){
	  coords <- subset(makeStationDataFrame(bikeshareData@stations),
	                   eval(parse(text=stationSubset)))[,c("stationId","numBikes","lat","long")]
	}
	if(typeof(stationSubset)=="logical"){
	  coords <- subset(makeStationDataFrame(bikeshareData@stations),
	                   stationSubset)[,c("stationId","numBikes","lat","long")]
	}
  
  ## Define functions for looking up latitude, longitude and number of bikes
	getLat <- function(id){return(coords$lat[coords$stationId==id])}
	getLong <- function(id){return(coords$long[coords$stationId==id])}
  getBikes <- function(id){return(coords$numBikes[coords$stationId==id])}
	
  ## Fill in latitude and longitude for each station in totalVisits
	totalVisits$lat <- sapply(totalVisits$station,getLat)
	totalVisits$long <- sapply(totalVisits$station,getLong)
	
	## Get background map
	center <- c(mean(totalVisits$long),mean(totalVisits$lat))
	map <- get_map(location=center,zoom=zoom,scale = "auto",source="google",api_key=apiKey)
	b <- ggmap(map)
	
	## Plot results
  if(weighted==FALSE){ # don't weight visits by number of bike slots
	  b <- b + geom_point(data=totalVisits, aes(x=long,y=lat,size=visits),colour=col,alpha=alpha)
  }
  if(weighted==TRUE){ # weight visits by number of bike slots
    totalVisits$numBikes <- sapply(totalVisits$station,getBikes)
    totalVisits$weights <- totalVisits$visits/totalVisits$numBikes
    b <- b + geom_point(data=totalVisits, aes(x=long,y=lat,size=weights),colour=col,alpha=alpha)
  }
  
  b
}
