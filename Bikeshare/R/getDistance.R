## Function to get biking or driving distances over a set of stations

getDistance <- function(stationData,from.subset=TRUE,to.subset=TRUE,mode=c("bicycling","driving"),return.limit=FALSE,api_key=NULL) {
	## Warning: if the subset size is too large, you may exceed the Google API query limit!
	
	## Convert data object to a dataframe
	stations.df <- makeStationDataFrame(stationData)
	from.locations <- unique(subset(stations.df,eval(parse(text=from.subset))))
	to.locations <- unique(subset(stations.df,eval(parse(text=to.subset))))
	
	## Create a dataframe of all from-to station combinations
	loc.combos <- expand.grid(from.locations$stationId,to.locations$stationId)
	names(loc.combos) <- c("from","to")
	
	## Allocate memory for output matrix
	m <- dim(from.locations)[1]
	n <- dim(to.locations)[1]
	distance.mat <- matrix(rep(NA,n*m),nrow=m,ncol=n)
	rownames(distance.mat) <- from.locations$stationId
	colnames(distance.mat) <- to.locations$stationId
	
	## Determine the coordinates of each station
	from.coordinates <- mapply(paste,from.locations$lat, from.locations$long,sep=" ")
	names(from.coordinates) <- from.locations$stationId
	to.coordinates <- mapply(paste,to.locations$lat, to.locations$long,sep=" ")
	names(to.coordinates) <- to.locations$stationId
	coordinates <- c(from.coordinates,to.coordinates) # all relevant coordinates in one vector
	
	## This function gets the coordinates for a given station ID
	get.coord <- function(id){
		paste(unique(coordinates[names(coordinates)==as.character(id)]))
	}
	
	coord.combos <- data.frame(start=sapply(loc.combos[,1],get.coord),
		end=sapply(loc.combos[,	2],get.coord))
	
	## This function is a wrapper of the ggplot mapdist function.  
	## It queries the Google Maps API the biking distance in km between 
	## two latitude-longitude pairs.
	get.dist <- function(coord.df){
		suppressMessages(mapdist(as.character(coord.df[,1]),
			as.character(coord.df[,2]),mode=mode[1],api_key=api_key)$km)
	}
	
	## Obtain final set of distances between each pair of coordinates
	distances <- get.dist(coord.combos)
	if(return.limit){distQueryCheck()} # return remaining available distance queries, if desired
	names(distances) <- mapply(paste,loc.combos$from, loc.combos$to,sep=" ")
	
	## Fill in the distance.mat matrix
	for(i in 1:length(distances)){
		index <- strsplit(names(distances[i]),split=" ")[[1]]
		distance.mat[index[1],index[2]] <- distances[i]
	}
		
	return(distance.mat)
}