## Function to get biking or driving distances over a set of stations

getDistance <- function(stationData,fromSubset=TRUE,toSubset=TRUE,mode=c("bicycling","driving"),returnLimit=FALSE) {
	## Warning: if the subset size is too large, you may exceed the Google API query limit!
	
	## Convert data object to a dataframe
	stationDF <- makeStationDataFrame(stationData)
	if(typeof(fromSubset)=="character"){
	  fromLocations <- unique(subset(stationDF,eval(parse(text=fromSubset))))
	}
	if(typeof(fromSubset)=="logical"){
	  fromLocations <- unique(subset(stationDF,fromSubset))
	}
	if(typeof(toSubset)=="character"){
	  toLocations <- unique(subset(stationDF,eval(parse(text=toSubset))))
	}
	if(typeof(toSubset)=="logical"){
	  toLocations <- unique(subset(stationDF,toSubset))
	}
	
	## Create a dataframe of all from-to station combinations
	locCombos <- expand.grid(fromLocations$stationId,toLocations$stationId)
	names(locCombos) <- c("from","to")
	
	## Allocate memory for output matrix
	m <- dim(fromLocations)[1]
	n <- dim(toLocations)[1]
	distanceMat <- matrix(rep(NA,n*m),nrow=m,ncol=n)
	rownames(distanceMat) <- fromLocations$stationId
	colnames(distanceMat) <- toLocations$stationId
	
	## Determine the coordinates of each station
	fromCoordinates <- mapply(paste,fromLocations$lat, fromLocations$long,sep=" ")
	names(fromCoordinates) <- fromLocations$stationId
	toCoordinates <- mapply(paste,toLocations$lat, toLocations$long,sep=" ")
	names(toCoordinates) <- toLocations$stationId
	coordinates <- c(fromCoordinates,toCoordinates) # all relevant coordinates in one vector
	
	## This function gets the coordinates for a given station ID
	getCoord <- function(id){
		paste(unique(coordinates[names(coordinates)==as.character(id)]))
	}
	
	coordCombos <- data.frame(start=sapply(locCombos[,1],getCoord),
		end=sapply(locCombos[,2],getCoord))
	
	## This function is a wrapper of the ggplot mapdist function.  
	## It queries the Google Maps API the biking distance in km between 
	## two latitude-longitude pairs.
	getDist <- function(df){
		
		## we need to break things up into groups of 50 queries so as to work with mapdist
		groups <- 1:dim(df)[1]
		groups <- (groups %/% 50) + 1
		indices <- c(0, cumsum(table(groups)))
		indexList <- as.list(rep(NA,length(indices)-1))
		for(i in 1:(length(indices)-1)){
			indexList[[i]] <- (indices[i]+1):indices[i+1]
		}
		
		## Now run the distance search over each 50-element subgroup
		dists <- rep(NA,dim(df)[1]) # allocate memory
		for(i in 1:max(groups)){
			froms <- df[groups==i,1]
			tos <- df[groups==i,2]
			dists[indexList[[i]]] <- suppressMessages(mapdist(as.character(froms),
				as.character(tos),mode=mode[1])$km)
		}
		return(dists)
	}
	
	## Obtain final set of distances between each pair of coordinates
	distances <- getDist(coordCombos)
	if(returnLimit){distQueryCheck()} # return remaining available distance queries, if desired
	names(distances) <- mapply(paste,locCombos$from, locCombos$to,sep=" ")
	
	## Fill in the distanceMat matrix
	for(i in 1:length(distances)){
		index <- strsplit(names(distances[i]),split=" ")[[1]]
		distanceMat[index[1],index[2]] <- distances[i]
	}
		
	return(distanceMat)
}