## Function to get biking distances over a set of stations

### need to finish debugging and run once google lets me run stuff again...

getBikingDistance <- function(station.data.object,sbst=TRUE) {
	
	## convert data object to a dataframe
	stations.df <- subset(makeStationDataFrame(station.data.object),sbst)
	locations <- unique(stations.df[,c("stationId","lat","long")])
	
	n <- dim(locations)[1]
	loc.combos <- as.data.frame(t(combn(locations$stationId,2)))
	names(loc.combos) <- c("start","end")
	#loc.list <- list(rep(NA,dim(loc.combos)[1]))
#	for(i in 1:dim(loc.combos)[1]){
	#	loc.list[[i]] <- loc.combos[i,]
	#}
	
	## allocate memory for output matrix
	distance.mat <- matrix(rep(NA,n^2),nrow=n,ncol=n)
	rownames(distance.mat) <- locations$stationId
	colnames(distance.mat) <- locations$stationId
	
	coordinates <- mapply(paste,locations$lat, locations$long,sep=" ")
	names(coordinates) <- locations$stationId
	
	
	get.coord <- function(coord){
		paste(coordinates[names(coordinates)==as.character(coord)])
	}
	
	coord.combos <- data.frame(start=sapply(loc.combos[,1],get.coord),end=sapply(loc.combos[,	2],get.coord))
	
	get.dist <- function(coord.df){
		suppressMessages(mapdist(as.character(coord.df[,1]),as.character(coord.df[,2]),mode="bicycling")$km)
	}
	
	
	#mapdist(as.character(coord.df[,1]),as.character(coord.df[,2]),mode="bicycling",override_limit=TRUE)
	
	distances <- get.dist(coord.combos)
	
	## get station distances
	#get.coord <- function(elem){
	#	coor1 <- coordinates[names(coordinates)==as.character(elem[1])]
	#	coor2 <- coordinates[names(coordinates)==as.character(elem[2])]
	#	paste(as.character(coor1),as.character(coor2),sep=":")
	#}
	
	## compute combinations of station coordinates
	#coord.combos <- sapply(loc.list,get.coord)




	#get.dist <- function(coord.string) {
	#	coords <- strsplit(coord.string,":")[[1]]
	#	suppressMessages(mapdist(as.character(coords[1]),as.character(coords[2]),mode="bicycling")$km)
	#}
	
	distances <- sapply(coord.combos,get.dist)
	
}