## Function plotting frequency of travel between stations in a given subset
## More frequently used routes are darker in color and have the highest overlay
## Ignores bikes that are returned to the same station

getTotalTrips <- function(bikeshareData,tripSubset=TRUE,stationSubset=TRUE){
  
  ## Get destination matrix
  destMat <- buildDestinationMatrix(bikeshareData,tripSubset,stationSubset)
  
  ## Fill in total trips for each station pair
  stationPairs <- data.frame(t(combn(rownames(destMat),2)))
  stationPairs <- rbind(stationPairs,data.frame(X1=rownames(destMat),X2=rownames(destMat)))
  
  getTrips <- function(id1,id2){
    if(as.character(id1)==as.character(id2)){return(destMat[as.character(id1),as.character(id2)])}
    else{return(destMat[as.character(id1),as.character(id2)] +
                  destMat[as.character(id2),as.character(id1)])}
  }
  stationPairs$trips <- mapply(getTrips,stationPairs[,1],stationPairs[,2])
  
  ## Convert stationPairs first two columns to character vectors
  stationPairs[,1] <- as.character(stationPairs[,1])
  stationPairs[,2] <- as.character(stationPairs[,2])
  return(stationPairs)
}