stationSimRank <- function(bikeshareData, station, tripSubset=TRUE, beta=0.8, errorTol = 1e-4){ 
    destinations = buildDestinationMatrix(bikeshareData=bikeshareData, tripSubset=tripSubset)
    
    # See http://www.stanford.edu/class/cs246/slides/10-spam.pdf
    M = t(destinations)
    # Remove deadends
    M[,colSums(M) == 0] = rep(1/nrow(M), nrow(M))
    # Convert into a column stochastic matrix    
    for(i in 1:ncol(M)){
        M[,i] = M[,i] / sum(M[,i])
    }

    # Initialize r
    r = rep(1/nrow(M), nrow(M))

    # In SimRank, random teleport only to start node
    tele = rep(0, nrow(M))
    tele[which(rownames(destinations) == station)] = 1

    # Use power iteration to efficiently derive r
    rOld = 1e10
    while(sum(abs(rOld - r)) > errorTol){
        rOld = r
        r = beta * M%*% r + (1-beta)*r
    }    
    
    r
    
}
