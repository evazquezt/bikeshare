install.packages("Bikeshare_0.0.1.tar.gz")
library(Bikeshare)

stations = readStationData(system.file("extData/bikeStations.xml", package="Bikeshare"),.cities()$WAS)

bd = readTripData(system.file("extData/2010-4th-quarter.csv", package="Bikeshare"), .cities()$WAS, stations)

stations.df = makeStationDataFrame(stations)
bd.df = as.data.frame(bd)

# Merge on starting location
merged = merge(bd.df, stations.df, by.y="stationId", by.x ="startLoc")

# Let's analyze  "Massachusetts Ave & Dupont Circle NW"
station = stations.df[stations.df$name == "Massachusetts Ave & Dupont Circle NW",]

# Pull out only trips that started there
data = bd.df[bd.df$startLoc == station$stationId,]
# Figure out where people went
destinations = unique(data$endLoc)
# Count number of times going to each destination
counts = sapply(destinations, FUN=function(x){sum(data$endLoc == as.integer(x), na.rm=TRUE)}, USE.NAMES=FALSE)

cbind(destinations, counts)

bd.df[is.na(bd.df$endLoc),]

