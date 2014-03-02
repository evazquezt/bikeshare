install.packages("Bikeshare_0.0.1.tar.gz")
library(Bikeshare)

stations = readStationData("Bikeshare/inst/extData/bikeStations.xml", WAS)

bd = readTripData("Bikeshare/inst/extData/2010-4th-quarter.csv", WAS, stations)

stations.df = makeStationDataFrame(stations)
bd.df = as.data.frame(bd)

# Merge on starting location
merged = merge(bd.df, stations.df, by.y="stationId", by.x ="startLoc")
