library("testthat")
library("Bikeshare")


# Load sample data
stations = readStationData(system.file("extData/bikeStations.xml", package="Bikeshare"),.cities()$WAS)
bd = readTripData(system.file("extData/2010-4th-quarter.csv", package="Bikeshare"), .cities()$WAS, stations)
bd.df = as.data.frame(bd)

test_package("Bikeshare")
