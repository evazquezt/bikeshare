stations = readStationData(system.file("extData/bikeStations.xml", package="Bikeshare"),.cities()$WAS)

expect_that(length(stations) > 0, is_true())
expect_that(nrow(makeStationDataFrame(stations)) > 0, is_true())
