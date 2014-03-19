stationGraph = plotStations(stations)
expect_true(sum(class(stationGraph) %in% c("gg","ggplot")) == 2)
