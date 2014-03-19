fromSubset <- "stationId <= 31001"
toSubset <- "numBikes <= 10"
dist = getDistance(stations,fromSubset,toSubset,mode="bicycling")

expect_equal(nrow(dist), 2)
expect_true(dist[1,1] > 0)
