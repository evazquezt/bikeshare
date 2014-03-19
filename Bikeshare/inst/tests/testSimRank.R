
r = stationSimRank(bd, 31200)
expect_equal(length(r), length(stations))
expect_equal(sum(r), 1)


