visits = getTotalVisits(bd)
expect_equal(nrow(visits), length(stations))
expect_true(sum(visits$visits) > 0)
