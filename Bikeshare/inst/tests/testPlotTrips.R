stationSubset <- "stationId <= 31020"
## short example
trips = plotTrips(bd,stationSubset=stationSubset,zoom=14,alpha=.3)

expect_true(sum(class(trips) %in% c("gg","ggplot")) == 2)
