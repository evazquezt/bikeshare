stationSubset <- "stationId <= 31010"
alpha = plotAlpha(bd,stationSubset=stationSubset,zoom=14)

expect_true(sum(class(alpha) %in% c("gg","ggplot")) == 2)
