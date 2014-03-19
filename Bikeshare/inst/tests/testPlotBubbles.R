stationSubset <- "stationId <= 31020"
suppressWarnings(bub = plotBubbles(bd,stationSubset=stationSubset,zoom=14,alpha=.5))

expect_true(sum(class(bub) %in% c("gg","ggplot")) == 2)
