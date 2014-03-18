destinationMatrix = buildDestinationMatrix(bd)

expect_equal(nrow(destinationMatrix), length(stations))
expect_equal(nrow(destinationMatrix), length(stations))
expect_true(sum(destinationMatrix[,1]) > 0)
