# Make sure we have rows
expect_true(nrow(bd.df) > 0)
expect_true(sum(bd.df$startLoc == 31111) > 10)

