expect_true(length(.cities()) > 0)
expect_equal(.cities()$WAS@name, "Washington, DC")
expect_equal(.cities()$CHI@name, "Chicago")
expect_equal(.cities()$BOS@name, "Boston")
