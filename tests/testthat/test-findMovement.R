test_that("Finding subsequences", {
    x <- c(4, 1, 0, 0, NA, 0, 0, 2, 2, 2, 2, 3, 3, 3, 3, 1, 2)

    expect_equal(findMovement(x), c(rep(NA, 6), rep(1, 6), rep(NA, 3), rep(2, 2)))
    expect_equal(findMovement(x, direction = "down"),
                 c(rep(1, 3),rep(NA, 11), rep(2, 2), NA))


    expect_equal(findMovement(x, upper_lim = 3),
                 c(rep(NA, 6), rep(1, 6), rep(NA, 5)))
    expect_equal(findMovement(x, upper_lim = 1),
                 c(rep(NA, 6), rep(1, 6), rep(NA, 3), rep(2, 2)))
    expect_equal(findMovement(x, lower_lim = 1),
                 c(rep(NA, 6), rep(1, 6), rep(NA, 3), rep(2, 2)))
    expect_equal(findMovement(x, lower_lim = 0),
                 c(rep(NA, 6), rep(1, 6), rep(NA, 5)))
    expect_equal(findMovement(x, upper_lim = 3, lower_lim = 1),
                 c(rep(NA, 6), rep(1, 6), rep(NA, 5)))
    expect_equal(findMovement(x, upper_lim = 4, lower_lim = 1),
                 as.numeric(rep(NA, 17)))

    expect_equal(findMovement(x, buffer = 1),
                 c(rep(NA, 5), rep(1, 8), rep(NA, 2), rep(2, 2)))
    expect_equal(findMovement(x, buffer = 2),
                 c(rep(NA, 5), rep(1, 9), rep(NA, 1), rep(2, 2)))
    expect_equal(findMovement(x, buffer = 10),
                 c(rep(NA, 5), rep(1, 10), rep(2, 2)))

    expect_equal(findMovement(x, buffer = 1, direction = "down"),
                 c(rep(1, 4), rep(NA, 9), rep(2, 3), NA))
    expect_equal(findMovement(x, buffer = 10, direction = "down"),
                 c(rep(1, 4), rep(NA, 7), rep(2, 5), NA))

    expect_equal(findMovement(x, lbuffer = 1),
                 c(rep(NA, 5), rep(1, 7), rep(NA, 3), rep(2, 2)))
    expect_equal(findMovement(x, direction = "down", rbuffer = 1),
                 c(rep(1, 3), rep(NA, 10), rep(2, 3), NA))

    expect_error(findMovement(x, direction = "foo"), "Invalid direction argument")
    expect_error(findMovement(x, buffer = -1), "Buffer values cannot be less than 0")

    expect_equal(findMovement(c(1, 1, 1, 1)), as.numeric(rep(NA, 4)))
    expect_equal(findMovement(c(NA, 1, NA)), as.numeric(rep(NA, 3)))
    expect_equal(findMovement(c(1, NA, 2, NA, 2)), as.numeric(rep(NA, 5)))

    expect_error(findMovement(c("a", "b", "c")))
    expect_equal(findMovement(c(4, NULL, 2), direction = "down"), c(1, 1))

    y <- c(16.1204090, 11.7990691, 12.0038573, 10.5534136,  7.2207943, 18.9345657,
           12.4892524,  0.1669142, 13.5067795,  7.6360430)
    expect_equal(findMovement(y, buffer = 2),
                 c(NA, rep(1, 2), NA, rep(2, 2), NA, rep(3, 2), NA))
    expect_equal(findMovement(y, direction = "down", buffer = 2),
                 c(rep(1, 2), rep(2, 3), rep(3, 3), rep(4, 2)))
})

    
