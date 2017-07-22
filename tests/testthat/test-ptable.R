test_that("Testing percentage tables", {
    df <- data.frame(x = c(3, 4),
                     y = c(1, 1),
                     z = c(4, 3))
    out <- data.frame(X = c("x", "x", "z"), Y = c("y", "z", "y"),
                      "X>Y" = c(100, 50, 100), "X=Y" = c(0, 0, 0), "X<Y" = c(0, 50, 0),
                      stringsAsFactors = F, check.names = F)

    expect_identical(ptable(df), out)

    
    df <- data.frame(x = c(0, 2, 1, 0, 2, 2, 2, 1, 3, 0),
                     y = c(2, 4, 3, 2, 4, 2, 1, 2, 3, 3))

    out <- data.frame(X = "y", Y = "x", "X>Y" = 70, "X=Y" = 20, "X<Y" = 10,
                      stringsAsFactors = F, check.names = F)
    expect_identical(ptable(df), out)

    df <- data.frame(x = c(NA, 2), y = c(3, 2))
    out <- data.frame(X = "x", Y = "y", "X>Y" = 0, "X=Y" = 100, "X<Y" = 0,
                      stringsAsFactors = F, check.names = F)
    out2 <- data.frame(X = "x", Y = "y", "X>Y" = 0, "X=Y" = 50, "X<Y" = 0,
                       stringsAsFactors = F, check.names = F)
    expect_identical(ptable(df), out)
    expect_identical(ptable(df, complete.obs = F), out2)

    df <- data.frame(x = NA, y = 1)
    out <- data.frame(X = "x", Y = "y", "X>Y" = 0, "X=Y" = 0, "X<Y" = 0,
                      stringsAsFactors = F, check.names = F)
    expect_identical(ptable(df), out)

    expect_identical(ptable(data.frame()), data.frame())
    expect_identical(ptable(data.frame(x = 1)), data.frame())
    expect_error(ptable(matrix()), "Expects a data frame")
                     
})
