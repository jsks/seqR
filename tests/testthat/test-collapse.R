test_that("Collapse function", {
    # Empty data frame
    df <- data.frame()

    expect_identical(collapse(df), df)


    # Simple collapse with numeric types
    df <- data.frame(x = c(0, 0, 1, 1, 1, 3),
                     y = c(2, 2, 2, 1, 1, 1))

    out <- data.frame(x = c(0, 1, 1, 3),
                      y = c(2, 2, 1, 1))
    rownames(out) <- c(1L, 3L, 4L, 6L)
    expect_identical(collapse(df), out)

    # No changes
    df <- data.frame(x = c(3, 3, 1, 2, 0, 3, 4, 4, 2, 4),
                     y = c(2, 0, 2, 1, 0, 1, 0, 1, 0, 0))

    expect_identical(collapse(df), df)

    # Collapse w/ NA
    df <- data.frame(x = c(1, NA, 2, 2, 2, 2),
                     y = c(1, 1, 1, NA, 2, 2))

    out <- data.frame(x = c(1, NA, 2, 2, 2),
                      y = c(1, 1, 1, NA, 2))

    expect_identical(collapse(df), out)

    # Collapse and preserve rownames
    df <- data.frame(x = c(1, 1, 1))
    rownames(df) <- c("a", "b", "c")

    out <- data.frame(x = 1)
    rownames(out) <- "a"

    expect_identical(collapse(df), out)

    # Collapse a mix of types
    df <- data.frame(a = c(1, 1, 2, 2),
                     b = c("a", "a", "b", "b"),
                     c = c(1L, 1L, 2L, 2L),
                     d = c(T, T, F, F),
                     stringsAsFactors = F)

    out <- data.frame(a = c(1, 2),
                      b = c("a", "b"),
                      c = c(1L, 2L),
                      d = c(T, F),
                      stringsAsFactors = F)
    rownames(out) <- c(1L, 3L)

    expect_identical(collapse(df), out)

    # Lastly, lets try to pass in an unsupported column type
    df <- data.frame(x = raw(length = 3))
    expect_error(collapse(df))
})
