ql_mat <- function(o) {
    m <- as.matrix(o) %>% structure(class = c("ql_mat", class(.)))
    storage.mode(m) <- "double"

    m
}

test_that("ql_matrix fuction", {
    df <- data.frame(x = c(0, 0, 1),
                     y = c(3, 1, 4),
                     z = c(1, 1, 1))
    qlm <- ql_matrix(df, "x")

    expect_s3_class(qlm, "ql_mat")
    expect_true(is.ql_mat(qlm))

    o <- read.csv2("ql_matrix_x.csv", row.names = 1) %>% ql_mat
    expect_identical(qlm, o)

    o <- read.csv2("ql_matrix_xy.csv", row.names = 1) %>% ql_mat
    expect_identical(ql_matrix(df, c("x", "y")), o)

    df <- data.frame(x = c(0, 0, 1, 1, 1, 1, 1, 1),
                     y = c(3, 0, 0, 0, 1, NA, 0, 4))

    o <- read.csv2("ql_matrix_05.csv", row.names = 1) %>% ql_mat
    expect_identical(ql_matrix(df, "x", na.rm = T), o)

    o <- read.csv2("ql_matrix_75.csv", row.names = 1) %>% ql_mat
    expect_identical(ql_matrix(df, "x", na.rm = T, p = 0.75), o)

    df <- data.frame(x = c(0, 0, 0, 1, 1, 1),
                    y = c(NA, NA, NA, 1, 1, 1),
                    z = c(NA, NA, NA, 0, 0, 0))
    o <- read.csv2("ql_matrix_missing.csv", row.names = 1) %>% ql_mat
    expect_identical(ql_matrix(df, "x", na.rm = T), o)
    
    expect_error(ql_matrix(df))
    expect_error(ql_matrix(data.frame()))
    expect_error(ql_matrix(data.frame(z = c(1, 2))))
    expect_error(ql_matrix(data.frame(x = 1, y = 2), "z"))
})

test_that("combine ql matrices", {
    df1 <- data.frame(x = c(1, 1, 2), y = c(0, 0, 0))
    df2 <- data.frame(x = c(1, 2, 1), y = c(2, 2, 3))

    expect_error(combine(ql_matrix(df1), ql_matrix(df2)))

    df1 <- data.frame(x = c(1, 1, 2), y = c(0, 0, 0))
    df2 <- data.frame(x = c(0, 0, 2), z = c(1, 1, 2))
    df3 <- data.frame(y = c(2, 1, 2), z = c(2, 2, 1))

    result <- lapply(list(df1, df2, df3), ql_matrix) %>% Reduce(combine, .)
    o <- read.csv2("ql_matrix_combine.csv", row.names = 1) %>% ql_mat
    expect_identical(result, o)
})


