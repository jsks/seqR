test_that("Testing adjacency matrix function", {
    # Let's try a simple example first
    a <- c(0, 1, 1)
    b <- c(0, 0, 1)
    adjm <- adj_matrix(a, b, 2)

    expect_s3_class(adjm, "adj_mat")
    expect_true(is.adj_mat(adjm))

    out <- matrix(0, 4, 4)
    out[1, 3] <- 1
    out[3, 4] <- 1
    class(out) <- c("adj_mat", "matrix")

    expect_identical(adjm, out)

    # Now something a little bit more involved
    a <- c(2, 3, 3, 2, 0, 2, 1, 3, 1, 3)
    b <- c(1, 0, 2, 1, 1, 0, 3, 1, 3, 1)

    out <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
                       0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(16L, 16L), class = c("adj_mat", "matrix"))

    expect_identical(adj_matrix(a, b),  out)

    # How about invalid sizes
    expect_error(adj_matrix(c(1, 2, 3), c(1, 2)))
    expect_error(adj_matrix(1, 2))
    
    # TODO: index starting at 1
})
