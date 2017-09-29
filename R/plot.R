ttm_plot <- function(x, ...) {
    size <- sqrt(nrow(x)) - 1
    
    g <- igraph::graph.adjacency(x, mode = "directed", weighted = T, diag = F)
    igraph::V(g)$color <- "skyblue"

    # This is an ugly hack to make 0 sized vertices "invisible"
    igraph::V(g)$size <- ifelse((y <- rowSums(x)) == 0, 0.0001, y) / (size + 1)

    graphics::plot(g, vertex.label = NA, vertex.frame.color = NA,
         layout = igraph::layout.grid(g, width = 0, height = 0),
         frame = F, edge.width = igraph::E(g)$weight/5, edge.curved = TRUE,
         edge.arrow.size = 0.4, margin = 0.1, ...)

    states <- 0:size
    graphics::axis(1, at = (states * (2 / size )) - 1, labels = states) +
        graphics::axis(2, at = (states * ( 2 / size)) - 1, labels = states, las = 2)

    invisible()
}

#' Directed Adjacency Graphs
#'
#' Creates "bubble plots" (*i.e.*, directed adjacency graphs) showing
#' transitions for observed pairs of two variables.
#'
#' @param x `NumericVector`.
#' @param y `NumericVector`.
#' @param data Optional `data.frame` containing the variables to be graphed.
#' @param ... Additional parameters passed to `plot`.
#' 
#' @examples
#' x <- c(0, 1, 1, 0, 0, 1, 0, 1)
#' y <- c(1, 1, 1, 0, 0, 1, 0, 1)
#' bubbleplot(x, y)
#'
#' @return Outputs a plot!
#' 
#' @export
bubbleplot <- function(x, y, data = NULL, ...) {
    x <- if (!is.null(data)) eval(substitute(x), data) else x
    y <- if (!is.null(data)) eval(substitute(y), data) else y
    
    adj_matrix(y, x) %>% ttm_plot(...)
}

#' @export
plot.adj_mat <- ttm_plot

#' @export
is.adj_mat <- function(x) {
    inherits(x, "adj_mat")
}

#' @export
print.ql_matrix <- function(x, ...) {
    class(x) <- "matrix"
    print(x, ...)
}

