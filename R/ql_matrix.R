# TODO: This is NOT a contingency table. What do we call it instead?

#' Contingency Table
#'
#' Construct a contingency table showing all observed pairs between different states of variables.
#'
#' @param data A `data.frame` of at least 2 columns.
#' @param vars Optional `CharacterVector` of target
#'     variables. Default: all variables within `data`.
#' @param p A `NumericVector` of probabilities passed to `quantile`. Default: `0.05`.
#' @param ... Additional options passed to `quantile`
#'
#' @details For each variable in `vars, `ql_matrix` first splits the
#'     data for each observed state and then calculates the percentile
#'     given by `p` for each of the remaining variables. So yes, this
#'     is not a contingency table. Instead, the default value of `p`
#'     is such that we're almost calulating the minimum value observed
#'     for all variables for each state of a target variable. What
#'     does this tell us? I have no idea.
#'
#' @return A `S3` `matrix` of class `ql_mat`.
#'
#' @examples
#' x <- data.frame(a = c(0, 1), b = c(1, 1))
#' ql_matrix(x)
#' 
#' @export
ql_matrix <- function(data, vars = colnames(data), p = 0.05, ...) {
    if (ncol(data) < 2)
        stop("Must supply of data frame with at least 2 columns")

    if (any(!vars %in% colnames(data)))
        stop("All given variables must appear in the data frame")

    ll <- lapply(vars, function(varname) {
        g <- as.factor(data[[varname]])
        
        m <- vapply(colnames(data)[colnames(data) != varname], function(s) {
            tapply(data[[s]], g, function(v) stats::quantile(v, p, ...) %>% round)
        }, numeric(nlevels(g)))

        # If no obs make sure we still appear in output table
        if (nrow(m) == 0)
            m <- rbind(m, NA)

        if (nlevels(g) == 1)
            m <- t(m)

        vns <- paste(varname, rownames(m) %||% 0)
        as.data.frame(m) %>% cbind(vns, stringsAsFactors = F)
    })

    f.df <- if (length(ll) > 1)
                Reduce(function(m, n) merge(m, n, all = T), ll)
            else
                ll[[1]]

    rownames(f.df) <- f.df$vns
    f.df$vns <- NULL
    f.m <- as.matrix(f.df)

    row_sums <- rowSums(f.m, na.rm = T)
    col_sums <- colSums(f.m, na.rm = T)

    f.m <- cbind(f.m, sums = row_sums)
    f.m <- rbind(f.m, sums = c(col_sums, NA))

    f.m <- f.m[order(f.m[, ncol(f.m)], decreasing = T),
               order(f.m[nrow(f.m), ], decreasing = F)]

    structure(f.m, class = c("ql_mat", class(f.m)))
}

#' @export
print.ql_matrix <- function(x, ...) {
    class(x) <- "matrix"
    print(x, ...)
}

#' @export
is.ql_mat <- function(x) {
    inherits(x, "ql_mat")
}

#' @export
as.list.ql_mat <- function(x, ...) {
    # Let's start by stripping the sums
    x <- x[-which(rownames(x) == "sums"), -which(colnames(x) == "sums")] 

    # TODO: properly split the matrix, or maybe just use data frames
    vars <- sub("\\s\\d+$", "", rownames(x)) %>% unique
    ll <- lapply(vars, function(s) {
        m <- t(x[grepl(s, rownames(x)),, drop = F])
        m[-which(rownames(m) == s), order(colnames(m)), drop = F]
    })

    stats::setNames(ll, vars)
}

#' @export
summary.ql_mat <- function(object, ...) {
    # Let's start by stripping the sums
    object <- object[-which(rownames(object) == "sums"), -which(colnames(object) == "sums")] 

    # TODO: properly split the matrix, or maybe just use data frames
    vars <- sub("\\s\\d+$", "", rownames(object)) %>% unique
    ll <- lapply(vars, function(s) {
        m <- t(object[grepl(s, rownames(object)),, drop = F])
        m <- m[, order(colnames(m)), drop = F]

        nr_states <- ncol(m)
        state_sum <- sum(m[, nr_states], na.rm = T)
        total_sum <- sum(m, na.rm = T)

        list("Num of States" = nr_states, "Num of Dependents" = nrow(m),
             "Matrix Column Sum" = state_sum, "Total Sum" = total_sum)
    })

    out <- do.call(rbind, ll)
    rownames(out) <- vars

    idx <- as.numeric(out[, ncol(out)]) %>% order(decreasing = T)
    out[idx, ]
}
