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
        if (class(m) == "matrix" && nrow(m) == 0)
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

add_empty <- function(x, y) {
    fn <- function(missing) as.list(setNames(rep(NA, length(missing)), missing))
                            
    empty_cols <- setdiff(colnames(y), colnames(x)) %>% fn
    empty_rows <- setdiff(rownames(y), rownames(x)) %>% fn

    x2 <- do.call(cbind, c(list(x), empty_cols))
    final <- do.call(rbind, c(list(x2), empty_rows))

    final[order(rownames(final)), order(colnames(final))]
}

#' @export
combine <- function(x, y) UseMethod("combine")

#' @export
combine.ql_mat <- function(x, y) {
    rows <- intersect(rownames(x), rownames(y)) %>% setdiff("sums")
    cols <- intersect(colnames(x), colnames(y)) %>% setdiff("sums")

    # We only want to combine unique ql matrices
    if (length(cols) > 1 & any(!is.na(x[rows, cols]) & !is.na(y[rows, cols])))
        stop("Overlapping matrices, unable to combine")

    x <- add_empty(x, y)
    y <- add_empty(y, x)

    out <- matrix(NA, nrow(x), ncol(x), dimnames = list(rownames(x), colnames(x)))

    out[] <- vapply(1:length(x), function(i) {
        # We're only adding together for the sums cols
        ifelse(is.na(x[i]) | is.na(y[i]), x[i] %||% y[i], x[i] + y[i])
    }, numeric(1))

    rows <- setdiff(rownames(out), "sums")
    cols <- setdiff(colnames(out), "sums")
    
    out <- out[c(rows, "sums"), c(cols, "sums")]
    out <- out[order(out[, ncol(out)], decreasing = T),
              order(out[nrow(out), ], decreasing = F)]

    structure(out, class = c("ql_mat", class(out)))
}


#' @export
print.ql_mat <- function(x, ...) {
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

        c("Num of States" = nr_states, "Num of Dependents" = nrow(m),
          "Matrix Column Sum" = state_sum, "Total Sum" = total_sum)
    })

    out <- do.call(rbind, ll)
    rownames(out) <- vars

    idx <- as.numeric(out[, ncol(out)]) %>% order(decreasing = T)
    out[idx, ]
}
