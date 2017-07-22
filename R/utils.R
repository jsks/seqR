`%||%` <- function(lhs, rhs) {
    args <- match.call()

    if (class(args$lhs) == "name") {
        sym <- deparse(substitute(lhs))

        if (!exists(sym, parent.frame()))
            return(rhs)
    }

    if (is.null(lhs) || is.na(lhs) || length(lhs) == 0)
        rhs
    else
        lhs
}


#' @importFrom magrittr %>%
magrittr::`%>%`
