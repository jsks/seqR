#' Percentage Table
#'
#' Construct a table of relative frequencies.
#'
#' @param data A `data.frame`.
#' @param complete.obs A boolean value. Calculate percentages based on
#'     complete observations for each pair of variables. Default: `TRUE`.
#'
#' @return A `data.frame` where the last three columns break down the
#'     relative frequency of variable `X` being larger than variable
#'     `Y`, vice versa, and when `X` == `Y`.
#'
#' @examples
#' x <- data.frame(a = c(0, 0, 1, 4), b = c(1, 2, 1, 0))
#' ptable(x)
#' 
#' @export
ptable <- function(data, complete.obs = T) {
    if (!"data.frame" %in% class(data))
        stop("Expects a data frame")
    
    if (nrow(data) == 0 || ncol(data) < 2)
        return(data.frame())

    indicators <- colnames(data)
    varlist <- utils::combn(indicators, m = 2, simplify = T)
    final <- data.frame(X = varlist[1, ],
                        Y = varlist[2, ],
                        "X>Y" = NA, "X=Y" = NA, "X<Y" = NA,
                        stringsAsFactors = F, check.names = F)

    for (C in 1:ncol(varlist)) {
        var1 <- varlist[1, C]
        var2 <- varlist[2, C]

        final[C, c("X", "Y")] <- c(var1, var2)
        final[C, c("X>Y", "X=Y", "X<Y")] <- percents(data[, var1], data[, var2],
                                                     complete.obs)

        if (final[C, "X<Y"] > final[C, "X>Y"]) {
            tmp <- final[C, "X<Y"]
            final[C, "X<Y"] <- final[C, "X>Y"]
            final[C, "X>Y"] <- tmp
            final[C, c("X", "Y")] <- final[C, c("Y", "X")]
        }
    }

    final
}

percents <- function(x, y, complete.obs) {
    # Beautifully ugly code :(
    len <- if (complete.obs)
               ifelse((l <- sum(!is.na(x) & !is.na(y))) == 0, 1, l)
           else
               length(x)
               
    greater <- (sum(x > y, na.rm = T)/ len) * 100
    equal <- (sum(x == y, na.rm = T) / len) * 100
    lesser <- (sum(x < y, na.rm = T) / len) * 100

    list("X>Y" = greater, "X=Y" = equal, "X<Y" = lesser)
}
