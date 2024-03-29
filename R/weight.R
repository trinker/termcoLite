#' Weight Term Counts from \code{term_count}
#'
#' Weight term counts from \code{\link[termco]{term_count}} object.
#'
#' @param x A \code{\link[termco]{term_count}} object.
#' @param weight A weight to use.   Currently the following are available:
#' \code{"proportion"}, \code{"percent"}.
#' @param \ldots ignored
#' @return Returns a weighted \code{\link[dplyr]{tibble}} object of term counts
#' by grouping variable.
#' @keywords weight
#' @export
weight <- function(x, weight = "percent", ...){

    switch(weight,
        percent = propify(x, perc),
        proportion = propify(x, prop),
        stop("Select an appropriate weighting method")
    )
}

