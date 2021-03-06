% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify.R
\name{classify}
\alias{classify}
\title{Classify Rows of a Count Matrix}
\usage{
classify(x, n = 1, ties.method = "probability", seed = NULL, ...)
}
\arguments{
\item{x}{A \code{term_count} or count \code{\link[base]{matrix}}/\code{\link[base]{data.frame}}.}

\item{n}{The number of classifications per row to return.}

\item{ties.method}{Either \code{c("probability", "random", "first", "last")}
for specifying how ties are handled; "probability" by default.  This utilizes
the probability distributions from all tags (regardless of strength/counts of
tags) to randomly \code{\link[base]{sample}} with probabilties to break ties.
Note that this can lead to different results each time \code{classify} is run.
Use \code{seed} to make results reproducible.  The other methods
use \code{\link[base]{max.col}} for tie breaking.  See
\code{\link[base]{max.col}} for a description of those arguments.}

\item{seed}{A seed to use in the sample to make the results reproducible.}

\item{\ldots}{ignored.}
}
\value{
Returns a single \code{\link[base]{vector}} or \code{\link[base]{list}}
of ordered vectors of predicted classifications; order by term frequency.
Ties default to random order.
}
\description{
Use a count matrix to classify the rows, based on the frequencies in the cells.
}
\seealso{
\code{\link[base]{max.col}}
}
\keyword{classify}
\keyword{predict}

