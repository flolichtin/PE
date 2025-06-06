% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_NA.R
\name{to_NA}
\alias{to_NA}
\title{Cast to NA}
\usage{
to_NA(df, labels_NA = PE::labels$na)
}
\arguments{
\item{df}{e.g. \code{PE::data_w}}

\item{labels_NA}{defaults to \code{PE::labels$na}}
}
\value{
relabelled data frame (with more real NA values...)
}
\description{
Careful: This just goes through a vector which is interpreted as NA values.
For example: "none" should probably not (always) be interpreted as NA as in education...
}
\examples{
to_NA(PE::data_w)
}
