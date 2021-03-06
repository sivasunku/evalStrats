% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/velocity.R
\name{velocity}
\alias{velocity}
\title{Calculates the velocity & acceleration in a given n points.}
\usage{
velocity(n, pct = FALSE)
}
\arguments{
\item{n}{- input numbers}

\item{pct}{- If 'n' should be normalized(to 100s) before finding the slope.}
}
\value{
returns velocity & acceleration
}
\description{
The function returns the slope of given n points
}
\examples{
velocity(n,pct = TRUE)
}
\author{
Siva Sunku
}
\keyword{velocity}
