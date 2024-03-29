% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{getAR1Correction}
\alias{getAR1Correction}
\title{AR(1) correction}
\usage{
getAR1Correction(Z)
}
\arguments{
\item{Z}{numeric, data (NA-free)}
}
\value{
A list with components: $lag1 (estimated lag-1 correlation
coefficient) and $correction (the correction for the variance of MK
stat)
}
\description{
Compute correction to the variance of MK statistics to
account for AR(1) autocorrelation
}
\examples{
\dontrun{
data(nhtemp) # Average Yearly Temperatures in New Haven
getAR1Correction(Z=nhtemp)
getAR1Correction(rnorm(100))
}
}
