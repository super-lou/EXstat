% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_trend.R
\name{get_intercept}
\alias{get_intercept}
\title{get_intercept}
\usage{
get_intercept(dataEX, trendEX, verbose = TRUE)
}
\arguments{
\item{dataEX}{A data frame containing the data.}

\item{trendEX}{A data frame containing the results of the trend analysis.}

\item{verbose}{A logical value indicating whether to display verbose output (default is TRUE).}
}
\value{
A modified data frame with an additional column indicating the intercept of the trend.
}
\description{
Computes the intercept of the trend in the data.
}
\details{
The function computes the intercept of the trend in the data based on the results of the trend analysis. It calculates the mean value of the dependent variable (Variable) and the mean time variable (date) for each code group. Then, it computes the intercept using the formula: b = mu_X - mu_t * a, where mu_X is the mean value, mu_t is the mean time, and a is the slope of the trend.
}
\note{
documentation generated by chatGPT
}
