% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{generalMannKendall}
\alias{generalMannKendall}
\title{General Mann-Kendall}
\usage{
generalMannKendall(
  X,
  level = 0.1,
  dep.option = "INDE",
  DoDetrending = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{X}{numeric vector, data. IMPORTANT: X is assumed to be
regularly-spaced. It uses NA to fill the gaps rather than removing
missing values.}

\item{level}{numeric in (0,1), level of the test.}

\item{dep.option}{string, option for handling temporal dependence.
Available :
\enumerate{
\item 'INDE', assume independence (i.e. the standard MK test)
\item 'AR1', assumes AR1 short-term dependence structure (i.e.
Hamed and Rao's version of the MK test)
\item 'LTP', assume long-term persistence (i.e. Hamed's version
of the MK test)
}}

\item{DoDetrending, }{logical, only used for dep.option == LTP:
do detrending before estimating Hurst coefficient (default=TRUE as
recommended in Hamed's paper)}

\item{verbose}{\link[base:logical]{logical} Whether to print intermediate messages. Default is \code{FALSE}.}
}
\value{
A list with the following fields :
\enumerate{
\item H: logical, reject (true) or do not reject (false) H0
\item P: p-value of the test
\item STAT: test statistics
\item TREND: trend estimate (using Sen's slope estimate)
\item DEP: dependence estimate (= 0 if dep.option='INDE',
=lag-1 autocorrelation if dep.option='AR1', =Hurst coefficient if
dep.option='LTP')
}
}
\description{
A general version of the Mann-Kendall test, enabling
various dependence assumptions.
}
\details{
\enumerate{
\item Handling of ties: Specific formula exist for INDE and
AR1, but the LTP case is trickier. Hammed's paper is unclear on how
to handle ties, especially at the step of Hurst coefficient
estimation. There is a normal-score transformation at this step,
and one needs to decide how to assign a rank to ties. What is
implemented below is the option ties.method = "random", i.e. the
rank is randomized for ties. This is not, strictly speaking,
correct because this randomization impacts the dependence
structure. However synthetic runs suggest it works OK.
\item Computational efficiency: Likely poor for case
dep.option='LTP'. There is a 4-level loop which leads to a n^4
algorithm. I attempted to vectorize this loop but it didn't improve
things => Expect significant running times for dep.option='LTP
' when size(X) > 50... (orders of magnitude: 1s for n=30, 10s for
n=50, 2-3 minutes for n=100). On the other hand both options INDE
and AR1 are very fast.
}
}
\examples{
\dontrun{
data(nhtemp) #Average Yearly Temperatures in New Haven
generalMannKendall(X=nhtemp, dep.option='AR1')
}
}
\references{
\enumerate{
\item Hamed, Rao, 1998. A modified Mann-Kendall trend test for
autocorrelated data. J. Hydrol., 204(1-4): 182-196.
\item Hamed, 2008. Trend detection in hydrologic data: The
Mann-Kendall trend test under the scaling hypothesis. J. Hydrol.,
349(3-4): 350-363.
}
}
