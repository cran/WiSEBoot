\name{SimulatedSmoothSeries}
\alias{SimulatedSmoothSeries}
\docType{data}
\title{
Simulated Wavelet-Smoothed Series
}
\description{
A matrix containing some simulated, smooth data series.  The smooth series are contained in the columns, and the data observations in the rows.  An inverse wavelet decomposition of the series exactly yields thresholded coefficients at the specified level (\eqn{J0=j}).  
}
\usage{data("SimulatedSmoothSeries")}
\format{
  The format is:
  
 num [1:1024, 1:9] -0.00934 -0.00934 -0.00935 -0.00935 -0.00935 ...
 
 - attr(*, "dimnames")=List of 2
 
  ..$ : NULL
  
  ..$ : chr [1:9] "J0.0" "J0.1" "J0.2" "J0.3" ...
}

\details{
The columns contain series created by different wavelet coefficient threshold levels. Smoothed series are available for thresholds of \eqn{J0} in \{0, 1, 2, 3, 4, 5, 6, 7, 8\}.  The rows are the data observations.  Thus, each smooth series is of length \eqn{2^{10}=1024}. 

The names of each column indicate the threshold (\eqn{J0}).  For example, the 3rd column, named \code{J0.2}, has a threshold of \eqn{J0=2}, and thus 0-valued wavelet coefficients for all mother wavelet coefficients finer than level 2.  

These smooth series were generated using \code{'wd'} and \code{'wr'} with the \code{family="DaubLeAsymm"}, \code{filter.number=8}, \code{bc="periodic"} options in the \code{'wavethresh'} package.
}

\references{
The user may want these smooth series to aid in simulations with this package.

Also see \code{\link[wavethresh]{wavethresh-package}}
}

\keyword{datasets}
