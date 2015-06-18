\name{SimulatedSNR0.5Series}
\alias{SimulatedSNR0.5Series}
\docType{data}
\title{
Simulated Wavelet Series with SNR=0.5
}
\description{
A matrix containing simulated signals with noise added such that the signal-to-noise ratio (SNR) is 0.5.  We have defined the SNR as the (Variance of Noise)/(Variance of Signal).  These series are obtained by adding white noise to the smooth data in \code{SimulatedSmoothSeries}.  Each column contains a wavelet coefficient threshold level and rows contain observations.  
}
\usage{data("SimulatedSNR0.5Series")}
\format{
  The format is:
  
 num [1:1024, 1:9] -0.01354 -0.01315 -0.00491 -0.01808 0.00472 ...
 
 - attr(*, "dimnames")=List of 2
 
  ..$ : NULL
  
  ..$ : chr [1:9] "J0.0" "J0.1" "J0.2" "J0.3" ...
}
\details{
The columns contain noisy series with signals in different wavelet coefficient threshold levels. Series are available for signal thresholds of \eqn{J0} in \{0, 1, 2, 3, 4, 5, 6, 7, 8\}.  The rows are the data observations.  Thus, each smooth series is of length \eqn{2^10=1024}.

The names of each column indicate the threshold (\eqn{J0}) in the smooth series.  For example, the 3rd column, named \code{J0.2}, has a threshold of \eqn{J0=2} in the signal, and thus 0-valued wavelet coefficients for all mother wavelet coefficients finer than level 2 in the signal.  Notice, the white noise added to the signal creates non-zero coefficients above the threshold.  

The original smooth series were generated using \code{'wd'} and \code{'wr'} with the \code{family="DaubLeAsymm"}, \code{filter.number=8}, \code{bc="periodic"} options in the \code{'wavethresh'} package.
}


\references{
\code{\link[wavethresh]{wavethresh-package}}, \code{\link{SimulatedSmoothSeries}}
}
\examples{
data(SimulatedSNR0.5Series)

##See if WiSEBoot selects the correct threshold for this data (J0=3)
## R=10 bootstrap samples is not recommended.  For demonstration only.
bootObj <- WiSEBoot(SimulatedSNR0.5Series[,4], R=10)
bootObj$MSECriteria

##Look at the noisy data compared to the true smooth
data(SimulatedSmoothSeries)
plot(seq(1, 2^10), SimulatedSNR0.5Series[ , 6], main="Threshold of J0=5", 
     col="lightgray", xlab="Time", ylab="Observations", type="l")
lines(seq(1, 2^10), SimulatedSmoothSeries[ ,6], col="red", lwd=2)
}

\keyword{datasets}
