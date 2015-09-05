\name{smoothTimeSeries}
\alias{smoothTimeSeries}
\title{
Threshold Wavelet Coefficients to Create Smooth Time Series
}


\description{
This function takes a vector of an equally-spaced time series which is of length \eqn{T=2^J} for a positive integer, \eqn{J}.  The series is thresholded at all wavelet coefficient levels and a matrix is returned which contains each of the wavelet-smoothed series.  Optionally, the user may plot none, one, or all of the wavelet smoothed series compared to the original input data.

This function is most useful for visualization of the data and primarily used within the WiSE bootstrap methodology.
}


\usage{
smoothTimeSeries(X, wavFam = "DaubLeAsymm", wavFil = 8, wavBC = "periodic", 
                 plotLevels = "none", ...)
}


\arguments{
  \item{X}{
a vector containing an equally-spaced data series.  The vector must be of length \eqn{T=2^J} for some integer, \eqn{J>3}, contain no missing values, and be numeric.  If the data is a single-row or column matrix, it must be input to this function as a vector.
}
  \item{wavFam}{
wavelet family.  Allowed values are \code{"DaubLeAsymm"} and \code{"DaubExPhase"}  -- Daubechies Least Asymmetric and Daubechies Extremal Phase.  This is the \code{family} used within the \code{wavethresh} package.
}
  \item{wavFil}{
wavelet filter number.  Allowed values are integers between 4 and 10 when \code{wavFam="DaubLeAsymm"} or integers between 1 and 10 when \code{wavFam="DaubExPhase"}.  These correspond to the number of vanishing moments of the wavelet.  This is the \code{filter.number} used within the \code{wavethresh} package.
}
  \item{wavBC}{
wavelet boundary condition.  Allowed values are \code{"periodic"} and \code{"symmetric"}.  This is the \code{bc} used within the \code{wavethresh} package.
}
  \item{plotLevels}{
plotting option.  Allowed values are \code{"none"}, \code{"all"}, or an integer between 0 and \eqn{J-1}.  
}
  \item{...}{
additional graphical arguments.  See \code{\link[graphics]{plot}}, \code{\link[graphics]{plot.default}}.
}
}
\details{
To smooth the series to the \eqn{J0=j} level, all wavelet coefficients at levels \eqn{j+1} and finer are set to 0.  The reverse wavelet transformation is performed upon the thresholded coefficients to obtain each smooth data series.  This method produces smooth data series for \eqn{J0+1} in \{\eqn{0, 1, ..., J-1}\}.  Note, when \eqn{J0+1=0}, this indicates that ALL mother wavelet coefficients have been set to 0.

If \code{"all"} plots are requested, all possible smooth series are plotted against the original input data within one graphical device.  This is recommended for users trying to visualize the level of smoothing needed in their data.  Note, \eqn{J0+1} corresponds to the first fine level of coefficients which is set entirely to 0.  

The wavelet options are only those allowed within the \code{wavethresh} package.  Please see the documentation for \code{wavethresh} for further explanation of these quantities.  
}
\value{
a matrix containing the smooth series.  The column names represent the smooth-level of the data series.  The first column contains the original input data.  The second column contains the wavelet smooth which sets only the finest level of coefficients to 0 (i.e. \eqn{J0+1=J-1}).  The last column contains the smooth which sets all mother wavelet coefficients to 0 (i.e. \eqn{J0+1=0}).  Notice, the column names should help indicate the smooth level.  The matrix rows contain the ordered observations.
}


\author{
Megan Heyman
}

\seealso{
To obtain data series of the appropriate length:  \code{\link{padVector}}.  

Documentation regarding the wavelet transform: \code{\link[wavethresh]{wavethresh-package}}
}

\examples{
##Visualize data smoothing on the AIRS 60E data
data(CM20N20S60E)
AIRS <- as.vector(CM20N20S60E[ ,1])
padAIRS <- padVector(AIRS)$xPad
smoothAIRS <- smoothTimeSeries(padAIRS, plotLevels="all")
}


\keyword{ smooth }
\keyword{ ts }
