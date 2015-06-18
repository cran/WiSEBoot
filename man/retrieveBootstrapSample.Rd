\name{retrieveBootstrapSample}
\alias{retrieveBootstrapSample}

\title{
Construct the bootstrap data series from wavelet coefficients
}
\description{
Use the wavelet coefficients from the selected WiSE bootstrap model to construct smooth bootstrap series in the original time/space domain.  This function may be of little use and creates large array objects within R.  The wavelet coefficients are a preferable representation of the data, as they are sparse and contain the signal information.
}

\usage{
retrieveBootstrapSample(WiSEObj)
}

\arguments{
  \item{WiSEObj}{
an object obtained from \code{WiSEBoot}.  The \code{"periodic"} boundary condition for wavelets is required.
}

}
\details{
The \code{wavethresh} package is used to perform the inverse wavelet decomposition of each bootstrap sample of wavelet coefficients.

The bootstrap series will be smoothed to the selected threshold level, \eqn{J0=j} (see \code{WiSEBoot}).  
}

\value{
\item{ BootSample }{an array.  The rows contain each bootstrap sample -- dimension is the \code{R} supplied to the original \code{WiSEBoot} call.  The columns contain the observations in the original time/space domain -- dimension is the length of the data series, \eqn{T}.  The 3rd dimension corresponds to the data series -- If the user supplied a matrix of more than 1 data series, a bootstrap sample is generated for each series, in the order of the data series supplied.}
}
\references{
For an overview of the wavelet methodology used in \code{wavethresh}, see "Wavelet Methods for Statistics in R," (Nason, 2008).
}
\author{
Megan Heyman
}

\seealso{
 \code{\link{WiSEBoot}}, \code{\link[wavethresh]{wavethresh-package}}
}

\examples{
someData <- rnorm(2^5)

##Bootstrap sample of size 10 is not recommended. For demonstration only.
bootInfo <- WiSEBoot(someData, R=10, J0=2)
bootSeries <- retrieveBootstrapSample(bootInfo)$BootSample

bootSeries[1, , 1] #this is the first bootstrap series
}

\keyword{ datagen }
\keyword{ smooth }
