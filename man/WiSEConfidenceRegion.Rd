\name{WiSEConfidenceRegion}
\alias{WiSEConfidenceRegion}

\title{
WiSE Wavelet Coefficients:  Linear Confidence Region
}
\description{
Calculate the WiSE bootstrap sample of the linear parameters describing a set of wavelet coefficients from two WiSEBoot objects.  See Details for a precise description.
}

\usage{
WiSEConfidenceRegion(X, Y, plot = TRUE, ...)
}


\arguments{
  \item{X}{
\code{WiSEBoot} object for a single data series.  The data series length from this \code{WiSEBoot} object should match the data series length for the object in \code{Y}.  The selected threshold within this object should match the selected threshold for the object in \code{Y}.  The number of bootstrap samples within this object should match the number of samples in \code{Y}.  It is recommended that the wavelet settings match between \code{X} and \code{Y}.
}
  \item{Y}{
\code{WiSEBoot} object for a single data series.  The data series length from this \code{WiSEBoot} object should match the data series length for the object in \code{X}.  The selected threshold within this object should match the selected threshold for the object in \code{X}.  The number of bootstrap samples within this object should match the number of samples in \code{X}.  It is recommended that the wavelet settings match between \code{X} and \code{Y}.
}
  \item{plot}{
logical.  If \code{TRUE}, a plot of the bootstrap sample of linear parameters in the wavelet coefficients is generated.
}
  \item{...}{
additional graphical arguments.  See \code{\link[graphics]{plot}}, \code{\link[graphics]{plot.default}}.
}
}


\details{
Given 2 vectors of equally-spaced data of length \eqn{T=2^J} for a positive integer, \eqn{J}, we assume the following models:
\deqn{X = \gamma_{x0} 1 + \gamma_{x1} t + W\gamma_x  + e_x} 
\deqn{Y = \gamma_{y0} 1 + \gamma_{y1} t + W\gamma_y  + e_y}

The WiSE bootstrap is performed on each data series with the same threshold level, \eqn{J0=j}, using \code{WiSEBoot}.  Bootstrap estimates of \eqn{\gamma_x} and \eqn{\gamma_y} are obtained from these \code{WiSEBoot} objects, called \eqn{g^*_{xj}} and \eqn{g^*_{yj}}.  The estimates of \eqn{\gamma_x} and \eqn{\gamma_y} from the input data may be noted as \eqn{g_{xj}} and \eqn{g_{yj}}.

This function allows the user to examine a linear relationship between the two sets of wavelet coefficients:
\deqn{\gamma_y = \alpha 1 + \beta \gamma_x }

Thus, we use \eqn{g^*_{xj}} and \eqn{g^*_{yj}} to obtain estimates of \eqn{\alpha} and \eqn{\beta}, called \eqn{a^*} and \eqn{b^*}.  Likewise, the original coefficients from the data, \eqn{g_{xj}} and \eqn{g_{yj}}, yield estimates, \eqn{a} and \eqn{b}.  The bootstrap parameter estimates allow the user to visualize the distribution of the linear parameters, \eqn{\alpha} and \eqn{\beta}.  Generally, these parameters give us an idea about the relationship between the two data series signals.

Currently, this function does not calculate a (1-\eqn{\alpha})\% 2-dimensional region in the parameters.  It allows a qualitative visualization of the bootstrap sample distribution for these linear parameters.  Asymptotically, the linear parameters follow a multivariate normal distribution when assumptions are met.

Note, the slope and intercept parameters here (\eqn{\alpha} and \eqn{\beta}) are different from the linear parameters in time within our data series (\eqn{\gamma_{x0}, \gamma_{y0}, \gamma_{x1}, \gamma_{y1}}).  The \code{BootSlope} and \code{BootIntercept} output from \code{WiSEBoot} represent the linear parameters in time within the data series.
}

\value{
\item{ dataIntercept }{ point estimate of intercept of the wavelet coefficients from the data.  Using the notation in the Details section, \eqn{a}.}

\item{ dataSlope }{ point estimate of slope of the wavelet coefficients from the data.  Using the notation in the Details section, \eqn{b}.}

\item{ bootIntercept }{ bootstrap estimates of the intercept of the wavelet coefficients.  Using the notation in the Details section, \eqn{a^*}.  This is a vector.}

\item{ bootSlope}{ bootstrap estimates of the slope of the wavelet coefficients.  Using the notation in the Details section, \eqn{b^*}.  This is a vector.}
}

\references{
The WiSE bootstrap hypothesis test is implemented as an analysis tool in Braverman, A. et al. "Probabilistic Climate Model Evaluation" (in progress).  This is the corresponding confidence region to the calculations presented there.
}

\author{
Megan Heyman
}


\seealso{
\code{\link{WiSEBoot}}, \code{\link[wavethresh]{wavethresh-package}}
}


\examples{
## R=10 bootstrap samples is not recommended. For demonstration only.


###Example with random data
x <- rnorm(2^8)
y <- x + rnorm(2^8, sd=0.001) #y has similar structure to x
xWise <- WiSEBoot(x, R=10, J0=4)
yWise <- WiSEBoot(y, R=10, J0=4)
xyConf <- WiSEConfidenceRegion(xWise, yWise) #does the region contain (0, 1)?


###Example with AIRS and IPSL data
data(CM20N20S60E)
padCM <- padMatrix(CM20N20S60E)  #pad data so we can use wavelet methodology

AIRS <- WiSEBoot(padCM$xPad[,1], R=10, J0=5, XParam=padCM$linearParam[,1])
IPSL1 <- WiSEBoot(padCM$xPad[,2], R=10, J0=5, XParam=padCM$linearParam[,2])

AIRS_IPSL1Conf <- WiSEConfidenceRegion(AIRS, IPSL1) #how are these signals related?
}

\keyword{ distributions }
\keyword{ htest }
\keyword{ nonparametric }
