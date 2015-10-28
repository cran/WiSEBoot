\name{WiSEBoot-package}
\alias{WiSEBoot-package}
\docType{package}
\title{
Wild Scale-Enhanced (WiSE) Bootstrap
}
\description{
The Wild Scale-Enhanced (WiSE) bootstrap method is implemented for models which include a wavelet signal component.  Functions within this package allow the users to automatically select a wavelet smoothing level, test wavelet coefficient-specific hypotheses, and manipulate the data for usage within wavelet methodologies.
}
\details{
\tabular{ll}{
Package: \tab WiSEBoot\cr
Type: \tab Package\cr
Version: \tab 1.3.0\cr
Date: \tab 2015-10-28\cr
License: \tab GPL-2\cr
}

Most users of this package will need to modify their data so that it is compatible with the discrete wavelet transform.  That is, the input data series for the WiSE bootstrap must be of length \eqn{T=2^J} for some positive integer, \eqn{J}.  The \code{padVector} and \code{padMatrix} functions are quick and easy tools which convert the data to appropriate length.  A base knowledge of wavelets is recommended before attempting to utilize this package.

The bootstrap methodology is built to automatically estimate parameters within models of the form

\deqn{Y = \gamma_0 1 + \gamma_1 t + W \gamma  + e}

where \eqn{Y} is the data is the vector, linear parameters in time (\eqn{t}) are \eqn{\gamma_0} and \eqn{\gamma_1}, \eqn{\gamma} are the wavelet coefficients (scaling and filter), and \eqn{W} is the DWT for a fixed wavelet basis.  Note, in many cases of the DWT, the scaling coefficient is equivalent to \eqn{\gamma_0}, and thus, estimated there.  

The \code{WiSEBoot} function allows the user to automatically select a threshold level for \eqn{\gamma}.  Our threshold is defined as the level above which all fine wavelet coefficients are set to 0.  This function also provides the WiSE bootstrap samples of the wavelet coefficients (for the selected threshold) and bootstrap samples of the linear parameters.

The \code{WiSEHypothesisTest} and \code{WiSEConfidenceRegion} functions allow the user to test or examine a specific relationship between wavelet coefficients from two data series.  Specifically, given 2 data series, 
\deqn{X = \gamma_{x0} 1 + \gamma_{x1} t + W\gamma_x  + e_x} 
\deqn{Y = \gamma_{y0} 1 + \gamma_{y1} t + W\gamma_y  + e_y}
these functions help the user examine the viability of the relationship \eqn{\gamma_y = \alpha + \beta \gamma_x}.  Note, \eqn{\gamma_x} and \eqn{\gamma_y} are defined as the non-thresholded wavelet coefficients of these series.  We may obtain a p-value for the specific null hypothesis 
\deqn{H_0:  \alpha = m, \beta = n}
(where \eqn{m, n} are real numbers), or visualize a confidence region in these parameters.  
}


\author{
Megan Heyman, Snigdhansu Chatterjee

Maintainer: Megan Heyman <heyma029@umn.edu>
}


\references{
Errata and breaking news:  \url{http://users.stat.umn.edu/~heyma029/WiSEBoot_errata.html}

Amy Braverman (NASA-JPL), Noel Cressie (Univ. of Wollongong), and Matthew Gunson (NASA-JPL) are other major contributors in projects related to developing methodology for this package.

The WiSE bootstrap methodology is defined in theoretical detail in Chatterjee, S. et al. "WiSE bootstrap for model selection" (in progress).

The WiSE bootstrap hypothesis test is implemented as an analysis tool in Braverman, A. et al. "Probabilistic Climate Model Evaluation" (in progress).


For an overview of wavelet methods using \code{wavethresh} in R, see "Wavelet Methods for Statistics in R," (Nason, 2008).
}


\keyword{ package }
\seealso{
\code{\link[wavethresh]{wavethresh-package}}
}
\examples{
##User should implement a high number of bootstrap samples (R).  
##  R=10 bootstrap samples is not recommended.  For demonstration only.

##Select a wavelet smooth level for signal
data("SimulatedSNR1.0Series")
bootObj <- WiSEBoot(SimulatedSNR1.0Series[,4], R=10)
bootObj$MSECriteria #check WiSEBoot selected threshold (minimum MSE) -- truth is J0=3

##Test whether \alpha=0 and \beta=1 for AIRS and IPSL Run 1 at 60E
data("CM20N20S60E")
padData <- padMatrix(CM20N20S60E)
hypTest <- WiSEHypothesisTest(padData$xPad[,1], padData$xPad[,2], J0=5, R=10, 
                              XParam=padData$linearParam[,1], YParam=padData$linearParam[,2], 
                              plot=TRUE)
}
