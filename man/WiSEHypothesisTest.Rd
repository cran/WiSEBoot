\name{WiSEHypothesisTest}
\alias{WiSEHypothesisTest}

\title{
WiSE Wavelet Coefficients:  Linear Hypothesis Test
}

\description{
Calculate the p-value for a hypothesis test regarding a linear relationship between wavelet coefficients from two data series.  See Details for a precise description.
}

\usage{
WiSEHypothesisTest(X, Y, J0, R, popParam = c(0, 1), XParam = c(NA, NA), 
                   YParam = c(NA, NA), TauSq = "log", bootDistn = "normal", 
                   wavFam = "DaubLeAsymm", wavFil = 8, wavBC = "periodic", 
                   plot = TRUE, ...)
}

\arguments{
  \item{X}{
vector of equally-spaced data.  This must be of length \eqn{T=2^J} where \eqn{J>2} is an integer.  It is required that \code{length(X)=length(Y)}.  The vector should contain only numeric values and be non-missing.  See the Details section for a description of the relationship between \code{X} and \code{Y}.
}
  \item{Y}{
vector of equally-spaced data.  This must be of length \eqn{T=2^J} where \eqn{J>2} is an integer.  It is required that \code{length(X)=length(Y)}.  The vector should contain only numeric values and be non-missing.  See the Details section for a description of the relationship between \code{X} and \code{Y}.
}
  \item{J0}{
wavelet coefficient threshold level.  Allowed values are integers between 0 and \eqn{J-2}.  Note, \eqn{J} is related to the data series length -- \eqn{T=2^J}.
}
  \item{R}{
number of WiSE bootstrap samples.  Allowed value is a positive integer.
}
  \item{popParam}{
hypothesized parameter values.  Allowed input is a vector of length 2 which is non-missing and contains numeric entries.  The first entry of the vector is the hypothesized value of the population intercept.  The second entry of the vector is the hypothesized value of the population slope.
}
  \item{XParam}{
estimated linear parameter values (in time) from \code{X}.  Allowed input is a vector of length 2 which is completely missing or contains numeric entries.  The first entry of the vector is the intercept for \code{X} and the second entry is the slope for \code{X}.  These are the estimated slope and intercept in the data -- correspond to estimates of \eqn{\gamma_{x0}} and \eqn{\gamma_{x1}} in the Details section.  If missing, these are estimated using least squares.  If the data is modified with \code{padVector} or \code{padMatrix}, it is recommended that the user supply the \code{linearParam} output here.  
}
  \item{YParam}{
estimated linear parameter values (in time) from \code{Y}.  Allowed input is a vector of length 2 which is completely missing or contains numeric entries.  The first entry of the vector is the intercept for \code{Y} and the second entry is the slope for \code{Y}.  These are the estimated slope and intercept in the data -- correspond to estimates of \eqn{\gamma_{y0}} and \eqn{\gamma_{y1}} in the Details section.  If missing, these are estimated using least squares.  If the data is modified with \code{padVector} or \code{padMatrix}, it is recommended that the user supply the \code{linearParam} output here.  
}
  \item{TauSq}{
scale parameter for the bootstrap.  Allowed values are \code{"log", "log10", "sqrt", "2/5"}.  The scale parameter is related to the length of the data series.  For example, \code{"log"} implies a value of the scale parameter, \eqn{\tau}, of \eqn{sqrt(log(T))}.
}
  \item{bootDistn}{
the distribution for the bootstrap.  The only allowed value is \code{"normal"}.  This draws iid random samples from the N(0,1) distribution for the wild bootstrap.
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
  \item{plot}{
logical.  If \code{TRUE}, a plot of the bootstrap sample of the linear parameters (generated under the null hypothesis) and the estimated parameters from the data are shown.
}
  \item{...}{
additional graphical arguments.  See \code{\link[graphics]{plot}}, \code{\link[graphics]{plot.default}}.
}
}


\details{
Given 2 vectors of equally-spaced data of length \eqn{T=2^J} for a positive integer, \eqn{J}, we assume the following models:
\deqn{X = \gamma_{x0} + \gamma_{x1} t + W\gamma_x  + e_x} 
\deqn{Y = \gamma_{y0} + \gamma_{y1} t + W\gamma_y  + e_y}
where \eqn{Y} and \eqn{X} are the data vectors, linear parameters in time (\eqn{t}) are \eqn{\gamma_{x0}, \gamma_{x1}, \gamma_{y0}} and \eqn{\gamma_{y1}}.

The \eqn{\gamma_x} and \eqn{\gamma_y} are the wavelet coefficients (scaling and filter) and W is the DWT for a fixed wavelet basis.  Note, in many cases of the DWT, the scaling coefficient is equivalent to \eqn{\gamma_{x0}, \gamma_{y0}}, and thus, estimated there.  

In this function, we consider a linear relationship between the wavelet coefficients.  Specifically, we hypothesize a relationship
\deqn{\gamma_y = \alpha + \beta \gamma_x}  
The null hypothesis is
\deqn{H_0:  \alpha = m, \beta=n}
for real numbers \eqn{m, n}.  The user specifies \code{popParam=c(m, n)}.  

The WiSE bootstrap sample is created under the null hypothesis for a set threshold, \code{J0=j}.  The sampling scheme is described in detail in Braverman et al.  The distributon of the bootstrap sample of the parameters allows for calculation of a p-value associated with the null hypothesis.

Some notation to aid in understanding outputs:

1) \eqn{a, b}:  estimates of \eqn{\alpha, \beta} from the data wavelet coefficients

2) \eqn{a*, b*}:  estimates of \eqn{\alpha, \beta} from the bootstrap wavelet coefficients

3) \eqn{g_{xj}, g_{yj}}:  estimates of \eqn{\gamma_x, \gamma_y} from the data at the threshold \code{J0=j}

4) \eqn{g*_{xj}, g*_{yj}}:  estimates of \eqn{\gamma_x, \gamma_y} from the bootstrap sample at the threshold \code{J0=j}
}


\value{
\item{AsymptoticPValue }{ the asymptotic p-value based upon Hotelling's T^2. }
\item{BootstrapPValue }{ the bootstrap p-value. }
\item{dataSlope}{ the estimated slope of the wavelet coefficients from the data.  In the notation from Details, \eqn{b}}
\item{dataIntercept}{ the estimated intercept of the wavelet coefficients from the data.  In the notation from Details, \eqn{a}}
\item{bootSlope}{ the estimated slopes of the wavelet coefficients from the bootstrap samples.  In the notation from Details, \eqn{b*}.  This is a vector of length \code{R}.}
\item{bootIntercept}{the estimated intercepts of the wavelet coefficients from the bootstrap samples.  In the notation from Details, \eqn{a*}.  This is a vector of length \code{R}.}
\item{YWavelet}{ the estimated wavelet coefficients from the \code{Y} data.  In the notation from Details, \eqn{g_{yj}}.  This is a vector of length \eqn{2^(J0 + 1) - 1}.  The first entry is the level 0 coefficient, ..., final entries are the level \eqn{J0} coefficients.}
\item{XWavelet}{the estimated wavelet coefficients from the \code{X} data.  In the notation from Details, \eqn{g_{xj}}.  This is a vector of length \eqn{2^(J0 + 1) - 1}.  The first entry is the level 0 coefficient, ..., final entries are the level \eqn{J0} coefficients.}
\item{bootYWavelet}{the estimated wavelet coefficients from the \code{Y} bootstrap sample.  In the notation from Details, \eqn{g*_{yj}}.  This is a matrix with \code{R} rows and \eqn{2^(J0 + 1) - 1} columns which correspond to the wavelet coefficients.  The first column is the level 0 coefficient, ..., final columns are the level \eqn{J0} coefficients.}
\item{bootXWavelet}{the estimated wavelet coefficients from the \code{X} bootstrap sample.  In the notation from Details, \eqn{g*_{xj}}.  This is a matrix with \code{R} rows and \eqn{2^(J0 + 1) - 1} columns which correspond to the wavelet coefficients.  The first column is the level 0 coefficient, ..., final columns are the level \eqn{J0} coefficients.}
}

\references{
The WiSE bootstrap hypothesis test is implemented as an analysis tool in Braverman, A. et al. "Probabilistic Climate Model Evaluation" (in progress).
}

\author{
Megan Heyman
}

\seealso{
\code{\link{padMatrix}}, \code{\link{padVector}}, \code{\link[wavethresh]{wavethresh-package}}
}
\examples{
##Test whether \alpha=0 and \beta=1 for AIRS and IPSL Run 1 at 60E
## R=10 bootstrap samples is not recommended.  For demonstration only.
data(CM20N20S60E)
padData <- padMatrix(CM20N20S60E)
hypTest <- WiSEHypothesisTest(padData$xPad[,1], padData$xPad[,2], J0=5, R=10, 
                              XParam=padData$linearParam[,1], YParam=padData$linearParam[,2], 
                              plot=TRUE)
}

\keyword{ distribution }
\keyword{ nonparametric }
\keyword{ htest }
