\name{WiSEBoot}
\alias{WiSEBoot}

\title{
Wild Scale-Enhanced  (WiSE) Bootstrap for Model Selection
}

\description{
Perform WiSE bootstrap to estimate parameters within models of the form
\deqn{Y = \gamma_0 + \gamma_1 t + W\gamma  + e}
Automatically select a threshold level for \eqn{\gamma}, or the user may specify the threshold.  This function also provides the WiSE bootstrap samples of the wavelet coefficients (for the selected threshold) and bootstrap samples of the linear parameters.
}

\usage{
WiSEBoot(X, R, XParam = NA, TauSq = "log", bootDistn = "normal", by.row = FALSE, 
         J0 = NA, wavFam = "DaubLeAsymm", wavFil = 8, wavBC = "periodic")
}

\arguments{
  \item{X}{
a matrix or vector of equally-spaced data.  All entries must be non-missing and numeric.  If a vector is supplied, the length must be \eqn{T=2^J} where \eqn{J} is a positive integer.  If a matrix is supplied, each data series must be of length \eqn{T}.
}
  \item{R}{
number of bootstrap samples.  Allowed value is a positive integer.
}
  \item{XParam}{
vector or matrix of linear trend parameters for the data series.  If a vector is supplied in \code{X}, this should be a vector of length 2.  If a matrix is supplied in \code{X}, this should be a matrix with 2 rows and an equal of columns (\code{dim(X)[2]=dim(XParam)[2]}).  If \code{NA}, \code{WiSEBoot} will automatically estimate linear parameters via least squares.  This quantity may be supplied by the \code{linearParam} return argument of \code{padVector} or \code{padMatrix}.  See Details below.
}
  \item{TauSq}{
scale parameter for the bootstrap.  Allowed values are \code{"log", "log10", "sqrt", "2/5"}.  The scale parameter is related to the length of the data series.  For example, \code{"log"} implies a value of the scale parameter, \eqn{\tau}, of \eqn{sqrt(log(T))}.
}
  \item{bootDistn}{
the distribution for the bootstrap.  The only allowed value is \code{"normal"}.  This draws iid random samples from the N(0,1) distribution for the wild bootstrap.
}
  \item{by.row}{
logical indicator of observation location.  If \code{TRUE}, the observations are by row and the columns contain different data series.  If \code{FALSE}, the rows contain different data series and the observations are by column.
}
  \item{J0}{
wavelet filter coefficient threshold.  Allowed values are \code{NA} and any integer between 0 and \eqn{J-2} (when the data series is of length \eqn{T=2^J}).  If a specific integer is given, all wavelet coefficients at levels finer than \code{J0} are set to 0.  If \code{NA}, the WiSEBoot creates bootstrap samples for all thresholds between 0 and \eqn{J-2}.  The selected threshold minimizes the mean of the MSE.
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
}


\details{
The assumed data model is
\deqn{Y = \gamma_0 + \gamma_1 t + W\gamma  + e}
where \eqn{Y} is the data vector, linear parameters in time (\eqn{t}) are \eqn{\gamma_0} and \eqn{\gamma_1}, \eqn{\gamma} are the wavelet coefficients (scaling and filter), and W is the DWT for a fixed wavelet basis.  Note, in many cases of the DWT, the scaling coefficient is equivalent to \eqn{\gamma_0}, and thus, estimated there.

This model requires estimation of linear terms \eqn{\gamma_0} and \eqn{\gamma_1}. It is recommended, if the data is padded to a length \eqn{T=2^J} using \code{padVector} or \code{padMatrix}, to supply the \code{linearParam} estimates and call \code{replaceLinearTrend=FALSE}.   If \code{XParam} is left missing, the \code{WiSEBoot} function will estimate \eqn{\gamma_0} and \eqn{\gamma_1} from the supplied data using least squares.

\code{J0} sets the threshold within the wavelet coefficients.  Our threshold is defined as the level above which all fine wavelet coefficients are set to 0.

For a single data series, \eqn{Y}, the WiSE bootstrap sample is obtained by 

1.  Find estimates of \eqn{\gamma_0} and \eqn{\gamma_1}:  \eqn{g0} and \eqn{g1}.  If supplied, this is \code{linearParam}.

2.  Estimate all levels of wavelet coefficients, \eqn{\gamma}, using the residuals \eqn{r = Y - g0- g1 t}.  Call these estimated coefficients \eqn{g}.

3.  For a set threshold, \eqn{J0=j}, set all coefficients in \eqn{g} finer than \eqn{j} to 0. Call this thresholded set of coefficients \eqn{g_j} Perform the inverse wavelet transform with \eqn{g_j}.  This smooth series may be called \eqn{rSmooth}.

4. Calculate the wavelet residuals using \eqn{rWave = r - rSmooth}.

5. A single bootstrap sample is defined as \eqn{Y* = g0 + g1 t + W g_j  + \tau N(0,1) rWave}.  \eqn{Y*} is used to obtain estimates for the un-thresholded wavelet coefficients and linear parameters.
}
\value{
\item{MSECriteria }{matrix with 2 columns.  The first column contains integer values corresponding to various \eqn{J0 + 1}.  The second column contains the mean of the MSE.  The MSE is computed using the estimated (smooth) bootstrap sample and the original data.  The selected model minimizes the mean of MSE.  }
\item{ BootIntercept }{matrix of \code{R} rows.  Each row corresponds to a single bootstrap sample estimate of \eqn{\gamma_0}.  The number of columns in the matrix corresponds to the number of data series supplied to the function (in \code{X}).  The order of the data series supplied matches to the order of the columns of \code{BootIntercept}. }
\item{ BootSlope }{ matrix of \code{R} rows.  Each row corresponds to a single bootstrap sample estimate of \eqn{\gamma_1}.  The number of columns in the matrix corresponds to the number of data series supplied to the function (in \code{X}).  The order of the data series supplied matches to the order of the columns of \code{BootSlope}. }
\item{ BootWavelet }{ array of bootstrap estimates of the wavelet coefficients.  The first dimension is \code{R} (bootstrap sample).  The second dimension is the \eqn{T=2^J} (data series length).  The order of wavelet coefficients in the second dimension is:  scaling, mother, level 0 (coarsest), level 1, ..., level \eqn{J-1} (finest).  The third dimension is the number of data series supplied. This array does not contain any boundary coefficients generated using the \code{wavBC="symmetric"} option.}
\item{ DataWavelet }{ matrix of wavelet coefficients from the data.  This matrix only contains the coefficients from the selected model (fine level coefficients are set to 0).  The number of rows is \eqn{T=2^J} and the order of coefficients within these rows matches the order of coefficients in the columns of \code{BootWavelet}.  Thus, the first row contains the scaling coefficient, the second row contains the level 0 coefficient, etc.  The number of columns matches the number of data series supplied, and are ordered as in \code{X}.}
\item{ wavFam, wavFil, wavBC, TauSq, BootDistn, by.row }{ Same as supplied to the function. }
}
\references{
The WiSE bootstrap methodology is defined in theoretical detail in Chatterjee, S. et al. "WiSE bootstrap for model selection" (in progress).
}
\author{
Megan Heyman
}


\seealso{
\code{\link{padVector}}, \code{\link{padMatrix}}, \code{\link[wavethresh]{wavethresh-package}}
}
\examples{
##R=10 bootstrap samples is not recommended.  For demonstration only.

##bootstrap one of the simulated series, threshold level 4 (not the truth)
data(SimulatedSNR1.5Series)
bootObj <- WiSEBoot(SimulatedSNR1.5Series[, 3], R=10, J0=4)

#boxplot of the bootstrap intercept and slope estimates (both 0 in truth)
par(mfrow=c(1,2))
boxplot(bootObj$BootIntercept); boxplot(bootObj$BootSlope)

#boxplot of the bootstrap wavelet coefficient estimates, level 1
par(mfrow=c(1,2))
boxplot(bootObj$BootWavelet[ , 3, 1]); boxplot(bootObj$BootWavelet[ , 4, 1])


##See what smooth level the bootstrap chooses (truth is J0=2)
bootObj2 <- WiSEBoot(SimulatedSNR1.5Series[ ,3], R=10)
bootObj2$MSECriteria
}


\keyword{ nonparametric }
\keyword{ ts }
\keyword{ distribution }
\keyword{ models }
