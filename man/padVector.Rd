\name{padVector}
\alias{padVector}

\title{
Increase data length to the closest power of 2.
}

\description{
To use the WiSE bootstrap methodology in this package, data must be of length \eqn{T=2^J} for some positive integer, \eqn{J}.  This function increases the length of data to achieve the particular length requirement. Generally, this function is useful for data pre-processing. 
}

\usage{
padVector(X, type = "reflect", pad.direction = "both", 
          replaceLinearTrend = FALSE)
}

\arguments{
  \item{X}{
a vector of data.  This must be numeric and non-missing.
}
  \item{type}{
how to increase the data length.  Allowed values are \code{"reflect"}, \code{"periodic"}, or \code{"mean"}.  The \code{"reflect"} option repeats values proceeding the end/beginning of the series (ex:  \code{ 12345 --> 12345432}).  The \code{"periodic"} option repeats the series in order (ex:  \code{ 12345 --> 12345123}).  The \code{"mean"} option repeats the series mean (ex: \code{ 12345 --> 12345333} ).
}
  \item{pad.direction}{
where to add the data padding.  Allowed values are \code{"both"}, \code{"front"}, or \code{"rear"}.  The \code{"both"} option pads data on both sides of the series (ex:  \code{ 12345 --> 31234533}).  The \code{"front"} option pads data on the beginning of the series (ex:  \code{ 12345 --> 33312345}).  The \code{"rear"} option pads data on the end of the series (ex:  \code{ 12345 --> 12345333}).
}
  \item{replaceLinearTrend}{
logical.  If \code{TRUE}, the estimated linear trend is replaced in the returned data.  If \code{FALSE}, the estimated trend is not replaced in the returned data.  See Details for more information.
}
}
\details{
If the data supplied is already a power of 2, this function will just return the original or de-trended data.

If the data supplied is of length, \eqn{t}, the padded data returned will be of length \eqn{T=2^ceiling(log(t, base=2))}.

The data length \eqn{T=2^J} for a positive integer, \eqn{J}, requirement is associated with the discrete wavelet transform.  Although methodology exists in the wavelet literature which allows for data series of any length, this methodology does not align with the theory behind WiSE bootstrap.

The \code{replaceLinearTrend} option allows the user to control whether linear trend appears in the padded data.  The linear trend (by data index) is estimated using least squares for the data series.  This trend is removed before padding the data.  The estimated trend may or may not be replaced to the padded data.  The linear trend consists of the data intercept and slope (by index).
}

\value{
\item{ xPad }{a vector of padded data.  This vector will be of length \eqn{T} instead of \eqn{t} (see Details).}
\item{ origSeriesIndex }{ a vector of 2 indices.  These indicate where the original data is in the padded series.  Note, the \code{xPad} will not exactly match the original data between these indices, since the linear trend has been estimated (and possibly replaced).}
\item{ linearParam }{ a vector of 2 numbers.  The first is the least squares estimated intercept and the second is the least squares estimated slope (by index) from the data originally supplied.}
}

\author{
Megan Heyman
}


\seealso{
\code{\link{padMatrix}}, \code{\link[wavethresh]{wavethresh-package}} 
}

\examples{
someData <- seq(1,9)+ rnorm(9)

padVector(someData) 
padVector(someData, type="mean", pad.direction="rear") 
padVector(someData, type="periodic", pad.direction="front") 
}

\keyword{ manip }
\keyword{ arith }
