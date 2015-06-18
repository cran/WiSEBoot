\name{deSeasonalize}
\alias{deSeasonalize}

\title{
De-seasonalize daily, monthly, or data series with IDs 
}

\description{
Generally, this function performs data standardization by an ID.  It is useful for data pre-processing, by removing daily, monthly, or other periodic means which are not of interest.  Two quick methods are available:  removing the mean and standardization.  Note, this is not the optimal method in the statistical literature for de-seasonalization.  
}

\usage{
deSeasonalize(dates, X, type = "daily", method = "deMean")
}


\arguments{
  \item{dates}{
vector of dates or IDs for the data, \code{X}.  This vector must have the same length as \code{X} (or the same number of rows as \code{X}).  A recognized R date format is required unless the \code{method="custom"} option is called.  Missing values are not allowed.  More than 1 replicate of the ID is recommended, as this is the criteria to remove the mean/standardize.
}
  \item{X}{
vector or matrix of all data to be de-seasonalized.  Missing values are not allowed and the data should be numeric.  The length, or number of rows, of \code{X} should match the length of \code{dates}.  Thus, if \code{X} is a matrix, the columns of \code{X} contain the individual data series.
}
  \item{type}{
how often to de-seasonalize.  Allowed values are \code{"daily"}, \code{"monthly"}, and \code{"custom"}.  The \code{"daily"} or \code{"monthly"} de-seasonalization options require that the \code{dates} vector be of a recognized R date format.  The \code{"custom"} option allows for de-seasonalization at other rates (e.g. quarterly, hourly, etc.), or, more generally, standardization by an ID.  
}
  \item{method}{
how de-seasonalization is implemented.  Allowed values are \code{"deMean"} and \code{"standardize"}.  The \code{"deMean"} method removes the mean by \code{dates}.  The \code{"standardize"} method standardizes (i.e. removes the mean and divides by the standard deviation) by \code{dates}.
}


}
\details{
For a supplied matrix, \code{X}, the same \code{dates} -- ID criteria -- is used for de-seasonalization in each column.  That is, each column of \code{X} is de-seasonalized by \code{dates}.

This is not an optimal de-seasonalization methodology.  For users interested in a more robust method, please see the '\code{deseasonalize}' R package.
}

\value{
a vector or matrix of the same dimension as \code{X} which has been de-seasonalized appropriately.
}


\author{
Megan Heyman
}

\seealso{
R package 'deseasonalize', \url{http://cran.r-project.org/package=deseasonalize}
}

\examples{
ID <- as.Date(c("2014-05-14", "2013-06-20", "2013-05-14", "2012-06-20",
                "1999-09-09", "1998-09-08", "1998-09-09", "1982-05-14", 
                "2000-09-08"))
someData <- seq(1, 9)

deSeasonalize(dates=ID, X=someData, type="daily", method="deMean") 
deSeasonalize(dates=ID, X=someData, type="monthly", method="standardize")
}

\keyword{ arith }
\keyword{ manip }
