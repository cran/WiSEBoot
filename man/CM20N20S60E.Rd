\name{CM20N20S60E}
\alias{CM20N20S60E}
\docType{data}
\title{
AIRS, IPSL, and MIROC5 Data at 60E
}
\description{
This matrix of data contains specific humidity observations from AIRS, 4 runs of the IPSL climate model, and 6 runs of the MIROC5 climate model.  All series have been gridded according to the IPSL model grid.  This data set contains 3012 observations at 60E between 20N and 20S at an altitude of 500 hPa.
}
\usage{data("CM20N20S60E")}
\format{
  The format is:
  
 num [1:3012, 1:11] 0.00176 0.0015 0.00152 0.00182 0.00198 ...
 
 - attr(*, "dimnames")=List of 2
 
  ..$ : chr [1:3012] "2002-10-01" "2002-10-02" "2002-10-03" "2002-10-04" ...
  
  ..$ : chr [1:11] "AIRS" "IPSLRun1" "IPSLRun2" "IPSLRun3" ...
}
\details{
The row and column names indicate the date and AIRS data or climate model output.  Dates of this particular data set are October 1, 2002 to December 29, 2010, and observations are recorded daily.  


}
\source{
The Atmospheric Infrared Sounder (AIRS) data is described and available at \url{http://airs.jpl.nasa.gov/data/overview}.

The Institut Pierre Simon Laplace (IPSL) model output is described at \url{http://icmc.ipsl.fr/}.

The Model for Interdisciplinary Research on Climate (MIROC5) model output is described at \url{http://www.icesfoundation.org/Pages/ScienceItemDetails.aspx?siid=181}.

Each data source is available on a different global latitude-longitudinal grid.  The data provided here was re-gridded and provided by Amy Braverman (NASA-JPL) and Matthew Gunson (NASA-JPL).
}

\examples{
data(CM20N20S60E)
}
\keyword{datasets}
