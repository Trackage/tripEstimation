\name{mkCalibration}
\alias{mkCalibration}

\title{Create calibration of solar elevation to measured light level.}
\description{
  Using a set of light level data from a known location create a calibration
  function to return the expected light level given solar elevation. 
}
\usage{
mkCalibration(x, known = NULL, elim = c(-36, 12), choose = TRUE)
}
\arguments{
  \item{x}{ a data frame containing at least \code{gmt} and \code{light}}
  \item{known}{ a known position - as a 2-element \code{c(x, y)} coordinate }
  \item{elim}{ a 2-element vector of the range of solar elevation to define }
  \item{choose}{ logical -  choose segments from a plot or use all the data?}
}
\details{
  It is assumed that the data frame \code{x} has columns "gmt" with POSIXct
  date-times and "light" with numeric light level data. 
}
\value{
 A function, defined by \code{approxfun}. 
}

\author{ Michael D. Sumner}

\seealso{\code{\link{approxfun}} }
\keyword{ manip }
\keyword{ dplot }
