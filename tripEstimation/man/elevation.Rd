\name{elevation}
\alias{elevation}
\title{ Calculate elevation of astronomical objects}
\description{
  Function to calculate elevation. 
}
\usage{
elevation(lon, lat, sun)
}
\arguments{
	\item{lon}{ vector of longitude values}
	\item{lat}{ vector of latitude values }
	\item{sun}{ pre-stored values  as returned by
	  \code{\link{solar}} or \code{\link{lunar}}}
}

\references{
\url{http://www.srrb.noaa.gov/highlights/sunrise/azel.html}
}
\author{Michael D. Sumner}

\keyword{manip}
