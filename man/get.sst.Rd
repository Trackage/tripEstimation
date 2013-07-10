\name{get.sst}
\alias{get.sst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read SST data from online service. 
}
\description{
Functions to read SST data from online services, by specifying spatial
and temporal bounds. 
}
\usage{
get.sst(xlim = NULL, ylim = NULL, tlim = NULL,
server = "http://atlas.nmfs.hawaii.edu/cgi-bin/reynolds_extract.py")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlim}{
 Longitude range of SST to read. 
}
  \item{ylim}{
 Latitude range of SST to read. 
}
  \item{tlim}{
 Time range of SST to read. 
}
  \item{server}{
 Server to read from. 
}
}
\details{
  This functionality was originally taken from the kfsst package by
  Anders Nielsen.
  
Longitude \code{xlim} range may be specified in [-180, 180] or [0, 360]
convention. The result is given in [-180, 180].

Time range must be given in POSIxct form. 
}
\value{
A list object with the temperature data and coordinates. 
%%  If it is a LIST, use
  \item{x,y,t}{ Coordinates of the temperature data }
  \item{z}{ Matrix or array of temperature data }

}
\references{
Original concept taken from kfsst: \url{http://www.soest.hawaii.edu/tag-data/tracking/kfsst/}
}
\author{
Michael D. Sumner
}

\seealso{
See Also \code{\link{mkMaskObject}} for the use of these data for masks. 
}
\examples{
\dontrun{
d0 <- read.url.sst(tlim = range(ISOdatetime(2009, 1:3, 1, 0, 0, 0)))
image(d0$x, d0$y, d0$z[,,1], axes = TRUE)


}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ spatial }% __ONLY ONE__ keyword per line
