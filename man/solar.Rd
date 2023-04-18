\name{solar}
\alias{solar}
\title{ Calculate solar postion parameters}
\description{
	Pre-calculates astronomical solar position components for
	Earth-location sampling functions.
}
\usage{
solar(day)
}
\arguments{
  \item{day}{ vector of date-time values }
}

\value{
A list of the following values for each input time:
  \item{solarTime }{solar time}
  \item{sinSolarDec }{sine solar declination}
  \item{cosSolarDec}{cosine solar declination}
}
\references{ \url{https://gml.noaa.gov/grad/solcalc/azel.html}}
\author{ Michael D. Sumner }
\note{
No account is made for horizon refraction, but this was available in the original
(Javascript) code.
}

\keyword{ manip }
