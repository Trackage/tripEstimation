\name{astro}
\alias{astro}
\alias{EQUHOR}
\alias{FRAC}
\alias{LMST}
\alias{lunar}
\alias{mini.sun}
\alias{MJD}
\alias{POLAR}
\title{Calculations for position of the sun and moon}
\description{
 This set of functions provides simple position calculations for the sun and moon,
 taken from Pascal routines published in Montenbruck and Pfleger (1994, Dunlop).

 These are completely independent from the (specifically optimized) solar elevation
 calculations available via [elevation and solar].
}
\usage{
astro(lon, lat, astro.calc)

EQUHOR(DEC, TAU, PHI)

FRAC(x)

LMST(MJDay, LAMBDA)

lunar(time)

mini.sun(time)

MJD(date)

POLAR(X, Y, Z)
}

\arguments{
   %% astro
   \item{lon}{vector of \code{lon}gitudes}
   \item{lat}{vector of \code{lat}itudes}
  \item{astro.calc}{list object containing \code{RA} right ascension }

  %% EQUHOR
  \item{DEC}{declination}
  \item{TAU}{\code{TAU}  }
  \item{PHI}{ \code{PHI}  }

  %% FRAC
  \item{x}{number}

  %% LMST
  \item{MJDay}{ modified julian day }
  \item{LAMBDA}{\code{LAMBDA} }

  %% lunar / mini.sun
  \item{time}{ vector of date-times in POSIXct format }

  %% MJDay
  \item{date}{ vector of date-times in POSIXct format }

  %% POLAR
  \item{X}{ x-coordinate}
  \item{Y}{ y-coordinate }
  \item{Z}{ z-coordinate }

}

\value{
 \code{astro} returns a list object with the components of the moon or sun's
 position,
  \item{r }{rho component }
  \item{theta }{theta component - elevation}
  \item{phi}{phi component - azimuth}
}



\references{\preformatted{@BOOK{,
  title = {Astronomy on the Personal Computer},
  publisher = {Springer-Verlag, Berlin},
  year = {1994},
  author = {Oliver Montenbruck and Thomas Pfleger},
  edition = {2  (translated from German by Storm Dunlop)},
}}}
\author{Michael D. Sumner}
\note{
  Thanks to Nick.Ellis@csiro.au for pointing out a mistake pre-0.0-27
}
\section{Warning }{
       Some of this could be faster (particularly the use of LMST in "astro" is not precalculated)

}

\seealso{ See Also \code{\link{elevation}}}
\examples{

## the moon
tm <- Sys.time() + seq(by = 3600, length = 100)
moon <- lunar(tm)
rtp <- astro(147, -42, moon)
op <- par(mfrow = c(2,1))
plot(tm, rtp$theta, main = "lunar elevation, Hobart")
plot(tm, rtp$phi, main = "lunar azimuth, Hobart")
par(op)

## the sun
tm <- Sys.time() + seq(by = 3600, length = 100)
sun <- mini.sun(tm)
rtp <- astro(147, -42, sun)
op <- par(mfrow = c(2,1))
plot(tm, rtp$theta, main = "solar elevation, Hobart")
plot(tm, rtp$phi, main = "solar azimuth, Hobart")
par(op)
  elev.gmt <- mkElevationSeg(1, tm)
  plot(tm, rtp$theta, main = "solar elevation mini.sun versus NOAA")
  lines(tm, elev.gmt(1, 147, -42))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

