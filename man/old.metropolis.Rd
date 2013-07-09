\name{old.metropolis}
\alias{old.metropolis}
\alias{old.dist.gc}
\alias{old.find.init}
\alias{old.mkLookup}
\alias{mkElevationSeg}
\alias{mkNLPosterior}
\alias{k.prior}
\title{Older versions of solar location estimation}
\description{
  Some deprecated functions, originally used purely for light level estimation before
  the sampling algorithm was generalized for satellite models as well.
  
}
\usage{
mkElevationSeg(segments, day)

mkNLPosterior(segments, day, light, calib)

old.dist.gc(x1, x2 = NULL)

old.find.init(mask, nseg, nlpost, pars = c("Lon", "Lat", "k"))

old.metropolis(nlpost, lookup, p0, cov0, start, end, iter = 1000, step = 100)

old.mkLookup(x, binArray = TRUE)

k.prior(seg, ps)
}

\arguments{
  %% mkElevationSeg
  \item{segments}{ vector identifying the segment of each time and light
  value}
  \item{day}{ date-time values in POSIXct }
  
  %% mkNLPosterior

  \item{light}{vector of light data }
  \item{calib}{calibration function for light levels}
  %% mkElevationSeg
  
  %% old.dist.gc
  \item{x1}{matrix of track locations}
  \item{x2}{ matrix of track locations (optional second part) }
  
  %% old.find.init
  \item{mask}{ image object of masked areas }
    \item{nseg}{ number of (twilight) segments }
    \item{nlpost}{ negative log posterior function}
  \item{pars}{ names of parameters }
  
  %% old.metropolis
  
    \item{lookup}{ lookup function for masked areas }
    \item{p0}{ initial locations for sampler }
    \item{cov0}{ covariance matrix for sampler }
    \item{start}{ known start parameters}
    \item{end}{ known end parameters}
    \item{iter}{ number of iterations }
  \item{step}{ number of thinning iterations per \code{iter}}
  
  %% old.mkLookup
   \item{x}{ image-like object of matrix or array of binary masks }
   \item{binArray}{ logical: are the masks compressed into bits? }

    \item{seg}{ segment }
  \item{ps}{ light attenuation value  }
}
\details{
  These functions are included for legacy purposes, this was the
  original implementation. 

}

\value{
  If it is a LIST, use


}

\author{Michael D. Sumner}

\seealso{ Please use the more up to date function
  \code{\link{metropolis}}, with the models such as
  \code{\link{solar.model}} or \code{\link{satellite.model}}.  }


\keyword{ misc }
