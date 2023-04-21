\name{solar.model}
\alias{solar.model}
\title{ Function to create a solar model object for metropolis location sampler }
\description{
 A solar model to manage likelihood functions, environmental masks and behavioural
likelihood functions. There are several options for configuring the model, and this
may be considered a template for any given model. The model \emph{function} exists
simply to make the object construction simple.
}
\usage{
solar.model(segments, day, light,
	proposal.x, proposal.z, mask.x, mask.z,
	fix.release = TRUE, fix.recapture = TRUE,
	calibration,
	light.sigma = 7, k.sigma = 10,
	behav = "speed", behav.dist = "gamma",
	behav.mean, behav.sd,
	proj.string = "+proj=longlat",
   ekstrom = c(-5, 3, light.sigma),
         ekstrom.limit = "light")
}
\arguments{
  \item{segments}{ vector identifying twilight segment}
  \item{day}{ vector of date-times for each light level}
  \item{light}{ vector of light levels }
  \item{proposal.x}{ function from object managing X proposals }
  \item{proposal.z}{ function from object managing Z proposals }
  \item{mask.x}{ lookup function for X's against masks }
  \item{mask.z}{ lookup function for Z's against masks }
  \item{fix.release}{ logical - is the release point known? }
  \item{fix.recapture}{ logical - is the recapture point known? }
  \item{calibration}{ calibration function for predicted light level for solar elevation}
  \item{light.sigma}{ variance for light data }
  \item{k.sigma}{ variance for light attenuation }
    \item{behav}{ model distributions to be used for behaviour - defaults to "speed"}
  \item{behav.dist}{ distribution to be used for behaviour }
  \item{behav.mean}{ mean for behavioural distribution }
  \item{behav.sd}{ variance for behavioural distribution }
  \item{proj.string}{ PROJ.4 string for coordinate system used }
    \item{ekstrom}{ parameters to use for ekstrom limit - min elevation, max elevation, sigma for outside that range }
  \item{ekstrom.limit}{ mode of ekstrom limit to impose - defaults to "light" }
}
\details{
The vectors of \code{segments}, \code{day} and \code{light} are expected to
be of the same length.

Fixed recapture and release points are treated specially for ease of sampling, but
the sampling is written to be general for any fixed locations.

Behavioural models may be specified either as lognormal or log-gamma. By editing the
function created as \code{logp.behavioural} this may be specified differently.

Transformation of coordinates is supported via a simple function that only performs coordinate
transforms if \code{proj.string} is not longlat.
}
\value{

  proposal.x(x) - generates new proposals for the x from the current
  x.  Generates all x at once.

  proposal.z(z) - generates new proposals for the x from the current
  z.  Generates all z at once.

  mask.x(x) - mask function for the x. Simultaneously tests all x and
  returns a vector of booleans indicating which are acceptable.

  mask.z(z) - mask function for the z. Simultaneously tests all z and
  returns a vector of booleans indicating which are acceptable.

  logp.position(x) - Given the set of x, returns a vector that gives
  the contribution each x make to the log posterior based on position
  alone.

  logp.behavourial(k,xa,z,xb) - Computes the contribution to the log
  posterior from the behavioural model on a subset of segments that
  make up the path.  Here k is a vector of the segment numbers, where
  the segments pass from xa to z to xb, and the function returns the
  contribution to the log posterior from each segment. This is the
  only function expected to work with only a subset of the x and z.

  start.x - suggested starting points for the x

  start.z - suggested starting points for the z

  The only function that must operate on a subset of the x/z is
  logp.behavourial.  All the other functions operate on all x or z
  simultaneously, simplifying the implementation for the user.

  Note that x can consist of several parameters, not just the
  locations, but we assume the first two components of each x specify
  the location.  For example, in the light level models each x is
  (lon,lat,k) where k is the attenuation of the light level.


  Some details of this implementation are not as nice as they could
  be.  First, it would be better if did not calculate the
  contributions to the posterior for points the mask rejects. Also,
  it may be better to separate the specification of the functions
  that generate proposals from the other functions, so that we can
  tune the proposal distributions without re-generating the whole
  model specification.

}

\author{ Simon Wotherspoon and Michael Sumner}

\keyword{models}
