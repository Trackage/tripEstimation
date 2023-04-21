\name{satellite.model}
\alias{satellite.model}
\title{ Function to create a satellite model object for metropolis location sampler }
\description{
 A model to manage likelihood functions, environmental masks and behavioural
likelihood functions for pre-derived satellite locations. There are some options for configuration,
but this
may be considered a template for any given model. The model \emph{function} exists
simply to make the object construction simple.
}
\arguments{
  \item{day}{ vector of date-times for each light level }
  \item{X}{ matrix of pre-derived satellite locations }
  \item{proposal.x}{ function from object managing X proposals }
    \item{proposal.z}{ function from object managing Z proposals }
    \item{mask.x}{ lookup function for X's against masks }
    \item{mask.z}{ lookup function for Z's against masks }
    \item{fix.release}{ logical - is the release point known? }
  \item{fix.recapture}{ logical - is the recapture point known? }
  \item{start.x}{ starting positions for the primary locations, see \code{\link{position.logp}} }
  \item{start.z}{ starting positions for the intermediat locations. }
  \item{posn.sigma}{ variance for locations }
  \item{behav.dist}{ distribution to use for behavioural constraint }
  \item{behav.mean}{ mean to use for behavioural distribution }
  \item{behav.sd}{ variance for behavioural distribution}
  \item{proj.string}{ PROJ.4 string for coordinate system used }
}
\details{
\code{posn.sigma} may be a single value for all estimates, or a vector of values for each position
estimate.

Transformation of coordinates is supported via a simple function that
only performs coordinate transforms if \code{proj.string} is not
longlat.
}

\value{ See \code{solar.model} for some related detail.  }

\references{Sumner, Wotherspoon and Hindell (2009). Bayesian Estimation of Animal Movement from Archival and Satellite Tags, PLoS ONE. \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0007324}}
\author{ Michael D. Sumner }

\note{


These are simple wrapper functions to create the desired model for use
in \code{\link{metropolis}}. These models are structurally very simple
and may be easily edited as required.  }

\seealso{ See also \code{\link{solar.model}} for the counterpart model
for estimating positions for light tags. }

\keyword{ manip }
