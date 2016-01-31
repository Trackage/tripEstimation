\name{mkLookup}
\alias{mkLookup}

\title{ Create a lookup function to query locations against spatial masks}
\description{
  Simple pixel spacing is used to overlay point locations on a spatial grid, or
  a series of grids. 
}
\usage{
mkLookup(x, by.segment = TRUE)
}

\arguments{
  \item{x}{ an xyz-list with matrix or array of masks }
  \item{by.segment}{logical - is the mask to be queried separately for each time step? }
}

\value{
A function, with one argument - a matrix of points - that returns a logical
 vector indicating the overlay of each point against the masks. 
}

\author{ Michael D. Sumner}
\note{ 
Very little error checking is done.
}
\seealso{ \code{\link{get.mask}} and related examples for creating and using 
masks. 

See \code{\link[sp]{over}}  for more general capabilities for overlays. }

\keyword{manip }% at least one, from doc/KEYWORDS

