\name{bits}
\alias{bits}
\alias{bits<-}
\title{ Set and get bits from binary masks.  }
\description{
  Utility functions to access bits from numeric values,
  for the efficient storage of spatial masks.
}
\usage{
bits(object, bit)

bits(object, bit) <-  value
}

\arguments{
  %% bits
  \item{object}{ a numeric value }
  \item{bit}{ the desired bit}

  %% bits<-

  \item{value}{ logical value to set bit to }
}
\details{
  \R uses 32-bit integers, so we can (easily) access 31 binary matrices in each numeric
  matrix.   This is very useful for storing long time-series of spatial masks,
  required for track-location estimation from archival tags.

}
\value{
 A numeric object with the given bit set, or a logical value designating the
 status of the given bit.
}

\author{ Michael D. Sumner }
\note{ The 32nd bit is harder to access, so we ignore it.
}
\seealso{ See Also  \code{\link{get.mask}} for a higher level access of a mask object }
%and \{code\link{\%\%}} for integer division.  }
\examples{
a <- 1L
bits(a, 0)  ## 1
bits(a, 2) <- 1
a   # 5

}
\keyword{manip}% at least one, from doc/KEYWORDS

