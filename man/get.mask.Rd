\name{get.mask}
\alias{get.mask}
\alias{mkSmall}
\alias{set.mask<-}
\alias{mkMaskObject}
\title{Create, access and manipulate spatial masks}
\description{
 Spatial masks are stored using the xyz-list structure used by
 \code{\link{image}} or as a series of masks stored as bits in the z-component
 as matrix or array object.     \code{get.mask} is used to extract a specific
 mask from the binary storage, and mkSmall can be used to quickly down-sample
 an existing mask or image.
}
\usage{
get.mask(masks, k)

mkSmall(lst, thin = 10)

set.mask(object, segment) <-  value

mkMaskObject(xs, ys, nsegs)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

  %  % get.mask
  \item{masks}{ A list object with components x, y, and z containing spatial masks }
  \item{k}{ specifies the k-th mask}

  %% mkSmall
  \item{lst}{ an xyz-list structure with z containing either a matrix or array }
  \item{thin}{ integer factor to down-sample grid }

  % %% set.mask
  \item{object}{ array Mask object }
  \item{segment}{ segment number to be modified in the mask }
  \item{value}{ individual mask to be set }

  %%mkMaskObject
  %\item{xs){ x coordinates of mask cells }
  %\item{ys}{ y coordinates of mask cells }
  %\item{nsegs}{ number of segments to be represented }
  \item{xs}{
    x coordinates of mask cells
}
  \item{ys}{
    y coordinates of mask cells
}
  \item{nsegs}{
    number of segments to be represented
}
}

\value{matrix of type logical}

\author{ Michael D. Sumner}

\seealso{ \code{\link{mkLookup}} for the use of these masks to query individual
locations and locations measured over time.
See \code{\link{bits}} for the underlying mechanism to set and get mask bits.

For the use of the xyz-list structure see \code{\link{image}}.  }
\examples{

      data(volcano)
      d <- list(x = seq(-10, 10, length = nrow(volcano)),
                y = seq(-5, 5, length = ncol(volcano)),
                z = array(0L, c(nrow(volcano), ncol(volcano), 2)) )
      mv <- min(volcano)

      for (i in 0:61) {
        blk <- (i \%/\% 31) + 1
        bit <- (i - 1) \%\% 31
        bits(d$z[,,blk], bit) <- volcano > (mv + i*1.6 )
      }
      for (i in 0:61) image(get.mask(d, i))

      ## an object with 62 masks is only twice the size of the source data
      object.size(d) / object.size(volcano)

      ## plot a smaller version
      image(get.mask(d, 20), 5)


}
\keyword{manip }% at least one, from doc/KEYWORDS

