\name{as.image.pimg}
\alias{as.image.pimg}
\alias{as.matrix.pimg}
\alias{combine}
\alias{coords.pimg}
\alias{unzipper}
\alias{as.local.pimg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert to image list}
\description{
Converts Probability image (pimage) component to standard R xyz list
image.
}
\usage{
as.image.pimg(pimg)
combine(pimgs, subset = 1:length(pimgs))
coords.pimg(pimg)
unzipper(px)
as.local.pimg(pimg)
\method{as.matrix}{pimg}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pimg}{Probability image component}
    \item{pimgs}{ \code{pimgs}  }
    \item{subset}{ \code{subset} }

      \item{px}{ \code{px} }

      \item{x}{\code{x}}

      \item{\dots}{\code{\dots}}
}

\value{
\code{as.image.pimg} returns a \code{image} list with vectors x,y and z matrix

\code{as.matrix.pimg} returns just the local matrix populated in the parent

\code{combine} returns the collective matrix, in \code{image} xyz form

\code{coords.pimg} returns the rectilinear coordinates of the pimg parent

\code{unzipper} returns a \code{pimg.list} by combining multiple compatible ones together and resolving their temporal order

\code{as.local.pimg} returns the \code{pimg} in local form
}

\author{Michael D. Sumner}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
