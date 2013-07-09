\name{as.image.pimg}
\alias{as.image.pimg}
\alias{as.matrix.pimg}
\alias{combine}
\alias{coords.pimg}
\alias{get.pimgs}
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
get.pimgs(rootdir = NULL, f2load = "p_ImageList.Rdata")
unzipper(px)
as.local.pimg(pimg)
\method{as.matrix}{pimg}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pimg}{Probability image component}
    \item{pimgs}{ \code{pimgs}  }
    \item{subset}{ \code{subset} }
      \item{rootdir}{ \code{rootdir}  }
      \item{f2load}{ \code{f2load}  }

      \item{px}{ \code{px} }

      \item{x}{\code{x}}

      \item{\dots}{\code{\dots}}
}

\author{Michael D. Sumner}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}