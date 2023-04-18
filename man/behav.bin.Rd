\name{behav.bin}
\alias{behav.bin}
\alias{bin.pimg}
\alias{chunk.bin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Bin MCMC chains.  }
\description{
  Bin MCMC chains in probability image summaries.
}
\usage{
behav.bin(z, pimgs, weights = NULL)
bin.pimg(pimg, xy, w = 1)
chunk.bin(filename, pimgs, weights = NULL, chunk = 2000, proj = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ \code{z}  }
  \item{pimgs}{  \code{pimgs}  }
  \item{weights}{  \code{weights}  }

    \item{pimg}{  \code{pimg}  }
  \item{xy}{  \code{xy}  }
  \item{w}{  \code{w}  }

    \item{filename}{  \code{filename}  }


  \item{chunk}{  \code{chunk}  }
  \item{proj}{  \code{proj}  }
}
\value{

\code{behav.bin} returns a \code{pimg.list}

\code{bin.pimg} and \code{chunk.bin} provide work flow for \code{behav.bin}, to do the local binning and
control the overal job


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
