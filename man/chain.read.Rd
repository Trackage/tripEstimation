\name{chain.read}
\alias{chain.read}
\alias{chain.dim}
\alias{chain.write}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Manage MCMC cache.  }
\description{
  These functions read and write to cache files for storing long MCMC
  outputs from model functions, such as \code{\link{solar.model}} or
  \code{\link{satellite.model}}.
}
\usage{
chain.read(filename)
chain.dim(filename)
chain.write(filename, A, append = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ cache file for model chain }

  \item{A}{ chain array }
  \item{append}{ append to existing file or overwrite? }
}

\value{
\code{chain.read} returns the actual array of MCMC samples from an archived file

\code{chain.dim} reports the dimensions of the archived file

\code{chain.write} writes an array of MCMC samples to an archive file

}
\author{Michael D. Sumner and Simon Wotherspoon}



\seealso{\code{pimg.list}}

\keyword{ manip }% at least one, from doc/KEYWORDS

