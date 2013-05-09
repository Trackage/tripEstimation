\name{norm.proposal}
\alias{norm.proposal}
\alias{mvnorm.proposal}
\alias{bmvnorm.proposal}
\title{Manage proposal functions tune variance for metropolis sampler}
\description{Generate new proposals for the x from the current. Generates all x at once.}
\usage{
norm.proposal(m, n, sigma)

mvnorm.proposal(m, n, Sigma)

bmvnorm.proposal(m, n, Sigma)
}
\arguments{
  \item{m}{ number of records }
  \item{n}{ number of parameters}
  \item{sigma}{ variance }
  \item{Sigma}{ variance }

}
\details{
norm.proposal - Independent Normal proposal - every component is independent, with
variances of individual components determined by sigma.  The
recycling rule applies to sigma, so sigma may be a scalar, an m
vector or a m by n matrix.

mvnorm.proposal - Multivariate Normal proposal - all components of all points are
 correlated.  In this case Sigma is the joint covariance of the m*n
 components of the proposal points.
 
 bmvnorm.proposal - Block Multivariate Normal proposal - components of points are
correlated, but points are independent. Here Sigma is an array of m
covariance matrices that determine the covariance of the m proposal
points.
}
\value{
  An list object with get, set and tune functions to manage the state of the proposals. 
  \item{proposal}{propose new set of parameters from last}
  \item{get}{get variance values }
  \item{set }{set variance values}
  \item{tune}{tune the variance for proposal functions}
}

\author{ Simon Wotherspoon }


\keyword{manip}% at least one, from doc/KEYWORDS

