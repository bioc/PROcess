\name{align}
\alias{align}
\title{Align peaks for a specified precision.}
\description{
A vector of peaks are expanded to a collection of
intervals, 'eps' * m/z of the peak to the left and
right of the peak position.  They are then aligned
using the method of intersection graphs by Gentleman
and Vandal.  
}
\usage{
align(pVec, eps = 0.003)
}
\arguments{
  \item{pVec}{A vector of peaks.}
  \item{eps}{A user specified precision of peak position.}
}
\value{
A vector of aligned peaks.
}
\references{R. Gentleman and A. C. Vandal,
Computational Algorithms for Censored-Data Problems Using 
Intersection Graphs. (2001), J. Comput. Graphic Statist.,
    vol. 10, p403--421.
 }
\keyword{manip}
