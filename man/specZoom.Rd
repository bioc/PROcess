\name{specZoom}
\alias{specZoom}
\title{Plotting a Spectrum with Peaks}
\description{
Function for plotting an object returned by \code{isPeak}.
}
\usage{
specZoom(pks, xlim = NULL, cols = c("cyan", "red", "black"))
}
\arguments{
  \item{pks}{an object (a list) returned by \code{isPeak}.}
  \item{xlim}{a range of m/z values over which a zoomed-in view of the spectrum is desired.}
  \item{cols}{a vector of color specification for the smooth (signal), peaks and local noise.}
}
\examples{
example(isPeak)
specZoom(pkobj, xlim=c(5000, 10000))
}
\keyword{hplot}
