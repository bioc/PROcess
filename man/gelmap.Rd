\name{gelmap}
\alias{gelmap}
\title{Plot the image of a set of spectra}
\description{
This function takes its argument as a intensity matrix of
a set of spectra and plots the image as a heatmap.
}
\usage{
gelmap(Ma, cols = gray(seq(1, 0, by = -0.01)), at.mz = NULL, at.col = NULL, cexCol = 0.2 + 1/log10(nc))
}
\arguments{
  \item{Ma}{a matrix of intensities; spectra are arranged column-wise.}
  \item{cols}{a vector of color to represent continuum of intensities.}
  \item{at.mz}{a vector of m/z values where labels are desired.}
  \item{at.col}{a vector of sample indices, useful to label particular samples. If NULL, samples will be labelled from down up by 1:ncol(Ma). }
  \item{cexCol}{text size of the sample labels.}
}
\seealso{See Also \code{\link{binning}}.}
\keyword{hplot}
