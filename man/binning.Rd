\name{binning}
\alias{binning}
\title{Binning the Mass Spectrometry Data}
\description{
The function 'binning' takes a matrix of  
spectra and constructs a matrix of intensities of reduced
dimensions based on a equally spaced mesh of 'breaks' 
intervals over the entire m/z range.
}
\usage{
binning(Ma, breaks = 400)
}
\arguments{
  \item{Ma}{a matrix of spectra by column. }
  \item{breaks}{number of bins, or equally-spaced intervals for
the image of reduced resolution}
}
\value{
A matrix of dimensions c(dim(Ma)[1], 'breaks').
}
\seealso{See Also \code{\link{gelmap}}.}
\keyword{manip}
