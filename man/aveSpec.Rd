\name{aveSpec}
\alias{aveSpec}
\title{Compute mean spectrum of a set of spectra}
\description{
Compute mean spectrum of a set of spectra.
}
\usage{
aveSpec(nVec)
}
\arguments{
  \item{nVec}{A character vector of file names.}
}
\details{
'aveSpec' computes the point-wise mean of intensities of 
a set of spectra over the whole range of the m/z values.
}
\value{
A matrix with 2 columns, the first being the m/z values
and the 2nd being the average of intensities corresponding
to the m/z value in the first column of the same row.
}
\author{Xiaochun Li}
\examples{
testfs <- dir(system.file("Test", package = "PROcess"),
	full.names=TRUE)
testAve <- aveSpec(testfs)
}
\keyword{arith}
