\name{getPeaks2}
\alias{getPeaks2}
\title{Quantify peaks for individual spectra.}
\description{
For a vector of given peak locations, quantify peaks for 
individual spectra.
}
\usage{
getPeaks2(bseoffM, pVec, eps = 0.003)
}
\arguments{
  \item{bseoffM}{A matrix of intensities, with rows the 
m/z values and columns samples. }
  \item{pVec}{A vector of given peak locations.}
  \item{eps}{Relative precision of peak location.}
}
\value{
A matrix of intensities with rows the peaks 'pVec' and
column the samples. The m/z values of 'pVec' is stored
as the 'rownames' of the returned matrix.
}
\details{
Each peak is expanded to an interval, eps * m/z to 
the left and right of the peak.  Intensities of
individual spectra are quantified by the maxima 
in this interval.
}
\keyword{manip}
