\name{getPeaks}
\alias{getPeaks}
\title{Peak Detection}
\description{
For given threshold criteria, find peaks.
}
\usage{
getPeaks(bseoffM, peakinfofile,SoN = 2,span = 81,sm.span=11, 
zerothrsh=2, area.w = 0.003, ratio = 0.2)
}
\arguments{
  \item{bseoffM}{a matrix holding the baseline-substracted
          spectra, with  row-names as the m/z values and
          column-names  as the spectrum names.}
  \item{peakinfofile}{a `.csv' file in the same format as  
		Ciphergen's peak info file, with 5 columns 
		data.  More details later.}
  \item{SoN}{see isPeak().}
  \item{span}{see isPeak().}
  \item{sm.span}{see isPeak().}
  \item{zerothrsh}{ignore peaks whose intensity values are below zerothrsh.}
   \item{area.w}{see isPeak().}
  \item{ratio}{see isPeak().}
}
\details{
For given threshold criteria, detect peaks and write the 
following columns of information into 'peakinfofile', 
spectrum name (Spectrum.Tag), spectrum sequential number
(Spectrum.), peak sequential number within a spectrum (Peak.),
relative intensity (Intensity) and the m/z value where the 
relative intensity occurs (Substance.Mass).
}
\author{Xiaochun Li}
\seealso{\code{\link{rmBaseline}}}
\examples{
example(renorm)
peakfile <- paste(tempdir(),"testpeakinfo.csv", sep="/")
getPeaks(rtM, peakfile)
}
\keyword{nonparametric}
