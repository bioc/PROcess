\name{quality}
\alias{quality}
\title{Quality Check on a Set of Spectra}
\description{
Compute three quality parameters for a set of spectra.
}
\usage{
quality(csvfldr, peakinfofile, cutoff)
}
\arguments{
  \item{csvfldr}{a path to where the baseline-subtracted spectra are stored.}
  \item{peakinfofile}{a `.csv' file in the same format as 
	Ciphergen's peak info file, with 5 columns data. See Details of \code{getPeaks}.}
  \item{cutoff}{The point in m/z below which spectra are cutoff.}
}
\details{
The quality parameters are computed a la fashion de Mani.
1. Estimate noise by moving average with a 5 point window.
2. Estimate the noise envelop by 3 times the standard deviation of noise in a 251 point moving window.
3. Compute the area under the baseline-subtracted curve, area0.
4. Compute the area after subtracting the noise envelop from the baseline-subtracted curve, area1.
5. Parameter 'Quality' is defined as area1/area0.
6. Parameter 'Retain' is defined as the number of points with height above 5 times the noise envelop over total number of points in the spectrum.
7. Detect peaks in each spectrum by \code{getPeaks} or Ciphergen software.
8. Parameter 'peak' is defined as the number of peaks in a spectrum divided by the mean number of peaks across spectra.

A spectrum is considered to be of poor quality if Quality<0.4, Retain<0.1 and peak<0.5 simultaneously.
}
\value{
A matrix with three named columns, 'Quality','Retain' and 'peak', with spectrum file names as row names.
}
\references{Mani Refn? }
\author{Xiaochun Li}
\examples{
example(getPeaks)
qualRes <- quality(testdir, "testpeakinfo.csv", cutoff=1500)
}
\keyword{math}
