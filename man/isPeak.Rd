\name{isPeak}
\alias{isPeak}
\title{Locate Peaks in a Spectrum}
\description{
Find local maxima of a spectrum.
}
\usage{
isPeak(f,SoN = 2,span = 81,sm.span=11,plot=F,add = F) }
\arguments{
  \item{f}{a matrix of two columns representing a spectrum, with the 
		first column the m/z value and second the intensity}
  \item{SoN}{signal to noise ratio criterion for peak detection}
  \item{span}{parameter for estimating local variance before 
	peak detection; default is 81 points, that is, 40 
	points to the left and right of a point of which the 
	variance is being estimated.}
  \item{sm.span}{parameter for smoothing the spectrum before 
	peak detection; default is 11 points, that is, 5 
	points to the left and right of a point being smoothed}
  \item{plot}{logical, plot the smoothed spectrum and peaks? }
  \item{add}{add to the existing raw and baseline-substracted plot?}
}
\details{
A spectrum is smoothed first using the nearest `span' 
neighbours. A larger span `sm.span' is needed to estimate
the local variation so that it is not overestimated due to
the peaks nearby.  Then potential peaks are identified using 
Ripley's `peaks' function with `span' points.

Peaks that satify the conditions that the (smoothed) signal 
to noise ratio is greater than `SoN' and that the smoothed 
signal is greater than 1.64 * mad(sm) are returned.
}
\value{
A data frame with five components, `peak',`smooth',`mz' and
     `sigmas', each of length the number of rows in `f'. 
     `peak' is logical indicating whethere there is a peak or not 
     (Y/N), `smooth' the smooth of the spectrum, `mz' the same as 
     `f[,1]' and `sigmas' the estimates of local variation.  
}
\author{Xiaochun Li}
\examples{
fdat <- system.file("spectra/ssc", package="PROcess")
fs <- list.files(fdat, pattern="*csv*", full.names=T)

j <- 94

f1 <- read.csv(gzfile(fs[j]))

fcut <- f1[f1[, 1] > 0, ]

bseoff <- bslnoff(fcut, method = "loess", plot = T,
    bw = 0.1)

pkobj <- isPeak(bseoff,span=81,sm.span=11,plot=T)

}
\keyword{function}
