\name{isPeak}
\alias{isPeak}
\title{Locate Peaks in a Spectrum}
\description{
Find local maxima of a spectrum.
}
\usage{
isPeak(f,SoN = 2,span = 81,sm.span=11,plot=FALSE,add = FALSE,
zerothrsh=2,area.w = 0.003, ratio = 0.2, ...) }
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
  \item{zerothrsh}{ignore peaks whose intensity values are below zerothrsh.}
  \item{area.w}{the neighbourhood of a peak m/z,
     mz*(1-area.w, 1+area.w).} to calculate area of the peak,
  \item{ratio}{if area/max(area) > ratio, the peak is retained.}
  \item{\dots}{further arguments that get passed on to \code{plot}.}
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
     `f[,1]', `sigmas' the estimates of local variation and `area' the area associated with each peak after the first pass.  
}
\author{Xiaochun Li}
\examples{
example(bslnoff)
pkobj <- isPeak(bseoff,span=81,sm.span=11,plot=TRUE)

}
\keyword{nonparametric}
