\name{bslnoff}
\alias{bslnoff}
\title{Baseline Substraction}
\description{
This function estimates the baseline and then removes baseline from the raw spectrum.
}
\usage{
bslnoff(fcut, breaks = 200, qntl = 0, method = c("loess", "approx"), bw = 0.005, plot = F, ...)
}
\arguments{
  \item{fcut}{a matrix with M/Z values in the first column and intensities in the second column }
  \item{breaks}{number of breaks set to M/Z values for finding the local minima or points below a centain quantile of intensities; breaks -1 equally spaced intervals on the log M/Z scale. }
  \item{qntl}{if 0, find local minima; if >0 find intensities < qntl*100th quantile locally.}
  \item{method}{"loess" or "approx" (linear interpolation).}
  \item{bw}{the bandwidth to be passed to loess.}
  \item{plot}{TRUE or FALSE, if true, it will plot the raw spectrum, theestimated baseline and the baseline substracted spectrum.}
  \item{\dots}{parameters for plot }
}
\value{
a matrix of two columns: the first column being the M/Z values same as 
the input, and the second column being the baseline substracted spectra.
}
\author{Xiaochun Li}
\examples{
fdat <- system.file("spectra/ssc", package="PROcess")
fs <- list.files(fdat, pattern="*csv*", full.names=T)
j <- sample(1:length(fs), 1)
f1 <- read.csv(gzfile(fs[j]))
fcut <- f1[f1[,1]>0,]
bseoff <-bslnoff(fcut,method="loess",plot=T, bw=0.1)
title(basename(fs[j]))
}
\keyword{function}
