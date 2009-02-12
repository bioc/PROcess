\name{rmBaseline}
\alias{rmBaseline}
\title{Batch Baseline Subtraction.}
\description{
Baseline subtraction from each raw spectrum in 'fldr'.
}
\usage{
rmBaseline(fldr, bseoffrda = NULL, breaks = 200, qntl = 0, 
  method = "loess", bw = 0.1, 
  SpecNames = list.files(fldr, pattern = "\\\.*csv\\\.*"))
}
\arguments{
  \item{fldr}{a path to where the raw spectra are stored}
  \item{bseoffrda}{ optional; name of the file (with 
	extension .rda) where the baseline-substracted 
	spectra, a matrix with row-names as the m/z  values 
	and column-names as the spectrum tags, will be saved 
	to. }
  \item{breaks}{see bslnoff().}
  \item{qntl}{see bslnoff().}
  \item{method}{see bslnoff().}
  \item{bw}{see bslnoff().}
  \item{SpecNames}{a vector of character strings as spectrum 
	names.}
}
\value{ A matrix whose columns correspond to 
	baseline-subtracted spectra with row-names as 
 	the m/z values and column-names as the spectrum 
        names.
}
\author{Xiaochun Li}
\seealso{`bslnoff'.}
\examples{
testdir <- system.file("Test", package = "PROcess")
testM <- rmBaseline(testdir)
}
\keyword{nonparametric}
