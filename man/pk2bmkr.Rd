\name{pk2bmkr}
\alias{pk2bmkr}
\title{Find Biomarkers.}
\description{
Align peaks of spectra in `peakinfofile' and find biomarkers by a procedure described in Gentleman and Geyer (1994).
}
\usage{
pk2bmkr(peakinfofile, bseoffM, bmkfile, eps = 0.003, binary = F,p.fltr = 0.1)
}
\arguments{
  \item{peakinfofile}{a `.csv' file in the same format as   
	Ciphergen's peakinfo file with 5 columns  data, 
	Spectrum.Tag, Spectrum., Peak., Intensity and  
     	Substance.Mass.}
  \item{bseoffM}{a matrix holding the baseline-substracted 
	spectra, with row-names as the m/z values and 
	column-names  as the spectrum names.
}
  \item{bmkfile}{a `.csv' file in the same format as   
        Ciphergen's biomarker file, with spectra (samples) 
	as columns, and biomarkers as rows.}
  \item{eps}{expected experimental variation in the m/z values.}
  \item{binary}{output intensity or binary peak 
	presence/absence signals. }
  \item{p.fltr}{a number between 0 and 1. If a proto-biomarker 
	is identified as peak in > p.fltr x 100 percent of spectra, 
	it's kept in 'bmkfile'. }
}
\value{A dataframe with spectra as rows and biomarkers as 
columns. Spectrum labels and biomarker positions may be in 
the names of the dataframe.
}
\references{Gentleman, R. and Geyer, C.J. (1994). Maximum 
likelihood for interval censored data: Consistency and 
computation. Biometrika, 81:618--623.}
\author{Xiaochun Li}
\seealso{ \code{\link{rmBaseline}},\code{\link{getPeaks}}
}
\examples{
example(getPeaks)
testBio <- pk2bmkr("testpeakinfo.csv", testM, 
	"testbiomarker.csv")

## plot biomarker intensities of the 2 spectra

mzs <- getMzs(testBio)
matplot(mzs, t(testBio), type="l", col=c("red", "green"))
}
\keyword{nonparametric}
