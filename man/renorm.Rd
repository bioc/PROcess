\name{renorm}
\alias{renorm}
\title{Renormalize Spectra}
\description{
Renormalize spectra for m/z values greater than `cutoff'.
}
\usage{
renorm(Ma, cutoff)
}
\arguments{
  \item{Ma}{a matrix, with rows the m/z values and the columns the samples.}
  \item{cutoff}{a real value, before which the portion of a spectrum will be ignored.}
}
\details{
A sample of spectra will be normalized to have the same AUC,
the median of the AUCs of spectra. Each AUC is calculated as
the sum of the intensities whose m/z values are greater than
`cutoff'.
}
\value{
A matrix, with rows the m/z values and the columns the samples.
Only rows with m/z values greater than `cutoff' are kept.
}
\examples{
example(rmBaseline)
rtM <- renorm(testM, cutoff=1500)
}
\keyword{utilities}
