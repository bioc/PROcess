\name{avesd}
\alias{avesd}
\title{Compute the Average Standard Deviation for a Set of Spectra and
a Given Cutoff Point}
\description{
Compute the Average Standard Deviation for a Set of Spectra and 
a Given Cutoff Point
}
\usage{
avesd(Ma, cutoff)
}
\arguments{
  \item{Ma}{A matrix with rows the m/z values and columns samples/spectra}
  \item{cutoff}{A number of m/z}
}
\details{
For a given cutoff point, the standard deviation of all spectra will be 
calculated at each m/z above this cutoff point. The resulting sd's are
then averaged and returned. 
}
\value{
A real number.
}
\keyword{arith}
