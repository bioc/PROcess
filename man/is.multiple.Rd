\name{is.multiple}
\alias{is.multiple}
\title{Find multiple-charged polypeptides.}
\description{
For each of the polypeptides in a vector, find its multiple-charged species in the vector. 
}
\usage{
is.multiple(v, k = 2, eps = 0.003)
}
\arguments{
  \item{v}{a vector of polypeptides.}
  \item{k}{a vector of integers, that is, multiples of interest.}
  \item{eps}{a user specified precision of peak position.}
}
\details{
If abs(v-k*u)/v <= eps, then v is considered to be u with
k charges.
}
\value{
A list named with the m/z values of polypeptides who have
multiple-charged species; each component is a named vector
of polypeptides with number of charges as names. 
}
\examples{
bmks <- c(2360.25, 2666.34, 3055.72, 3058.04, 
3776.94, 3778.24, 3779.53, 4712.37,7559.76, 4587.03, 
4589.88, 9155.59, 13298.7)
is.multiple(bmks, k=2:5)
}
\keyword{arith}
