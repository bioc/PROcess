\name{intg}
\alias{intg}
\title{Integration}
\description{
This function calculates the integration of y with respect to x.
}
\usage{
intg(y, x)
}
\arguments{
\item{x}{a vector of real values, not necessarily sorted.} 
\item{y}{a vector of function values at x.}
}
\details{
 (x,y)s are sorted according to ascending x values and then
 the integration is calculated as sum of the products of average of adjacent y values and the difference of adjacent x values.
}
\author{Xiaochun Li}
\examples{
x <- seq(0, 1,length=100)
y <- x^2
intg(y, x)
}
\keyword{math}
