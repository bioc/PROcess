\name{peaks}
\alias{peaks}
\alias{noise}
\alias{sigma}
\title{Peak Detection}
\description{
Finds the local maxima, local noise and its associated standard 
deviations in a vector.
}
\usage{
peaks(x, span = 3)
noise(x, span = 5)
sigma(x, span = 5)
}
\arguments{
\item{x}{a vector.}
\item{span}{a local miximum is defined as an element in a sequence which is greater than all other elements within a window of width `span' centered at that element. The default value is 3, meaning that a peak is bigger than both of its neighbors. Local noise is definedas an element minus the mean of all elements within a window of width `span' centered at that element. Local standard deviation of an element is defined as the standard deviation of all elements within a window of width `span' centered at that element.}
}
\value{
a logical vector of the same length as `series' indicating where the peaks are.
}
\author{Xiaochun Li}
\examples{
x <- seq(0, 10*pi, by=0.1)
y <- sin(x)*x
plot(x,y, type="l")
is.max <- peaks(y)
points(x[is.max],y[is.max], pch=21, bg="red")
legend(2, 25, legend = "Peaks",pch = 19, col="red", bty = "n")

# can be used for local minima too:
# is.min <- peaks(-y)
# points(x[is.min],y[is.min], pch=21, bg="blue")
}
\keyword{nonparametric}
