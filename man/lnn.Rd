\name{lnn}
\alias{lnn}
\title{Estimate Signal and Variation of a Spectrum }
\description{
Estimate the signal and the variation of a spectrum.
}
\usage{
lnn(x, span = 21, sm.span = 11)
}
\arguments{
  \item{x}{a vector of real values.}
  \item{span}{the window widith for estimation of local variation.}
  \item{sm.span}{the window width for estimation of the signal of x.}
}
\details{
The signal of a spectrum is estimated by moving average and its local variation is estimated by moving `mad', possibly in a large window.
}
\value{A list with two components:
  \item{fitted}{estimated signal,}
  \item{sigma}{estimated local variation.}
}
\keyword{nonparametric}
