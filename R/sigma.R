sigma <- function (x, span = 5)     
{
    n <- length(x)
    z <- embed(x, span)
    s <- span%/%2
    msd <- apply(z, 1, sd)
    c(rep(msd[1],s), msd, rep(msd[n-2*s],s))
}


