lnn <- function (x, span = 21, sm.span=11) 
{
    n <- length(x)
    z <- embed(x, sm.span)
    ss <- sm.span%/%2
    s <- span%/%2
    mmn <- apply(z, 1, mean) # moving average
    fitted <-c(rep(mmn[1],ss), mmn, rep(mmn[n-2*ss],ss))
    zz <- embed(x, span)
    msd <- apply(zz, 1, mad) # local variation estimation
    sigma <- c(rep(msd[1],s), msd, rep(msd[n-2*s],s))
    list(fitted=fitted, s=sigma)
}

