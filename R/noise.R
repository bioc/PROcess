noise <- function (x, span = 5) 
{
    n <- length(x)
    z <- embed(x, span)
    s <- span%/%2
    mmn <- rowMeans(z)
    c(x[1:s]-mmn[1], z[,1+s]-mmn, x[(n-s+1):n]-mmn[n-2*s])
}
