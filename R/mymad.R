mymad <- function (x) 
{
    center <- median(x)
    y <- abs(x - center)
    n <- length(y)
    if (n == 0) 
        return(NA)
    half <- (n + 1)/2
    1.4826 * if (n%%2 == 1) {
        sort(y, partial = half)[half]
    }
    else {
        sum(sort(y, partial = c(half, half + 1))[c(half, half + 
            1)])/2
    }
}

