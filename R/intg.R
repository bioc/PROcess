"intg" <-
function(y,x) {
        n <- length(x)
        index <- order(x)
        dx <- diff(sort(x))
        z <- y[index]
        ys <- (z[1:(n-1)]+z[2:n])/2
        sum(ys*dx)
}
