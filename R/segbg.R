"segbg" <-
function(f2, prob=0.25, breaks=100) {
        x <- f2[,1]
        y <- f2[,2]
	rx <- range(x)
        bc <- cut(log10(x+abs(min(x))+0.5), breaks=breaks)
        unlist(lapply(split(y, bc), trnc, prob=prob))
}
