binning <- function(Ma, breaks=400){
        x <- as.numeric(rownames(Ma))
        rx <- range(x)
        bc <- cut(x,breaks = breaks)
        z <- attr(bc, "levels")
        cintvls<-sapply(z, function(l) substr(l,2,nchar(l)-1))
        mpt <- sapply(cintvls,
         function(x) mean(as.numeric(unlist(strsplit(x,",")))))
	bMa <- aggregate(Ma, list(bc), max)
	rownames(bMa) <- round(mpt,0)
	as.matrix(bMa[-1])
}
