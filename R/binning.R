binning <- function(Ma, breaks=400){
        x <- as.numeric(rownames(Ma))
        apply(Ma, 2, bim, x=x,breaks=breaks)
}

bim <- function(x,y, breaks=400) {
        rx <- range(x)
        bc <- cut(x,breaks = breaks)
        mpt <- midpt(bc)
        z <- unlist(lapply(split(y,bc), max))
        names(z) <- round(mpt,0)
        z
}

midpt <- function(cutobj) {
        z <- attr(cutobj, "levels")
        cintvls<-sapply(z, function(l) substr(l,2,nchar(l)-1))
        sapply(cintvls,
         function(x) mean(as.numeric(unlist(strsplit(x,",")))))
}

