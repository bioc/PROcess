is.multiple <- function(v, k=2, eps=0.003) {
	sv <- sort(v)
	n <- length(sv)
	res <- vector("list",n-1)
	names(res) <- as.character(sv[2:n])
	m <- length(k)
	for (i in 2:n) {
		x <- vector("numeric",0) 
		xlab <- vector("character",0)
		for (j in 1:m) {
		 which <-abs(sv[i]-k[j]*sv[1:(i-1)])/sv[i]<=eps
		 if (sum(which)) {
		  lx <- length(x)
		  x[(lx+1):(lx+sum(which))]<-sv[1:(i-1)][which]	
		  xlab[(lx+1):(lx+sum(which))] <-
			rep(paste(k[j],"H+",sep=""),sum(which))
		 }
		names(x) <- xlab
		}
		if(sum(x)) res[[i-1]] <- x
	}
	res[!sapply(res, is.null)]
}
