"bslnoff" <-
function(f,breaks=200, qntl=0,
		method=c("loess","approx"), bw=0.005, 
		plot=FALSE, ...) {
	trnc <- function(y, prob) {
        	if (prob > 0) {
        	qs <- quantile(y, probs=prob)
        	ifelse(y < qs, y, qs)
        	}
        	else rep(min(y), length(y))
	}
	segbg <- function(f2, prob=0.25, breaks=100) {
        	x <- f2[,1]
        	y <- f2[,2]
		rx <- range(x)
        	bc <- cut(log10(x+abs(min(x))+0.5), 
			breaks=breaks)
        	unlist(lapply(split(y, bc), trnc, prob=prob))
	}
	dots <- match.call(expand.dots=TRUE)
	ylim <- dots$ylim
	bgs <- segbg(f, prob=qntl,breaks=breaks)
        if (!method %in% c("loess","approx")) 
		stop("**no such method**")
        if (method=="loess") {
                f.lo<- loess(bgs ~ f[,1], span=bw, degree=1)
		bsln <- f.lo$fitted
        } else {
                f.lo <- approx(f[,1], bgs, xout=f[,1])
		bsln <- f.lo$y
        }
	bseoff <- cbind(f[,1], f[,2]-bsln)
 	if (plot) {	
		if (is.null(ylim)) ylim <- c(min(bseoff[,2]), 
                        		max(f[,2]))
		else ylim <- eval(ylim, parent.frame())
           	plot(f,type="l", col="green",ylim=ylim)
                lines(f[,1],bsln, col="red")
                lines(bseoff, col="blue")
                abline(h=0, col="gray")
		loc <- c(quantile(f[,1],.8),max(f[,2]))
		legend(loc[1],loc[2], cex=0.8,
			legend=c("Raw","Baseline","Processed"), 
                	pch=19, lty=1,  bty="n",
         		col=c("green","red", "blue"))
	
	}
        return(bseoff)
}
