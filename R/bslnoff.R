"bslnoff" <-
function(f,breaks=200, qntl=0,
		method=c("loess","approx"), bw=0.005, plot=F, ...) {
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
           	plot(f,type="l",ylim=c(min(bseoff), max(f[,2])), 
			col="green",...)
                lines(f[,1],bsln, col="red")
                lines(bseoff, col="blue")
                abline(h=0, col="gray")
		loc <- c(quantile(f[,1],.9),max(f[,2]))
		legend(loc[1],loc[2], cex=0.8,
			legend=c("Raw","Baseline","Processed"), 
                	pch=19, lty=1,  bty="n",
         		col=c("green","red", "blue"))
	
	}
        return(bseoff)
}
