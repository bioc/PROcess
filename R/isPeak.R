isPeak <- function(f,SoN=2,span=81, sm.span=11,
	plot=F, add=F){
        n <- dim(f)[1]
	mz <- f[,1]
        lo <- lnn(f[,2], span=span, sm.span=sm.span)

        sm <- lo$fitted

        ispeak <- peaks(sm, span)

        sigmas <- lo$s

	zerothrsh <- mad(f[,2]) * 1.64

        peak <- ispeak &  sm > zerothrsh & sm > SoN * sigmas

	# locate valleys for each peak

	pks <- data.frame(peak=peak, smooth=sm, mz=mz, 
		sigmas=sigmas)
	
        if (plot) {
	  if (add) {
		lines(mz, sm, col="cyan")
		points(mz[peak],sm[peak], col="orange")
	  } else specZoom(pks)
        }
	return(pks)
}

