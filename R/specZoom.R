# color order: sm, pk, vly, nse

specZoom <- function(pks, xlim=NULL, cols=c("cyan","red",
	"black")) {

        mz <- pks$mz
        smooth <- pks$smooth
        peak <- pks$peak

        plot(mz, smooth, type="l",col=cols[1], xlim=xlim,
               xlab="m/z", ylab="smoothed spectrum")
        points(mz[peak], smooth[peak],col=cols[2])
	lines(mz,pks$sigmas, col=cols[3])

    	if (!is.null(xlim)) sub <- mz < xlim[2] & mz > xlim[1]
	else sub <- rep(T,length(mz))

        loc <- c(quantile(mz[sub], 0.8), max(smooth))

	ltxt <- c("smooth", "peaks", "local sigma")
	pchs <- c(" ","o"," ")
	ltys <- c(1,0,1)

	legend(loc[1], loc[2], legend =ltxt, pch = pchs, 
	    lty = ltys,  bty = "n", col = cols, cex=0.8)
}

