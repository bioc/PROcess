getPeaks<-function(bseoffM, peakinfofile, SoN = 2, 
	span = 81, sm.span=11) {

	mzs <- as.numeric(rownames(bseoffM))	
	n <- dim(bseoffM)[2]
	Spec <- colnames(bseoffM)
	cnts <- rep(0, n)
	for (j in 1:n) {
		bseoff <- cbind(mzs,bseoffM[,j])
		pks <- isPeak(bseoff,SoN=SoN,span=span,
			sm.span=sm.span)
		cnts[j] <- sum(pks$peak)
		is.peak <- pks$peak
	        if (j >1) {
		  Peak. <- c(Peak.,cumsum(is.peak[is.peak]))
		  Intensity <- c(Intensity,pks$smooth[is.peak])
		  Substance.Mass <- c(Substance.Mass,
					pks$mz[is.peak])
		} else {
                        Peak. <- cumsum(is.peak[is.peak])
                        Intensity <- pks$smooth[is.peak]
                        Substance.Mass <- pks$mz[is.peak]
		}
	}
	Spectrum.Tag <- rep(Spec, cnts)
	Spectrum. <- rep(1:n, cnts)
	write.table(data.frame(Spectrum.Tag,Spectrum., Peak.,
		Intensity, Substance.Mass), file=peakinfofile, 
		sep=",", row.names=F)	
}

