pk2bmkr <- function(peakinfofile, bseoffM, bmkfile, 
		eps=0.003, binary=F, p.fltr=0.1) {
	pkf <- read.csv(peakinfofile, as.is=T)
	if (!binary) {
		bsemz <- as.numeric(rownames(bseoffM))
	}
	pkfSpec <- pkf$Spectrum.Tag
	pkfInten <- pkf$Intensity
	mzs <- pkf$Substance.Mass
	delta <- mzs*eps
	tmp <- cbind(mzs-delta, mzs+delta)
	cliqs <- Maclist(tmp)
	intvls <- MLEintvl(tmp, ml=cliqs)
	bmkrs <- rowMeans(intvls)	
	nmkrs <- length(bmkrs)
	SPECTRUM <- unique(pkfSpec)
	nSpec <- length(SPECTRUM)
	bM <- matrix(0, nSpec, nmkrs) 
	mkrnames <- paste("M",signif(bmkrs,6),sep="")
	dimnames(bM) <- list(SPECTRUM,mkrnames)
	tM <- bM
	for (i in 1:nmkrs ) {
		whichRows <- cliqs[[i]]
		vals <- sapply(split(pkfInten[whichRows],
				pkfSpec[whichRows]), max)
		whichSpecs <- names(vals)
		tM[whichSpecs,i] <- 1
		if (binary) bM[whichSpecs,i] <- 1
		else {
			bM[whichSpecs,i] <- vals
			fill <- ! (SPECTRUM %in% whichSpecs)
			whichmz <- bsemz >= intvls[i,1] &
					bsemz <= intvls[i,2]
			if(sum(whichmz)<1) {
			  dis <- abs(bsemz-bmkrs[i])
			  whichmz <- dis==min(dis)
			}
			if(sum(whichmz)>1) vfill <-
			  apply(bseoffM[whichmz,],2,max)
			else vfill <- bseoffM[whichmz,]
			bM[fill, i] <- vfill[fill]
		}
	}
	keep <- colSums(tM)/nSpec >= p.fltr 
	bM <- bM[,keep]
        write.table(data.frame(bM),file=bmkfile, 
	sep=",", row.names=T)
	data.frame(bM)
}
