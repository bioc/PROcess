rmBaseline<- function(fldr, outputname="baseoffM",
	bseoffrda=NULL, breaks=200, 
	qntl=0,method="loess",bw=0.1, 
	SpecNames=list.files(fldr,pattern = "*csv*"))  {

	fs <- list.files(fldr,pattern = "*csv*", 
		full.names = T)
	n <- length(fs)
	for (j in 1:n) {
		f1 <- read.files(fs[j])
		fcut <- f1[f1[, 1] > 0, ]
		bseoff <- bslnoff(fcut, breaks=breaks, 
			qntl=qntl, method = method, bw=bw)
	        if (j >1) bseoffM <- cbind(bseoffM,bseoff[,2])
                else bseoffM <- bseoff[,2]
	}
	dimnames(bseoffM) <- list(signif(bseoff[,1], 6), 
					SpecNames)
	assign(outputname, bseoffM, env=sys.frame())
	if (!is.null(bseoffrda))
		save(list=outputname, file=bseoffrda)
}

