getPeaks2 <- function(bseoffM, pVec, eps=0.003) {
	mzs <- as.numeric(rownames(bseoffM))
	left <- (1- eps) * pVec 
	right <- (1+eps) * pVec
	getMax <- function(i) {
		sub <- mzs > left[i] & mzs < right[i] 
		apply(bseoffM[sub,],2,max)
	}
	mat <- t(sapply(seq(along=pVec), getMax))
	rownames(mat) <- pVec
	mat
}


