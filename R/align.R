align <- function(pVec, eps=0.003) {
	delta <- pVec*eps
	nbhd <- cbind(pVec - delta, pVec + delta)
	cliqs <- Maclist(nbhd)
	intvls <- MLEintvl(nbhd, ml = cliqs)
	rowMeans(intvls)
}
