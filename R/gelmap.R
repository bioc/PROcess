gelmap <- function(Ma, cols=gray(seq(1,0, by=-0.01)), at.mz=NULL,
        at.col=NULL, cexCol= 0.2 + 1/log10(nc)) {
        di <- dim(Ma)
        nr <- di[1]
        nc <- di[2]
	if (is.null(rownames(Ma)))
		x <- 1:nr
	else x <- as.numeric(rownames(Ma))
        y <- 1:nc
        sampLab <- if (is.null(colnames(Ma))) y else colnames(Ma)
        image(x, y, Ma, col=cols, axes=FALSE, xlab="", ylab="")
        every <- ifelse(nr %/% 30, nr %/% 30, 1)
        if(is.null(at.mz)) at.mz <- x[seq(1,nr,by=every)]
        axis(1, at=at.mz, lab=at.mz, las=2, lty=0, cex.axis=.8)
        if(is.null(at.col)) at.col <- 1:nc
        axis(2, at=at.col, lab=sampLab[at.col],las=2,
                lty=0, cex.axis=cexCol)
}

