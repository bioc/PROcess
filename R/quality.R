quality <- function(Ma, peakinfofile, cutoff) {
   Q.R <- function(y, mzs, cutoff) {
        tmp <- cbind(mzs, y)
        dat <- tmp[tmp[,1]>cutoff,]
        n <- dim(dat)[1]
        nse <- noise(dat[,2], span=5)
        nse.evlp <- 3*sigma(nse, span=251)
        area0 <- intg(dat[,2], dat[,1])
        area1 <- intg(dat[,2]-nse.evlp, dat[,1])
        Quality <- area1/area0
        Retain <- sum(dat[,2]>5*nse.evlp)/n
        return(c(Quality=Quality, Retain=Retain))
   }
	mzs <- as.numeric(rownames(Ma))
        qrs <- t(apply(Ma, 2, Q.R, mzs=mzs, cutoff=cutoff))
        fns <- basename(dimnames(qrs)[[1]])
        pks <- read.csv(peakinfofile, as.is=TRUE)
        ns <- sapply(split(pks$"Peak.", pks$"Spectrum.Tag"), 
                length)
        peak <- ns / mean(ns)
        matched <- match(fns, names(peak))
        keep <- !is.na(matched)
        mtc <- matched[keep]
        res <- cbind(qrs[keep,], peak=peak[mtc])
        dimnames(res)[[1]] <- fns[keep]
        return(res)
}

