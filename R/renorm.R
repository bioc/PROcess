renorm <- function(Ma, cutoff) {
        mz <- as.numeric(rownames(Ma))
        subs <- mz > cutoff
        if (sum(subs)) {
                Ks <- colSums(Ma[subs,])
                t(t(Ma[subs,]) * median(Ks)/Ks)
        } else return(NA)
}

