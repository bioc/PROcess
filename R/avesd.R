avesd <- function(Ma, cutoff) {
        mz <- as.numeric(rownames(Ma))
        subs <- mz > cutoff
        if (sum(subs)) {
                Ks <- rowSums(Ma[subs,])
                Ma.normed <- Ma[subs,] * median(Ks)/Ks
                return(mean(apply(Ma.normed, 1, sd)))
        } else return(NA)
}

