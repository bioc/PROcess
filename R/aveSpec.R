aveSpec <- function(nVec) {
        n <- length(nVec)
        cMa <- read.files(nVec[1])[,2]
        for (i in 2:n) {
                f1 <- read.files(nVec[i])
                cMa <- cbind(cMa, f1[,2])
        }
        obj <- rowMeans(cMa)
	cbind(f1[,1], obj)
}

