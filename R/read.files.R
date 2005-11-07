 read.files <- function(fn) {
	if ("gz" %in% strsplit(fn,split="\\.")[[1]]){
            fn <- gzfile(fn)
            open(fn, "rb") ## using "rb" because gzfile is binary
        }else{
            fn <- file(fn, "r")
        }
        on.exit(close(fn))
        
        ## read.csv/table has bugs in the win32 platform
        ## see:
        ## https://stat.ethz.ch/pipermail/r-devel/2004-March/029135.html
        ## https://stat.ethz.ch/pipermail/r-devel/2004-March/029144.html
        ##
        ##read.csv(fn, as.is=TRUE)
        
        header <- readLines(fn, n=1)
        header <- unlist(strsplit(header, ","))
        
        tmpRead <- readLines(fn)
        tmpRead <- strsplit(tmpRead, ",")
        tmpMatrix <- matrix(NA, nrow=length(tmpRead), ncol=length(header))
        for(i in 1:ncol(tmpMatrix)){
            tmpMatrix[,i] <- sapply(tmpRead, function(x) as.numeric(x[i]))
        }
        colnames(tmpMatrix) <- header
        return(data.frame(tmpMatrix))
}

