read.files <- function(fn) {
	if ("gz" %in% strsplit(fn,split="\\.")[[1]])
        	fn <- gzfile(fn)
        read.csv(fn, as.is=T)
}

