"peaks" <-
function(series,span=3) { 
          z <- embed(series, span) 
	  s <- span %/% 2
          result <- max.col(z) == 1 + s 
          c(rep(FALSE,s), result, rep(FALSE,s)) 
          }
