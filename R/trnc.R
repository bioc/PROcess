"trnc" <-
function(y, prob) {
        if (prob > 0) {
        qs <- quantile(y, probs=prob)
        ifelse(y < qs, y, qs)
        }
        else rep(min(y), length(y))
}
