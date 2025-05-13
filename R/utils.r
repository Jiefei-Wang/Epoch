tryToNum <- function(x) {
    x <- tryCatch(as.numeric(x), error = function(e) x, warning = function(w) x)
    if (is.numeric(x)) {
        return(x)
    } else {
        return(NULL)
    }
}


isWholeNumber <- function(x) {
    return(x %% 1 == 0)
}
