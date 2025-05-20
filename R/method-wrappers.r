
#' @rdname Epoch-method
#' @export
setMethod("dim", "Epoch", function(x) {
    callNextMethod()
})

#' @rdname Epoch-method
#' @export
setMethod("dimnames", "Epoch", function(x) {
    callNextMethod()
})



#' @param i Row indices for subsetting. If only `i` is provided, it will return the entire row(s).
#' @param j Column indices for subsetting.
#' @param ... Additional arguments.
#' @param drop Not used.
#' @rdname Epoch-method
#' @return `[`: A new Epoch object with the selected data.
#' @export
setMethod("[", signature(x = "TableContainer"), 
function(x, i, j, ..., drop = TRUE) {
    ## Call the next method in the chain
    result <- callNextMethod()
    ## Check if the result is a TableContainer
    result <- .TableContainer2Epoch(result)
    result
})