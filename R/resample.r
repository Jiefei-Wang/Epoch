#' Generic function for resampling objects
#' 
#' This function allows you to resample an object to a different sampling frequency.
#' 
#' @rdname resample-Epoch-method
#' @export
setGeneric("resample", function(x, ...) standardGeneric("resample"))


#' Resample an Epoch object to a different frequency
#' 
#' This function allows you to resample an Epoch object to a different sampling frequency.
#' 
#' @param x An `Epoch` object to be resampled.
#' @param freq The new sampling frequency.
#' @param ... Additional arguments passed to `gsignal::resample`
#' 
#' @return An `Epoch` object with the resampled data.
#' 
#' @rdname resample-Epoch-method
#' @export 
setMethod("resample", "Epoch", function(x, freq, ...) {
    # TODO: # rowData(x)$sampling_frequency -> metaData(x)$sampling_frequency
    oldFreq <- rowData(x)$sampling_frequency[1]
    electrodes <- rownames(x)
    timeRange <- range(coltimes(x))
    ntimes <- length(coltimes(x))

    newTimes <- seq(timeRange[1], timeRange[2], length.out = ntimes * freq / oldFreq)
    mat <- tblData(x)
    colnames(mat) <- NULL
    newMat <- gsignal::resample(
        t(mat),
        p = freq,
        q = oldFreq,
        ...
    )
    newMat <- t(newMat)
    colnames(newMat) <- newTimes
    tblData(x) <- newMat
    rowData(x)$sampling_frequency <- freq
    x
})
