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


.message <- function(verbose, ...) {
    if (verbose) {
        message(...)
    }
}


#' Check and keep valid index only
#'
#' @param indices Numeric or character index to check
#' @param names Character. All names corresponding to the indices
.checkIndex <- function(indices, names) {
    if (length(names) == 0) {
        return()
    }
    if (length(indices) == 0) {
        return()
    }
    if (is(indices, "numeric")) {
        allIndices <- seq_along(names)
        diffIndices <- setdiff(indices, allIndices)
        indicesFiltered <- indices[!indices %in% diffIndices]
        result <- indicesFiltered
    } else {
        diffIndices <- setdiff(indices, names)
        indicesFiltered <- indices[!indices %in% diffIndices]
        result <- which(names %in% indicesFiltered)
    }
    if (length(diffIndices)) {
        indicesMissing <- paste(diffIndices, collapse = ", ")
        indicesExist <- paste(indicesFiltered, collapse = ", ")
        warning(
            glue("Indices {indicesMissing} are out of range. I will keep the valid values {indicesExist}.")
        )
    }
    result
}



#' Standardize iEEG row data for plotting
#'
#' @param data Matrix or data frame of iEEG data
#' @return Standardized data matrix
.standardizeIEEG <- function(data) {
    # Simple standardization - can be enhanced based on needs
    if (is.data.frame(data)) {
        data <- as.matrix(data)
    }
    
    # Z-score standardization per electrode
    apply(data, 1, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}