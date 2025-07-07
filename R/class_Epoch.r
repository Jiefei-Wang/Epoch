setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("arrayOrNULL", c("array", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))

#' @title Epoch Class
#' @description S4 class to handle epoch data with electrodes and time points
#' @slot table a matrix containing iEEG data (columns=time points, rows=electrodes)
#' @slot colData a data frame containing metadata for columns (time points)
#' @slot rowData a data frame containing metadata for rows (electrodes)
#' @slot metaData a list containing metadata for the Epoch object
#' @exportClass Epoch
.Epoch <- setClass("Epoch",
    slots = list(),
    contains = "TableContainer",
)

.TableContainer2Epoch <- function(x) {
    if (!is(x, "TableContainer")) {
        return(x)
    }
    # Create a new Epoch object
    .Epoch(
        table = tblData(x),
        rowData = rowData(x),
        colData = colData(x),
        metaData = metaData(x)
    )
}

#' Constructor for Epoch class
#' @param table Matrix containing epoch data (rows=electrodes, columns=time points)
#' @param electrodes Optional character vector for electrode names, if not provided, row names of data are used.
#' @param times Optional numeric vector of time points.
#' @param startTime Optional numeric value for start time, if provided, times will be calculated based on this and samplingRate.
#' @param samplingRate Optional numeric value for sampling rate, if provided, times will be calculated based on this and startTime.
#' @param rowData Optional data frame containing metadata for rows (electrodes).
#' @param colData Optional data frame containing metadata for columns (time points).
#' @param metaData Optional list containing metadata for the Epoch object.
#' @return An Epoch object
#' @export 
Epoch <- function(
    table,
    electrodes = NULL, times = NULL, 
    startTime = NULL, samplingRate = NULL,
    rowData = NULL, colData = NULL, metaData = NULL) {
    if (!is.null(times) && !is.null(startTime)) {
        stop("Only one of times or startTime can be non-null")
    }
    if (xor(is.null(startTime), is.null(samplingRate))) {
        stop("Both startTime and samplingRate must be provided or both must be NULL")
    }
    if (!is.null(startTime) && !is.null(samplingRate)) {
        times <- startTime + seq(0, ncol(table) - 1) / samplingRate
    }

    if (is.null(rowData)) {
        rowData <- data.frame()
    }
    if (is.null(colData)) {
        colData <- data.frame()
    }

    if (!is(rowData, "data.frame")) {
        stop("rowData must be a data.frame")
    }

    if (!is(colData, "data.frame")) {
        stop("colData must be a data.frame")
    }

    # set the time points of the table
    if (!is.null(times)) {
        colnames(table) <- times
    }

    # set the electrodes of the table
    if (!is.null(electrodes)) {
        rownames(table) <- electrodes
    }


    # Create new Epoch object
    .Epoch(
        table = table,
        rowData = rowData,
        colData = colData,
        metaData = metaData
    )
}

.times <- function(x) {
    as.numeric(colnames(tblData(x)))
}

.electrodes <- function(x) {
    as.numeric(rownames(tblData(x)))
}


###############################
## other Methods
###############################
#' Methods for Epoch class
#' 
#' @description `clip`: Truncating time range
#'
#' @param x An Epoch object
#' @param start Numeric value specifying start of new time range
#' @param end Numeric value specifying end of new time range
#' @return clip: clip the time range of the Epoch object
#' @rdname Epoch-method
#' @export
setGeneric("crop", function(x, start, end) standardGeneric("crop"))

#' @rdname Epoch-method
#' @export
setMethod("crop", "Epoch", function(x, start, end) {
    times <- coltimes(x)
    indices <- which(times >= start & times <= end)
    x[, indices] 
})


#' @return `coltimes`: A numeric vector of time points, or column indices if time points are not defined
#' @rdname Epoch-method
#' @export
setGeneric("coltimes", function(x) standardGeneric("coltimes"))

#' @rdname Epoch-method
#' @export
setMethod("coltimes", "Epoch", function(x) {
    tms <- .times(x)
    if (!length(tms)) {
        tms <- seq(1, ncol(x))
    }
    tms
})

