setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("arrayOrNULL", c("array", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))

#' @title Epoch Class
#' @description S4 class to handle epoch data with electrodes and time points
#' @slot data a tibble containing epoch data (columns=time points, rows=electrodes)
#' @slot times Numeric vector containing time range
.Epoch <- setClass("Epoch",
    slots = list(),
    contains = "TableContainer",
)



#' Constructor for Epoch class
#' @param table Matrix containing epoch data (rows=electrodes, columns=time points)
#' @param electrodes Optional character vector for electrode names, if not provided, row names of data are used.
#' @param times Optional numeric vector of time points.
#' @param startTime Optional numeric value for start time, if provided, times will be calculated based on this and samplingRate.
#' @param samplingRate Optional numeric value for sampling rate, if provided, times will be calculated based on this and startTime.
#' @param rowData Optional data frame containing metadata for rows (electrodes).
#' @param colData Optional data frame containing metadata for columns (time points).
#' @param metaData Optional list containing metadata for the Epoch object.
#' @export
#' @return An Epoch object
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

    # set default time points if not provided
    if (is.null(times)) {
        if ("times" %in% names(colData)) {
            times <- colData$times
        }
    }

    # Set electrode names
    if (is.null(electrodes)) {
        if ("electrodes" %in% names(rowData)) {
            electrodes <- rowData$electrodes
        } else if (!is.null(rownames(table))) {
            electrodes <- rownames(table)
        }
    }


    if (!is.null(times) && length(times) != ncol(table)) {
        stop("Length of times must be equal to number of columns in data")
    }
    if (!is.null(electrodes) && length(electrodes) != nrow(table)) {
        stop("Length of electrodes must be equal to number of rows in data")
    }


    rowData$electrodes <- electrodes
    colData$times <- times

    rownames(table) <- electrodes
    colnames(table) <- times

    ## make sure electrodes is the first column if exists
    if ("electrodes" %in% names(rowData)) {
        rowData <- rowData[, c("electrodes", setdiff(names(rowData), "electrodes"))]
    }

    ## make sure times is the first column if exists
    if ("times" %in% names(colData)) {
        colData <- colData[, c("times", setdiff(names(colData), "times"))]
    }

    # Create new Epoch object
    .Epoch(
        table = table,
        rowData = rowData,
        colData = colData
    )
}

.times <- function(x) {
    colData(x)$times
}

.electrodes <- function(x) {
    rowData(x)$electrodes
}


###############################
## other Methods
###############################
#' Methods for Epoch class
#' 
#' @description `clip`: Truncating time range
#'
#' @param start Numeric value specifying start of new time range
#' @param end Numeric value specifying end of new time range
#' @return clip: clip the time range of the Epoch object
#' @rdname Epoch-method
#' @export
setGeneric("crop", function(x, start, end) standardGeneric("crop"))

#' @rdname Epoch-method
#' @export
setMethod("crop", "Epoch", function(x, start, end) {
    times <- .times(x)
    if (is.null(times)) {
        if (!isWholeNumber(start) || !isWholeNumber(end)) {
            stop("Time points is not defined for this Epoch object, from and to must be whole numbers")
        }
        indices <- seq(start, end)
        newTimes <- NULL
    } else {
        # current time points
        # Find indices within new time range
        indices <- which(times >= start & times <= end)
    }

    x[, indices] 
})



