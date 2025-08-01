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
#' @param metaData Optional list containing metadata for the Epoch object. Element name "SamplingRate" is reserved by the Epoch class.
#' @return An Epoch object
#'
#' @examples
#' epoch_data <- matrix(rnorm(1000), nrow = 10)
#' rownames(epoch_data) <- paste0("Electrode_", 1:10)
#' epoch <- Epoch(epoch_data, startTime = 0, samplingRate = 100)
#'
#' @export
Epoch <- function(
    table,
    electrodes = NULL, times = NULL,
    startTime = NULL, samplingRate = NULL,
    rowData = NULL, colData = NULL, metaData = NULL) {
    if (!xor(is.null(times), is.null(startTime))) {
        stop("You must specify exactly one of 'times' or 'startTime'")
    }
    if (!is.null(startTime)) {
        # If startTime is provided, we need to calculate times based on samplingRate
        if (is.null(samplingRate)) {
            stop("If 'startTime' is provided, 'samplingRate' must also be provided")
        }
        times <- startTime + seq(0, ncol(table) - 1) / samplingRate
    } else {
        # If times is provided, we need to estimate samplingRate
        estSamplingRate <- length(times) / (times[length(times)] - times[1])
        if (is.null(samplingRate)) {
            samplingRate <- estSamplingRate
        } else {
            if (abs(estSamplingRate - samplingRate) > 1e-6) {
                stop(
                    glue("Estimated sampling rate {estSamplingRate} does not match provided sampling rate {samplingRate}.")
                )
            }
        }
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

    # reserved metaData name
    metaData$samplingRate <- samplingRate

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

.samplingRate <- function(x) {
    if (!is.null(metaData(x)$samplingRate)) {
        return(metaData(x)$samplingRate)
    }
    stop("Sampling rate is not defined in metaData")
}

`.samplingRate<-` <- function(x, value) {
    metaData(x)$samplingRate <- value
    x
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
#' 
#' @examples 
#' # Create an Epoch object
#' epoch_data <- matrix(rnorm(1000), nrow = 10)
#' rownames(epoch_data) <- paste0("Electrode_", 1:10)
#' epoch <- Epoch(epoch_data, startTime = 0, samplingRate = 100)
#' 
#' # crop the epoch
#' crop(epoch, start = 0.5, end = 1.5)
#' 
#' @family Epoch methods
#' @export
setGeneric("crop", function(x, start, end, ...) standardGeneric("crop"))

#' @rdname Epoch-method
#' @param checkTimeRange Logical, whether to check the validity of the time range. This includes checking if the time range is empty, if `start` is greater than `end`, and if `start` or `end` are out of bounds. Default is TRUE.
#' @export
setMethod("crop", "Epoch", function(x, start, end, checkTimeRange = TRUE) {
    times <- coltimes(x)
    # Check 1: type validation
    if (!is.numeric(start) || !is.numeric(end)) {
        stop("crop function: `start` and `end` must be numeric.")
    }

    if (checkTimeRange) {
        # Check 2: empty times
        if (length(times) == 0) {
            warning("crop function: The Epoch object has no time information.")
        }

        # Check 3: logic error
        if (start > end) {
            warning("crop function: `start` is greater than `end`. Empty Epoch is returned.")
        }

        # Check 4: out-of-bounds time range
        if (start < min(times) || end > max(times)) {
            warning(glue::glue(
                "crop function: `start` or `end` is outside the available time range [{min(times)}, {max(times)}]."
            ))
        }
    }

    indices <- which(times >= start & times <= end)

    if (length(indices) == 0) {
        warning("crop function: No time points found in the specified range. Returning an empty Epoch object.")
    }

    x[, indices]
})


#' @return `coltimes`: A numeric vector of time points, or column indices if time points are not defined
#' @rdname Epoch-method
#' @export
setGeneric("coltimes", function(x) standardGeneric("coltimes"))

#' @rdname Epoch-method
#' @examples 
#' # get the time points of an Epoch object
#' coltimes(epoch)
#' 
#' @export
setMethod("coltimes", "Epoch", function(x) {
    tms <- .times(x)
    if (!length(tms)) {
        tms <- seq(1, ncol(x))
    }
    tms
})
