
#' Standardize iEEG data for plotting
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

#' Plot method for Epoch objects
#'
#' @param x An Epoch object
#' @param y Not used (for S4 method compatibility)
#' @param gaps Numeric value specifying the gap between electrode traces (default: 2)
#' @param resolution Number of time points to keep for each electrode
#' @param ... Additional arguments (not currently used)
#' @return A ggplot object showing iEEG electrode traces
#'
#' @examples
#' \dontrun{
#' # Create an Epoch object from sample data
#' epoch_data <- matrix(rnorm(1000), nrow = 10)
#' rownames(epoch_data) <- paste0("Electrode_", 1:10)
#' epoch <- Epoch(epoch_data)
#' 
#' # Plot the epoch
#' plot(epoch)
#' }
#' @rdname Epoch-method
#' @export
setMethod("plot", signature(x = "Epoch", y = "missing"), 
    function(x, y, gaps = 2, resolution = 2048,  ...) {
        # Convert matrix to Epoch if needed (already handled in method signature)
        
        elecNames <- rownames(x)
        data <- tblData(x)
        elecNum <- nrow(data)
        timesNum <- ncol(data)

        ## The indices of the time points to plot
        if (timesNum > resolution) {
            indices <- floor(seq(1, timesNum, length.out = resolution))
        }else{
            indices <- seq_len(timesNum)
        }

        # ticks for x-axis
        timePoints <- .times(x)
        if (is.null(timePoints) || all(is.na(timePoints))) {
            xlabel <- "Time Index"
            timeTicks <- indices
        } else {
            xlabel <- "Time (s)"
            timeTicks <- timePoints[indices]
        }

        # Standardize the data
        plotData <- data[, indices, drop = FALSE]
        plotData <- .standardizeIEEG(plotData)
        
        # Center the data by subtracting mean for each electrode
        plotData <- as.data.frame(plotData)
        plotData$timeTicks <- timeTicks
        

        # Add gaps between electrodes for visual separation
        breakplot <- (seq_len(elecNum) - 1) * gaps
        elecNamesReversed <- rev(elecNames)
        for (i in seq_along(elecNamesReversed)) {
            elec <- elecNamesReversed[i]
            plotData[[elec]] <- plotData[[elec]] + (i-1) * gaps
        }

        # Create the plot
        p <- ggplot2::ggplot(data = plotData)
        for (i in seq_along(elecNamesReversed)) {
            elec <- elecNamesReversed[i]
            p <- p + ggplot2::geom_line(ggplot2::aes(x = .data$timeTicks, y = .data[[elec]]))
        }

        p +
            ggplot2::labs(x = xlabel, y = "Electrode") +
            ggplot2::scale_y_continuous(labels = elecNamesReversed, breaks = breakplot) +
            ggplot2::theme_minimal()
    }
)
