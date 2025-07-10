#' Plot method for Epoch objects
#'
#' @param x An Epoch object
#' @param y Not used (for S4 method compatibility)
#' @param gaps Numeric value specifying the gap between electrode traces (default: 2)
#' @param groupIndex Integer or string. A group of electrodes to show together in a different color. If NULL(default), all electrodes are shown in the same color. 
#' @param timeResolution Maximum number of time points to keep for each electrode (default: 2048)
#' @param maxLabels Maximum number of electrode labels to display on the y-axis (default: 50)
#' @param x.lab.size Size of the x-axis label text (default: 2)
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
    function(x, y, gaps = 2, 
    groupIndex = NULL, timeResolution = 2048, 
    maxLabels = 50, x.lab.size = 2,  ...) {
    elecNames <- rownames(x)
    data <- tblData(x)
    elecNum <- nrow(data)
    timesNum <- ncol(data)

    plotData <- data
    ## The indices of the time points to plot
    if (timesNum > timeResolution) {
        indices <- floor(seq(1, timesNum, length.out = timeResolution))
    }else{
        indices <- seq_len(timesNum)
    }
    plotData <- plotData[, indices, drop = FALSE]

    # ticks for x-axis
    timePoints <- .times(x)
    if (is.null(timePoints) || all(is.na(timePoints))) {
        xlabel <- "Time Index"
        timeTicks <- indices
    } else {
        xlabel <- "Time (s)"
        timeTicks <- timePoints[indices]
    }

    # group electrodes
    groupIndex <- .checkIndex(groupIndex, elecNames)
    group1 <- groupIndex
    group2 <- setdiff(seq_len(elecNum), groupIndex)

    # group colors
    elecColor <- rep("blue", elecNum)
    elecColor[seq_along(group2)] <- "black"

    # reorder the electrodes
    plotData <- plotData[c(group1, group2), , drop = FALSE]
    elecNames <- c(elecNames[group1], elecNames[group2])

    # Standardize the data
    plotData <- .standardizeIEEG(plotData)
    plotData <- as.data.frame(plotData)
    plotData$timeTicks <- timeTicks

    # Add gaps between electrodes for visual separation
    breakplot <- (seq_len(elecNum) - 1) * gaps
    elecNamesReversed <- rev(elecNames)
    for (i in seq_along(elecNamesReversed)) {
        elec <- elecNamesReversed[i]
        plotData[[elec]] <- plotData[[elec]] + (i-1) * gaps
    }

    p <- ggplot(data = plotData)
    for (i in seq_along(elecNamesReversed)) {
        elec <- elecNamesReversed[i]
        p <- p + geom_line(aes(x = .data$timeTicks, y = .data[[elec]]))
    }

    ## limit the number of labels on y-axis
    if (length(elecNamesReversed) > maxLabels) {
        by_num <- ceiling(length(elecNamesReversed)/maxLabels)
        label_idx <- seq(length(elecNamesReversed), 1, by=-by_num)
        elecNamesReversed[-label_idx] <- ""
    }

    p +
        labs(x = xlabel, y = "Electrode", size = x.lab.size) +
        scale_y_continuous(labels = elecNamesReversed, breaks = breakplot) +
        theme(
            axis.text.y = element_markdown(colour = elecColor)
        )
    }
)
