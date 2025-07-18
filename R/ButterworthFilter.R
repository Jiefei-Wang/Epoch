#' Apply Fourth-Order Butterworth Filter to Epoch Data
#'
#' @description Apply a fourth-order Butterworth filter to iEEG epoch data.
#' This function can apply lowpass, highpass, or bandpass filtering depending
#' on the parameters provided.
#'
#' @param epoch An Epoch object containing iEEG data
#' @param lowpass Numeric. Lowpass cutoff frequency in Hz. The default is 0.5 Hz.
#' @param highpass Numeric. Highpass cutoff frequency in Hz. The default is 99% of the Nyquist frequency.
#' @param order Integer. The order of the Butterworth filter. Default is 4.
#'
#' @return An Epoch object with filtered data
#'
#' @export
butterworthFilter <- function(epoch, lowpass = NULL, highpass = NULL, order = 4) {
    # Input validation
    if (!inherits(epoch, "Epoch")) {
        stop("Input must be an Epoch object")
    }
    
    row_data <- rowData(epoch)
    if (!"sampling_frequency" %in% colnames(row_data)) {
        stop("rowData must contain a 'sampling_frequency' column")
    }

    
    sampling_freq <- row_data$sampling_frequency[1]
    nyquist_freq <- sampling_freq / 2
    
    if (is.null(lowpass)) lowpass <- 0.5
    if (is.null(highpass)) highpass <- nyquist_freq * 0.99
    

    normalized_freqs <- c(lowpass, highpass) / nyquist_freq
    filter_type <- "pass"
    butter_filter <- gsignal::butter(
        n = order, 
        w = normalized_freqs, 
        type = filter_type)

    # Apply filter to epoch data
    mat <- tblData(epoch)
    
    # Apply zero-phase filtering (filtfilt) to each row
    filtered_data <- gsignal::filtfilt(
        filt = butter_filter,
        x = t(mat))

    filtered_data <- t(filtered_data)
    tblData(epoch) <- filtered_data
    
    epoch
}
