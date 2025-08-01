% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-Epoch.r, R/method-Epoch-print.r,
%   R/method-wrappers.r
\name{crop}
\alias{crop}
\alias{crop,Epoch-method}
\alias{coltimes}
\alias{coltimes,Epoch-method}
\alias{show,Epoch-method}
\alias{dim,Epoch-method}
\alias{dimnames,Epoch-method}
\alias{[,Epoch-method}
\title{Methods for Epoch class}
\usage{
crop(x, start, end, ...)

\S4method{crop}{Epoch}(x, start, end, checkTimeRange = TRUE)

coltimes(x)

\S4method{coltimes}{Epoch}(x)

\S4method{show}{Epoch}(object)

\S4method{dim}{Epoch}(x)

\S4method{dimnames}{Epoch}(x)

\S4method{[}{Epoch}(x, i, j, ..., drop = TRUE)
}
\arguments{
\item{x}{An Epoch object}

\item{start}{Numeric value specifying start of new time range}

\item{end}{Numeric value specifying end of new time range}

\item{...}{Additional arguments.}

\item{checkTimeRange}{Logical, whether to check the validity of the time range. This includes checking if the time range is empty, if \code{start} is greater than \code{end}, and if \code{start} or \code{end} are out of bounds. Default is TRUE.}

\item{object}{Epoch object}

\item{i}{Row indices for subsetting. If only \code{i} is provided, it will return the entire row(s).}

\item{j}{Column indices for subsetting.}

\item{drop}{Not used.}
}
\value{
clip: clip the time range of the Epoch object

\code{coltimes}: A numeric vector of time points, or column indices if time points are not defined

\code{[}: A new Epoch object with the selected data.
}
\description{
\code{clip}: Truncating time range
}
\examples{
# Create an Epoch object
epoch_data <- matrix(rnorm(1000), nrow = 10)
rownames(epoch_data) <- paste0("Electrode_", 1:10)
epoch <- Epoch(epoch_data, startTime = 0, samplingRate = 100)

# crop the epoch
crop(epoch, start = 0.5, end = 1.5)

# get the time points of an Epoch object
coltimes(epoch)

}
\seealso{
Other Epoch methods: 
\code{\link{plot,Epoch,missing-method}},
\code{\link{resample}()}
}
\concept{Epoch methods}
