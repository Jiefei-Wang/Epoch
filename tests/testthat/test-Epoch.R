library(testthat)
library(Epoch)

# Create a dummy Epoch object for testing
row_num <- 10
col_num <- 100
dummy_data <- matrix(rnorm(row_num*col_num), nrow = row_num, dimnames = list(paste0("Elec", seq_len(row_num)), NULL))
expected_times <- seq(0, by = 0.1, length.out = col_num)
dummy_epoch <- Epoch(dummy_data, time = expected_times)

test_that("Epoch object creation and basic properties with valid input", {
  expect_s4_class(dummy_epoch, "Epoch")
  expect_equal(nrow(tblData(dummy_epoch)), row_num)
  expect_equal(ncol(tblData(dummy_epoch)), col_num)
  # Time is stored as colnames of the data table
  expect_equal(as.numeric(colnames(tblData(dummy_epoch))), expected_times)
  expect_equal(coltimes(dummy_epoch), expected_times)
  expect_equal(dim(dummy_epoch), dim(dummy_data))
  expect_equal(rownames(dummy_epoch), rownames(dummy_data))
})

test_that("tblData method for Epoch", {
  expect_true(is.matrix(tblData(dummy_epoch)))
  expect_equal(dim(tblData(dummy_epoch)), c(row_num, col_num))
})

test_that("crop method for Epoch with valid inputs", {
  # Times are accessed via colnames(tblData())
  original_times <- coltimes(dummy_epoch)
  cropped_epoch_time <- crop(dummy_epoch, start = 1, end = 5)
  expect_s4_class(cropped_epoch_time, "Epoch")
  cropped_times <- as.numeric(colnames(tblData(cropped_epoch_time)))
  expect_true(all(cropped_times >= 1))
  expect_true(all(cropped_times <= 5))
  expect_lt(ncol(tblData(cropped_epoch_time)), ncol(tblData(dummy_epoch)))
})


test_that("crop() returns full Epoch when range matches full extent", {
  times <- coltimes(dummy_epoch)
  e <- crop(dummy_epoch, start = min(times), end = max(times))
  expect_equal(dim(e), dim(dummy_epoch))
})

test_that("crop() gives error when start or end is not numeric", {
  expect_error(
    e <- crop(dummy_epoch, start = 'a', end = 0.2),
    regexp = "must be numeric"
  )
  expect_error(
    e <- crop(dummy_epoch, start = 0.1, end = '0.2'),
    regexp = "must be numeric"
  )
})

test_that("crop() gives warning when start or end is out of range", {
  expect_warning(
    e <- crop(dummy_epoch, start = -1, end = 0.2),
    regexp = "outside the available time range"
  )
  expect_s4_class(e, "Epoch")
})

test_that("crop() returns empty Epoch and warns when no time points match", {
  warnings <- character()
  
  withCallingHandlers(
    {
      e <- crop(dummy_epoch, start = 500, end = 600)
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  expect_length(warnings, 2)
  expect_true(any(grepl("No time points found", warnings)))
  expect_true(any(grepl("outside the available time range", warnings)))
  expect_equal(ncol(e), 0)
})

test_that("crop() with check_time_range = FALSE returns silently", {
  e <- crop(dummy_epoch, start = -1, end = 0.2, check_time_range = FALSE)
  expect_s4_class(e, "Epoch")
})

test_that("crop() throws error for start > end when check_time_range = TRUE", {
  warnings <- character()
  
  withCallingHandlers(
    {
      e <- crop(dummy_epoch, start = 0.6, end = 0.2)
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  expect_length(warnings, 2)
  expect_true(any(grepl("No time points found", warnings)))
  expect_true(any(grepl("`start` is greater than `end`", warnings)))
})


test_that("plot method for Epoch", {
  # Check if plot returns a ggplot object without errors
  library(ggplot2)
  p <- plot(dummy_epoch)
  expect_s3_class(p, "ggplot")
})


test_that("Epoch print/show methods", {
  # Capture print output
  expect_no_error(capture_output(print(dummy_epoch)))
  expect_no_error(capture_output(show(dummy_epoch)))
})

test_that("Epoch dim, rownames, colnames", {
  expect_equal(dim(dummy_epoch), c(row_num, col_num))
  expect_equal(rownames(dummy_epoch), paste0("Elec", seq_len(row_num)))
})

test_that("Epoch subsetting with valid values", {
  subset_epoch <- dummy_epoch[1:5, 1:6]
  expect_s4_class(subset_epoch, "Epoch")
  expect_equal(dim(subset_epoch), c(5, 6))
  expect_equal(rownames(subset_epoch), paste0("Elec", 1:5))

  subset_epoch_single_elec <- dummy_epoch["Elec1", ]
  expect_s4_class(subset_epoch_single_elec, "Epoch")
  expect_equal(nrow(subset_epoch_single_elec), 1)
  expect_equal(rownames(subset_epoch_single_elec), "Elec1")

  subset_epoch_single_time <- dummy_epoch[, 2]
  expect_s4_class(subset_epoch_single_time, "Epoch")
  expect_equal(ncol(subset_epoch_single_time), 1)
  expect_equal(colnames(subset_epoch_single_time), "0.1")
  
  expect_equal(dim(dummy_epoch[0, 0]), c(0, 0))

})

test_that("Epoch subsetting with invalid values", {

  expect_error(dummy_epoch[999, ], regexp = "subscript out of bounds")
  expect_error(dummy_epoch[, 999], regexp = "subscript out of bounds")
  expect_error(dummy_epoch["nonexistent_electrode", ], regexp = "subscript out of bounds")
  expect_error(dummy_epoch[, "nonexistent_time"], regexp = "subscript out of bounds")

})

test_that("logical subsetting recycles by default", {
  subset <- dummy_epoch[c(TRUE, FALSE), ]
  expect_equal(nrow(subset), ceiling(nrow(dummy_epoch) / 2))
})

test_that("subsetting with NA returns NA-filled Epoch", {
  subset_epoch <- dummy_epoch[NA, ]
  expect_s4_class(subset_epoch, "Epoch")
  expect_equal(nrow(subset_epoch), 10)
  expect_true(all(is.na(as.matrix(tblData(subset_epoch)))))
})

test_that("subsetting with -999 returns full object (nothing dropped)", {
  subset <- dummy_epoch[-999, ]
  expect_equal(dim(subset), dim(dummy_epoch))
})

