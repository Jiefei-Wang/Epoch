library(testthat)
library(Epoch)

# Create a dummy Epoch object for testing
row_num <- 10
col_num <- 100
dummy_data <- matrix(rnorm(row_num*col_num), nrow = row_num, dimnames = list(paste0("Elec", seq_len(row_num)), NULL))
expected_times <- seq(0, by = 0.1, length.out = col_num)
dummy_epoch <- Epoch(dummy_data, time = expected_times)

test_that("Epoch object creation and basic properties", {
  expect_s4_class(dummy_epoch, "Epoch")
  expect_equal(nrow(tblData(dummy_epoch)), row_num)
  expect_equal(ncol(tblData(dummy_epoch)), col_num)
  # Time is stored as colnames of the data table
  expect_equal(as.numeric(colnames(tblData(dummy_epoch))), expected_times)
  expect_equal(coltimes(dummy_epoch), expected_times)
})

test_that("tblData method for Epoch", {
  expect_true(is.matrix(tblData(dummy_epoch)))
  expect_equal(dim(tblData(dummy_epoch)), c(row_num, col_num))
})

test_that("crop method for Epoch", {
  # Times are accessed via colnames(tblData())
  original_times <- coltimes(dummy_epoch)
  cropped_epoch_time <- crop(dummy_epoch, start = 1, end = 5)
  expect_s4_class(cropped_epoch_time, "Epoch")
  cropped_times <- as.numeric(colnames(tblData(cropped_epoch_time)))
  expect_true(all(cropped_times >= 1))
  expect_true(all(cropped_times <= 5))
  expect_lt(ncol(tblData(cropped_epoch_time)), ncol(tblData(dummy_epoch)))

  dummy_epoch_no_time <- Epoch(dummy_data)
  # Verify no colnames that look like times, or they are just default indices
  expect_equal(coltimes(dummy_epoch_no_time), seq_len(col_num))
  expect_true(is.null(
    colnames(tblData(dummy_epoch_no_time))
    ))

  cropped_epoch_index <- crop(dummy_epoch_no_time, start = 10, end = 20) # Assuming these are indices
  expect_s4_class(cropped_epoch_index, "Epoch")
  expect_equal(ncol(cropped_epoch_index), 11) # 20 - 10 + 1
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

test_that("Epoch subsetting", {
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
})

