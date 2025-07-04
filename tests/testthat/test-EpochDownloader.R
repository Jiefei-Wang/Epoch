library(testthat)
library(Epoch) # Assuming your package name is Epoch

# This test will try to download actual data, ensure network connectivity
# and that the downloader is configured correctly.

# Initialize the downloader
dl <- EpochDownloader()

test_that("EpochDownloader initialization and listing", {
  expect_s4_class(dl, "EpochDownloader")
  # Check if names(dl) returns a character vector of available epochs
  available_epochs <- names(dl)
  expect_type(available_epochs, "character")
  expect_gt(length(available_epochs), 0) # Expect at least one epoch to be available
})

# test_that("EpochDownloader downloads an epoch successfully", {
#     skip("Skip download test to reduce the server burden")
#   # Attempt to download the first available epoch
#   # This is a live test and depends on the downloader's source being available
#   if (length(names(dl)) > 0) {
#     first_epoch_name <- names(dl)[1]
    
#     expect_no_error({
#       epoch <- dl[[first_epoch_name]]
#     })
#     expect_s4_class(epoch, "Epoch")
#   } else {
#     skip("No epochs available from EpochDownloader to test download.")
#   }
# })
