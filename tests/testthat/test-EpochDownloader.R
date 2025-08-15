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

test_id <- "fragility"
downloader <- EpochDownloader(id = test_id, verbose = FALSE)

test_that("EpochDownloader constructor initializes correctly", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  expect_s4_class(downloader, "EpochDownloader")
  expect_true(dir.exists(downloader@tmp_folder))
  expect_type(downloader@dataNames, "character")
  expect_gt(length(downloader@dataNames), 0)
})

test_that("Only .rds files are included in files and dataNames", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  # All files should end with .rds
  expect_true(all(grepl("\\.rds$", downloader@files$name, ignore.case = TRUE)))
  
  # dataNames should correspond to filename minus .rds
  expect_identical(
    downloader@dataNames,
    gsub("\\.rds$", "", downloader@files$name, ignore.case = TRUE)
  )
  
  test_id_2 <- "e81xl"
  
  downloader_2 <- EpochDownloader(id = test_id_2, verbose = FALSE)
  
  # dataNames should correspond to filename minus .rds
  expect_true(length(downloader_2@dataNames) == 0)
  
  # Ensure no non-.rds files are in dataNames
  expect_false(any(grepl("\\.csv$|\\.txt$|\\.docx$|\\.xls$|\\.xlsx$", downloader_2@dataNames, ignore.case = TRUE)))
  
})

test_that("EpochDownloader filters .rds files correctly", {
  # Fake OSF file listing
  fake_files <- data.frame(
    name = c(
      "file1.rds",
      "file2.RDS",    # uppercase extension
      "file3.csv",
      "notes.txt"
    ),
    stringsAsFactors = FALSE
  )

  # Mock functions
  mock_osf_retrieve_node <- function(id) {
    structure(list(id = id), class = "osf_tbl_node")
  }
  mock_osf_ls_files <- function(node, n_max) {
    fake_files
  }

  # Temporarily replace osf functions
  with_mocked_bindings(
    osf_retrieve_node = mock_osf_retrieve_node,
    osf_ls_files      = mock_osf_ls_files,
    {
      downloader <- EpochDownloader(id = "fakeid", path = tempdir())

      # Expect 'files' slot to contain less file entries
      expect_false(identical(downloader@files, fake_files))

      # Expect 'dataNames' to only contain .rds files, without extension
      expect_identical(
        downloader@dataNames,
        c("file1", "file2") # only RDS files, name stripped
      )

      # Ensure no non-.rds files are in dataNames
      expect_false(any(grepl("\\.csv$|\\.txt$", downloader@dataNames, ignore.case = TRUE)))
    }
  )
})

test_that("names() returns correct data names", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  expect_equal(names(downloader), downloader@dataNames)
})


test_that("length() returns correct number of files", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  expect_equal(length(downloader), length(downloader@dataNames))
})

test_that("[ indexing returns correct number of Epoch objects", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  subset_names <- downloader@dataNames[1:2]
  result <- downloader[subset_names]
  
  expect_type(result, "list")
  expect_length(result, 2)
  
  expect_s4_class(result[[1]], "Epoch")
})


test_that("[[ returns a single Epoch object", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  obj <- downloader[[1]]
  
  expect_true(!is.null(obj))
  expect_s4_class(obj, "Epoch")
})


test_that("$ returns a single Epoch object by name", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  name1 <- downloader@dataNames[1]
  
  obj <- do.call("$", list(downloader, name1))
  
  expect_true(!is.null(obj))
  expect_s4_class(obj, "Epoch")
})


test_that("Invalid index or name throws an error", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  expect_error(downloader[9999], "Index out of bounds")
  expect_error(downloader["nonexistent_name"], "do not exist")
})

test_that("get_file_names matches input names", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  sample_name <- downloader@dataNames[1]
  file_name <- get_file_names(downloader, sample_name)
  
  expect_true(is.character(file_name))
  expect_true(grepl("\\.rds$", file_name))
})


test_that("find_nonexist_files identifies missing files", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  sample_name <- downloader@dataNames[1]
  sample_file <- get_file_names(downloader, sample_name)
  
  # Ensure file is removed if it exists
  file_path <- file.path(downloader@tmp_folder, sample_file)
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
  
  non_exist <- find_nonexist_files(downloader, sample_name)
  expect_equal(non_exist, sample_file)
})

test_that("show method prints summary", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  expect_output(show(downloader), "EpochDownloader object")
  expect_output(show(downloader), downloader@id)
})


test_that("EpochDownloader gives custom error for 404", {
  expect_error(
    EpochDownloader("fakeid"),
    "Invalid OSF ID format"
  )
})

test_that("EpochDownloader gives The ID does not exist or you do not have permission to access it", {
  expect_error(
    EpochDownloader("aaaaa"),
    "The ID does not exist or you do not have permission to access it"
  )
})

test_that("EpochDownloader rethrows non-404 errors", {
  with_mocked_bindings(
    osf_retrieve_node = function(id) {
      stop("HTTP error 500", call. = FALSE)
    },
    {
      expect_error(
        EpochDownloader("badid"),
        "HTTP error 500"
      )
    }
  )
})

test_that("[ indexing rejects non-whole number", {
  #downloader <- EpochDownloader(id = test_id, verbose = FALSE)
  
  expect_error(downloader[1.5], "must be a whole number")
  expect_error(downloader[[integer(0)]], "`\\[\\[` expects a single index")
  expect_error(downloader[9999], "out of bounds")
  expect_no_error(downloader[1])
})

test_that("[[ enforces length 1", {
  #downloader <- EpochDownloader (id = test_id, verbose = FALSE)
  expect_error(downloader[[c('FragilityData_subpt2_3', 'FragilityData_subpt13_2')]], "`\\[\\[` expects a single index")
  expect_silent(downloader[['FragilityData_subpt2_3']])
})

