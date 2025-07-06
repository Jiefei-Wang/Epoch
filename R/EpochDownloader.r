
##############################
## Utils
##############################
load_single_file <- function(file_path) {
    tryCatch(
        {
            args <- readRDS(file_path)
            do.call(Epoch, args)
        }, 
        error = function(e) {
            file.remove(file_path)
            stop(paste("Error loading file:", file_path, "\n", e$message))
        }
    )
}


# get the file names from the data names
get_file_names <- function(x, i) {
    dataNames <- x@dataNames
    files <- x@files
    files$name[dataNames %in% i]
}

find_nonexist_files <- function(x, i) {
    file_names <- get_file_names(x, i)
    tmp_folder <- x@tmp_folder

    ## list all the files in the tmp folder
    tmp_files <- list.files(tmp_folder, full.names = FALSE)
    non_exist_names <- file_names[!file_names %in% tmp_files]
    non_exist_names
}

download_to_tmp_folder <- function(x, file_names) {
    files <- x@files
    tmp_folder <- x@tmp_folder
    files[files$name %in% file_names, ] |>
        osf_download(
            path = tmp_folder,
            conflicts = "overwrite",
            progress = x@progress
        )
}

##############################
## class
##############################
#' EpochDownloader
#' 
#' @description
#' `EpochDownloader` is a class that allows
#' downloading and accessing files from a OSF project.
#' 
#' @slot id The ID of the OSF project.
#' @slot files The files in the OSF project.
#' @slot dataNames The names of the files in the OSF project.
#' @slot tmp_folder The temporary folder where the files are downloaded.
#' @slot progress Logical indicating whether to show progress during download.
.EpochDownloader <- setClass("EpochDownloader",
    slots = list(
        id = "character",
        files = "ANY",
        dataNames = "character",
        tmp_folder = "character",
        progress = "logical"
    )
)


##############################
## functions
##############################
#' EpochDownloader constructor
#' 
#' @param id Either the ID of the OSF project or the name of the iEEG dataset. Check a list of available projects using `EpochRepos()`. The default points to the fragility data from the Fragility multi-center retrospective study.
#' @param path The path to the temporary folder where the files will be downloaded.
#' @param progress Logical indicating whether to show progress during download.
#' 
#' @return An `EpochDownloader` object.
#' @export 
EpochDownloader <- function(id = EpochRepos()[[1]], 
    progress = TRUE,
    path = file.path(tempdir(), id)) {
    
    # Handle id parameter
    if (length(id) > 1) {
        id <- id[1]
    }
    
    # Check if id is a known project name, otherwise use as OSF project ID
    if (id %in% names(pkg_global$.project_list)) {
        id <- pkg_global$.project_list[[id]]
    }
    
    files <- osf_retrieve_node(id) |>
        osf_ls_files(n_max = Inf)
    ## remove .rds extension from name column
    dataNames <- gsub("\\.rds$", "", files$name)

    ## create the tmp folder if it does not exist
    if (!dir.exists(path)) {
        dir.create(path)
    }

    .EpochDownloader(
        id = id,
        files = files,
        dataNames = dataNames,
        tmp_folder = path,
        progress = progress
    )
}

#' EpochDownloader Methods
#' 
#' @param x An `EpochDownloader` object.
#' 
#' @return `names`: A character vector of file names.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "names", "EpochDownloader",
    function(x) {
        x@dataNames
    }
)


#' @param i Index or name of the files to be accessed.
#' 
#' @return `[`: A list of `Epoch` objects
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "[", "EpochDownloader",
    function(x, i) {
        if (is.numeric(i)) {
            if (!isWholeNumber(i)) {
                stop("Index must be a whole number")
            }
            if(max(i) > length(x@dataNames)) {
                stop(paste("Index out of bounds. The maximum index is", length(x@dataNames)))
            }
            i <- x@dataNames[i]
        }

        dataNames <- x@dataNames
        non_exist_names <- i[!i %in% dataNames]
        if (length(non_exist_names) > 0) {
            stop(paste("The following data do not exist:", paste(non_exist_names, collapse = ", ")))
        }

        # file_names <- get_file_names(x, i)
        non_exist_file_names <- find_nonexist_files(x, i)
        download_to_tmp_folder(x, non_exist_file_names)

        file_names <- get_file_names(x, i)
        file_paths <- file.path(x@tmp_folder, file_names)
        lapply(
            file_paths,
            load_single_file
        )
    }
)



#' @param name The name of the file to be accessed.
#' @return `$`: A single `Epoch` object.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "$", "EpochDownloader",
    function(x, name) {
        x[name][[1]]
    }
)


#' @return `[[`: A single `Epoch` object.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "[[", "EpochDownloader",
    function(x, i) {
        x[i][[1]]
    }
)


#' @param object An `EpochDownloader` object.
#' @return `show`: Prints a summary of the `EpochDownloader` object.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "show", "EpochDownloader",
    function(object) {
        n_show <- min(5, length(object))
        cat("EpochDownloader object\n")
        cat("  - ID:", object@id, "\n")
        cat("  - Number of files:", nrow(object@files), "\n")
        cat(glue("  - Files: {paste(names(object)[1:n_show], collapse = ', ')}"))
        if (length(object) > n_show) {
            cat(glue("...({length(object) - n_show} more)"))
        }
        cat("\n")
        cat("Use $, [, or [[ to access the Epoch Data\n") 
    }
)

#' @return `length`: Returns the number of files in the `EpochDownloader` object.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "length", "EpochDownloader",
    function(x) {
        length(x@dataNames)
    }
)