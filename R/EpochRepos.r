##############################
## Utils
##############################

#' Get configuration data from remote URL
#' 
#' @return A list of project configurations
get_config_data <- function() {
    url <- "https://raw.githubusercontent.com/Jiefei-Wang/EpochData/refs/heads/main/config.json"
    
    tryCatch({
        config <- fromJSON(url)
        config <- config[[.config_version]]
        return(list(config=config, success = TRUE))
    }, error = function(e) {
        message("Failed to fetch remote configuration: ", e$message)
        return(list(config = .default_project_list, success = FALSE))
    })
}



##############################
## functions
##############################

#' Update downloader repositories configuration
#' 
#' @description
#' Manually update the project list from the remote repository.
#' This function will attempt to download the latest configuration
#' from the GitHub repository. If it fails, the current configuration
#' will remain unchanged.
#' 
#' @param verbose Logical indicating whether to show messages
#' @return Invisibly returns TRUE if successful, FALSE if failed
#' @export
update_downloader_repos <- function(verbose = TRUE) {
    if (verbose)
        message("Attempting to update project repository configuration...")
    
    result <- get_config_data()
    new_config <- result$config
    success <- result$success
    
    # Check if we got the default config
    if (success) {
        pkg_global$.project_list <- new_config
        if (verbose){
            message("Project repository configuration updated successfully.")
            message("Available projects: ", paste(names(pkg_global$.project_list), collapse = ", "))
        }
    } else {
        if (verbose)
            message("Failed to update configuration. Current settings remain unchanged.")
    }
    return(invisible(success))
}

#' Get the list of available projects
#' 
#' @description
#' This function returns the list of available projects.
#' If the configuration has not been fetched yet, it will
#' automatically update the project list from the remote repository.
#' 
#' @return A list of project names and their corresponding OSF project IDs.
#' @examples EpochRepos()
#' @export
EpochRepos <- function(){
    if (!pkg_global$.config_fetched) {
        update_downloader_repos(verbose = TRUE) 
        pkg_global$.config_fetched <- TRUE
    }
    pkg_global$.project_list
}
