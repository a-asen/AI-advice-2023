# General loader function
loader <- function(path){
  lapply( path,  \(item){
    message("Loading: ", item)

    # If "dcf":
    if( grepl("\\.dcf$", item, ignore.case = T)){
      strsplit(
        read.dcf( item )[,"libraries"],
        ", "
      )[["libraries"]] |>
        lapply( \(library){
          message("Loading library:", library)
          library(library, character.only = T)
        })
    }

    # If "data":
    if ( grepl("\\.rdata$", item, ignore.case = TRUE) ){
      load(item, envir = .GlobalEnv)
    }

    # If "ccp":
    if( grepl("\\.cpp$", item,  ignore.case = TRUE) ){
      Rcpp::sourceCpp(item)
    }

    # If ".r"
    if ( grepl("\\.r$", item, ignore.case = TRUE) ){
      source(item)
    }
  })
}

#' Load a ProjectTemplate structure in a Quarto document using a relative path
#'
#' @param relative_path The relative path to load *FROM*
#' @param load_libraries (Default = TRUE) Load ProjecTemplate defined libraries?
#' @param load_local_library (Default = TRUE) Load local functions (in the "lib/" folder)?
#' @param load_caches (Default = FALSE) Load data from the "cache/" folder"?
#' @param load_data_data (Default = TRUE) Load data stored in the "data/" folder?
#' @param load_data_scripts (Default = TRUE) Load data scripts stored in the "data/" folder?
#' @param load_munges (Default = TRUE) Load preprocessing scripts stored in the "munge/" folder?
#'
#' @return Nothing, loads data to the environment.
#'
#' @examples
#' relative_path <- "../"
#' source( paste0(relative_path, "lib/quarto_load_project.R") )
#' quarto_load_project(relative_path,)
#'
#' @export
quarto_load_project <- function(relative_path
                                , load_libraries     = TRUE
                                , load_local_library = TRUE
                                , load_caches        = FALSE
                                , load_data_data     = TRUE
                                , load_data_scripts  = TRUE
                                , load_munges        = TRUE   ){
  # Get path
  get_path <- function(path, pattern){
    list.files( paste0(relative_path, path),
                pattern = pattern,
                ignore.case = T,
                full.names = T)
  }

  # Load libraries
  #' Specified in the .dcf file for ProjectTemplate file structure.
  if( load_libraries ){
    message("Loading libraries...")
    loader( get_path("config", "\\.dcf$") )
  }

  #' Load local libraries
  if( load_local_library ){
    message("Loading local libraries...")
    loader( get_path("lib", ".r$") )
    loader( get_path("lib", "\\.cpp$") )
  }

  # Data
  if( load_data_data ){
    message("Loading data (from 'data/'...")
    loader( get_path("data", "\\.rdata$") )
  }
  # Data scripts
  if( load_data_scripts ){
    message("Loading data scripts (from 'data/'...")
    loader (get_path("data", "\\.r$") )
  }

  # Cache
  if( load_caches ){
    message("Loading cache (from 'cache/'...")
    loader( get_path("cache", "\\.rdata$") )
  }

  # Munge
  if( load_munges ){
    message("Loading munges (from 'munge/'...")
    loader( get_path("munge", "\\.r$") )
  }
}
