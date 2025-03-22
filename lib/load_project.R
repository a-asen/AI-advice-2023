load_project <- function(relative_path, lib=TRUE, data=TRUE, munge=TRUE, cache=FALSE, data_pattern = "\\.r"){
  #' Function to load libraries, data, and munging for .qmds.

  # Get higher-order libraries
  load_libraries <- lapply(
    read.dcf(paste0(relative_path, "config/global.dcf"))[,"libraries"],
    \(x){ strsplit(x, ", ") } )[[1]][[1]]

  # Load higher-order libraries
  lapply(load_libraries, \(library){
    cat("Loading library:", library, "\n")
    library(library, character.only = T)
  })

  # Get local libraries, data, and munge paths...
  get_path <- function(path, pattern){
    list.files(paste0(relative_path, path), pattern = pattern,
               ignore.case = T, full.names = T)
  }

  # Paths:
  load_path <- c()
  if(lib) load_path   <- c(load_path, get_path("lib",   "\\.r") )
  if(cache) load_path <- c(load_path, get_path("cache", "\\.rdata") )
  if(data) load_path  <- c(load_path, get_path("data",  data_pattern) )
  if(munge) load_path <- c(load_path, get_path("munge", "\\.r") )

  # Load local data/functions/pre-processing
  lapply(load_path, \(path){
    message("Loading ", path)

    if( str_ends(path, regex("\\.rdata", ignore_case=T)) ){
      load(path, envir = .GlobalEnv)
    } else {
      source(path)
    }
  })
}
