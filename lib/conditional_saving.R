#' Save data based on project options.
#'
#' A condition saving function for data.
#' Based upon "project_save_data_to_file", "project_save_data_location", "project_save_data_formats".
#'
#' @param data The data.
#' @param name The name of the file.
#' @param ... To the 'save' function.
#' @param .relative_path Manually interject a relative path (ignores project options).
#'
#' @return Nothing
#'
#' @examples
#' # Context: Option("project_save_data_to_file" = TRUE)
#' condition_save_data(bayes_model, "bayes_test")
#' # Message:
#' # "Saving enabled, saving..."
#' # "Saving file to data/bayes_test.RData"
#' #
#' @export
condition_save_data <- function(data , name, ..., .relative_path = NULL){
  if( getOption("project_save_data_to_file") ){
    message("Saving enabled, saving...")

    # Get the name of the inputed data
    data_name <- deparse( substitute(data) )

    # Get project settings
    loc <- getOption("project_save_data_location")
    fmts <- getOption("project_save_data_formats")

    # Check relative path saving ...
    if( !exists("relative_path") & is.null(.relative_path)) relative_path <- ""
    if( !is.null(.relative_path)) relative_path <- .relative_path

    # Get time ...
    time <- ""
    if( getOption("project_bayes_save_with_date_time") ) time <- getOption("project_date_time")

    # Save (across formats)
    walk(fmts, \(fmt){
      filename <- paste0(relative_path, loc, name, time, fmt)
      message("Saving file to:", filename, "\n")
      # Save the data as
      save(list = data_name, file = filename, ...)
        #* So this function gets the object name, from data_name,
        #* and retrieves it from the parent environment, and saves THAT object?
    })

    message("Saved")

  } else {
    message("Saving not enabled, skipping...")
  }
}



#' Save a figure based on the project options.
#'
#' A function to check the conditions to save a figure.
#' Based on "project_save_figs_to_file", "project_save_figs_format",
#' "project_save_figs_with_date_time", "project_save_figs_location".
#'
#' @param data The figure/plot.
#' @param name The name of the file name.
#' @param ... To the 'ggsave' function.
#' @param .suppressMessages Suppress messages.
#' @param .relative_path Manually interject a relative path (ignores project options).
#'
#' @return The plot.
#'
#' @examples
#' # Context: Option("project_save_figs_to_file" = TRUE)
#' condition_save_figure(figure, "fig_1")
#'
#' @export
condition_save_figure <- function(plot, name, ..., .suppressMessages = FALSE, .relative_path = NULL){
  #' Save ggplot figures locally
  #'
  #' A handy function to save ggplots with minimal effort.
  #' Options are set at the project level, such that the function requires only
  #' applying a saving name.

  require(tidyverse)
  require(ggplot2)

  # Check whether the data has the correct data structure to be saved...
  class <- class(plot)

  if(any(class %in% c("gg", "ggplot"))){
    if(!.suppressMessages) message("Valid figure structure found, proceeding...")

    # Get settings
    save_figure <- getOption("project_save_figs_to_file")

    # Check whether we should save anything
    if(save_figure){
      if(!.suppressMessages) message("Saving figure...")

      fmts <- getOption("project_save_figs_formats")

      if(!(any( fmts %in% c(".eps", ".ps", ".tex", ".pdf", ".jpeg", ".tiff", ".png", ".bmp", ".svg")) )){
        warning("Figure format(s) not valid. Saving as .png")
        fmts <- ".png"
      }

      base <- getOption("project_save_figs_location")
      cond <- NULL
      if(getOption("project_save_figs_with_date_time")) cond <- getOption("project_date_time")

      # Check relative saving parameter as "relative_path".
      if(!exists("relative_path") & is.null(.relative_path)) relative_path <- ""
      if(!is.null(.relative_path)) relative_path <- .relative_path

      # Set filename
      filename <- paste0(relative_path, base, name, cond)
      message("Saving files to:", filename, "\n")

      # Save across formats
      walk(fmts, \(fmt){
        if(!.suppressMessages) message("Saving figure with format: ", fmt)
        ggsave(paste0(filename, fmt), plot, ...)
      })

      if(!.suppressMessages) message("Done!")
    } else {
      if(!.suppressMessages) message("Figure saving disabled, skipping...")
    }
  } else {
    warning("Data is not a ggplot object. Cannot save as figure.")
  }
  plot
}

#' Save a table based on the project options.
#'
#' A function to check the conditions to save a figure.
#' Based on "project_save_figs_to_file", "project_save_figs_format",
#' "project_save_figs_with_date_time", "project_save_figs_location".
#'
#' @param data The figure/plot.
#' @param name The name of the file name.
#' @param ... To the 'gtsave' function.
#' @param .suppressMessages Suppress messages.
#' @param .relative_path Manually interject a relative path (ignores project options).
#'
#' @return The plot.
#'
#' @examples
#' # Context: Option("project_save_figs_to_file" = TRUE)
#' condition_save_figure(figure, "fig_1")
#'
#' @export
condition_save_table <- function(table, name, ..., .suppressMessages = FALSE, .relative_path = NULL, .suppress_print = FALSE){
  #' Save gt tables locally
  #'
  #' A handy function to save gt tables with minimal effort.
  #' Options are set at the project level, such that the function requires only
  #' applying a saving name.

  require(tidyverse)
  require(gt)

  # Check whether the data has the correct data structure to be saved...
  class <- class(table)

  if(any(class %in% c("gt_tbl"))){
    if(!.suppressMessages) message("Valid table structure found, proceeding...")

    # Get settings
    save_table <- getOption("project_save_tbls_to_file")

    # Check whether we should save anything
    if(save_table){
      if(!.suppressMessages) message("Saving table...")

      fmts <- getOption("project_save_tbls_formats")

      if(!(any( fmts %in% c(".html", ".tex", ".ltx", ".rtf", ".docx")) )){
        warning("Table format not valid, setting to .rtf...")
        fmts <- ".rtf"
      }

      base <- getOption("project_save_tbls_location")
      cond <- NULL
      if(getOption("project_save_tbls_with_date_time")) cond <- getOption("project_date_time")

      # Check relative saving parameter as "relative_path".
      if(!exists("relative_path") & is.null(.relative_path)) relative_path <- ""
      if(!is.null(.relative_path)) relative_path <- .relative_path

      # Set filename
      filename <- paste0(relative_path, base, name, cond)
      message("Saving files to:", filename, "\n")

      # Save across formats
      walk(fmts, \(fmt){
        if(!.suppressMessages) message("Saving table with format: ", fmt)
        gtsave(table, paste0(filename, fmt), ...)
      })

      if(!.suppressMessages) message("Done!")
    } else {
      if(!.suppressMessages) message("Table saving disabled, skipping...")
    }
  } else {
    warning("Data is not a gt_tbl object. Cannot save as table.")
  }

  if(!.suppress_print){
    table
  }
}



#' Add a meta function to each of the other functions.


#' save_output_cnd <- function(data, name, relative_path = "",  ..., .suppressMessages = FALSE, .suppress_print = FALSE){
#'   #' Save tabel and figure locally
#'   #'
#'   #' A handy function to save ggplots and gttables with minimal effort.
#'   #' Options are set at the project level, such that the function requires only
#'   #' applying a saving name.
#'
#'   #' could add "..., force_diff = FALSE"
#'
#'   require(tidyverse)
#'   require(gt)
#'   require(ggplot2)
#'
#'   # Check whether the data has the correct data structure to be saved...
#'   class <- class(data)
#'
#'   if( any( class %in% c("gg", " ggplot", "gt_tbl") ) ){
#'     if( !.suppressMessages )  message("Valid data structure found, proceeding...")
#'
#'     # Get settings
#'     save_figure <- getOption("project_save_figs_to_file")
#'     save_table  <- getOption("project_save_tbls_to_file")
#'
#'     #' Check whether we should save anything and *get* the respective *parameters*
#'     if( any( save_figure, save_table) ){
#'       # FIGURE
#'       if( save_figure & any( class %in% c("gg", "ggplot") ) ){
#'         if( !.suppressMessages )   message("Saving figure...")
#'         type <- "fig"
#'         fmts <- getOption("project_save_figs_formats")
#'
#'         if( !( any( fmts %in% c(".eps", ".ps", ".tex", ".pdf", ".jpeg", ".tiff", ".png", ".bmp", ".svg") ) ) ){
#'           warning("Figure format not valid, using default .png...")
#'           fmts <- ".png"
#'         }
#'         base <- getOption("project_save_figs_location")
#'         cond <- NULL
#'         if( getOption("project_save_figs_with_date_time") )   cond <- getOption("project_date_time")
#'       }
#'
#'       # TABLE
#'       if( save_table & any( class %in% c("gt_tbl") ) ){
#'         if( !.suppressMessages )   message("Saving table...")
#'
#'         type <- "tbl"
#'         fmts <- getOption("project_save_tbls_formats")
#'         if( !( any( fmts %in% c(".html", ".tex", ".ltx", ".rtf", ".docx") ) ) ){
#'           warning("Table format not valid, setting to .rtf...")
#'           fmts <- ".rtf"
#'         }
#'         base <- getOption("project_save_tbls_location")
#'         cond <- NULL
#'         if( getOption("project_save_tbls_with_date_time") )   cond <- getOption("project_date_time")
#'       }
#'
#'       print(relative_path)
#'       # Check relative saving parameter as "relative_path".
#'       if( !exists("relative_path") & is.null(.relative_path) ) relative_path <- ""
#'       if( !is.null(.relative_path) ) relative_path <- .relative_path
#'
#'       # Set filname
#'       filename <- paste0(relative_path, base, name, cond)
#'       message("Saving files to:", filename, "\n")
#'
#'       # Save across....
#'       walk(fmts, \(fmt){
#'         if( !.suppressMessages ) message("Saving object: ", type, " with format: ", fmt)
#'         if(type == "fig")        ggsave(paste0(filename, fmt), data, ...)
#'         if(type == "tbl")        gtsave(data, paste0(filename, fmt))
#'       })
#'
#'       if( !.suppressMessages )  message("Done!")
#'     } else {
#'       if( !.suppressMessages )  message("Saving disabled, skipping...")
#'     }
#'   }
#'
#'   if( !.suppress_print ){
#'     data
#'   }
#' }

