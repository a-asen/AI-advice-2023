#'                    **PROJECT PARAMETERS**                              ======

#' Set general project parameters
options(
  # Get current date/time
  project_date_time = format( Sys.time(), "_%Y-%m-%d_%H-%M-%S_"),

  # Figure related settings
  project_save_figs_to_file = TRUE,
  project_save_figs_location = "outputs/figs/",
  project_save_figs_formats = c(".svg", ".jpeg"),
  project_save_figs_with_date_time = FALSE,

  # Table related settings
  project_save_tbls_location = "outputs/tbls/",
  project_save_tbls_to_file = TRUE,
  project_save_tbls_formats = c(".html", ".docx"),
  project_save_tbls_with_date_time = FALSE,

  # Aesthetics:
  project_custom_ggplot = TRUE,
  project_custom_ggplot_colours = c("#F8766D", "#619CFF", "#f8ea6d"),
  project_custom_ggplot_colours2 = c("#619CFF","#0046bc","#F8766D","#b20b00"),

  # Bayesian options:
  project_bayes_run_models = FALSE,
  project_bayes_save_to_file = FALSE,
  project_bayes_save_with_date_time = TRUE,
  project_bayes_diagnostics = TRUE,

  mc.cores = 6,   #' or as many as you have
  brms.backend = "cmdstanr",


  #' Because partial matching exists, warn whenever such an instance occur,
  #' as this can be especially problematic for statistical analysis.
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

#' If conditions can take vectors but only evaluate the first on,
#' this will add a warning.
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_"=1)

# Custom colours      =====
#'  custom colours for ggplot (if enabled)
if( getOption("project_custom_ggplot") ){
  options(
    ggplot2.continous.colour = getOption("project_custom_ggplot_colours"),
    ggplot2.continous.fill   = getOption("project_custom_ggplot_colours"),
    ggplot2.discrete.colour  = getOption("project_custom_ggplot_colours"),
    ggplot2.discrete.fill    = getOption("project_custom_ggplot_colours")
  )
  theme_set( theme_bw() )
}


#'                       **SET VARIABLES**                                   =======
data_loss <- list(
  study1 = list(),
  study2 = list(),
  study3 = list()
)
