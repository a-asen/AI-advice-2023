# Custom colours      =====
#'  custom colours for ggplot (if enabled)
if( getOption("project_custom_ggplot") ){
  options(
    ggplot2.continous.colour  = gen_col("brBR")
    , ggplot2.continous.fill  = gen_col("brBR")
    , ggplot2.discrete.colour = gen_col("brBR")
    , ggplot2.discrete.fill   = gen_col("brBR")
  )
  theme_set( theme_bw() )
}


loss <- list(
  pref    = list(),
  detect  = list(),
  quality = list()
)

raw_data <- list(
  pref    = list(),
  detect  = list(),
  quality = list()
)

pre_data <- list(
  pref    = list(),
  detect  = list(),
  quality = list()
)

data <- list(
  pref    = list(),
  detect  = list(),
  quality = list()
)

figs <- list()
tbls <- list()
