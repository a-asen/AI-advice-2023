#* Quality of human and AI advice.

#* Detection of AI.

if( getOption("project_bayes_run_models") ){
  mod.quality <- list()

  # visualize
  data[["quality"]] |>
    pivot_longer(c(Helpfulness, Effectiveness, Appropriateness, Sensitivity)) |>
    ggplot(aes(as.numeric(value))) +
    facet_wrap(~name) +
    geom_histogram()

  # HELPFULNESS       ======
  ## Base model      =====
  mod.quality[["help"]][["base"]] <- brms::brm(
    Helpfulness ~ Source + (1 | ResponseId)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.quality[["help"]][["base"]], ndraws=50)
    bayes_chain_stab( mod.quality[["help"]][["base"]])
    bayes_diag(       mod.quality[["help"]][["base"]])
  }

  ## Base + random intercept for quesitons        ======
  mod.quality[["help"]][["base+Rq"]] <- brms::brm(
    Helpfulness ~ Source + (1 | ResponseId) + (1 | Question)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.quality[["help"]][["base+Rq"]], ndraws=50)
    bayes_chain_stab( mod.quality[["help"]][["base+Rq"]])
    bayes_diag(       mod.quality[["help"]][["base+Rq"]])
  }

  ## COMPARE       =====
  loo_compare(mod.quality[["help"]][["base"]], mod.quality[["help"]][["base+Rq"]])
  #' adding a random intercept for the questions DOES improve the model.
  #' Continue WITH


  # EFFECTIVENESS         ======
  ## Base model           ======
  mod.quality[["eff"]][["base"]] <- brms::brm(
    Effectiveness ~ Source + (1 | ResponseId)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.quality[["eff"]][["base"]], ndraws=50)
    bayes_chain_stab( mod.quality[["eff"]][["base"]])
    bayes_diag(       mod.quality[["eff"]][["base"]])
  }

  ## Base + random intercept for questions      ========
  mod.quality[["eff"]][["base+Rq"]] <- brms::brm(
    Effectiveness ~ Source + (1 | ResponseId) + (1 | Question)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.quality[["eff"]][["base+Rq"]], ndraws=50)
    bayes_chain_stab( mod.quality[["eff"]][["base+Rq"]])
    bayes_diag(       mod.quality[["eff"]][["base+Rq"]])
  }

  ## COMPARE      =======
  loo_compare(mod.quality[["eff"]][["base"]], mod.quality[["eff"]][["base+Rq"]])
  #' Adding a random intercept for the questions improve the model.


  # APPROPRIATENESS         ======
  ## Base model             =======
  mod.quality[["appr"]][["base"]] <- brms::brm(
    Appropriateness ~ Source + (1 | ResponseId)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.quality[["appr"]][["base"]], ndraws=50)
    bayes_chain_stab( mod.quality[["appr"]][["base"]])
    bayes_diag(       mod.quality[["appr"]][["base"]])
  }

  ## Base + random intercept for questions      =======
  mod.quality[["appr"]][["base+Rq"]] <- brms::brm(
    Appropriateness ~ Source + (1 | ResponseId) + (1 | Question)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.quality[["appr"]][["base+Rq"]], ndraws=50)
    bayes_chain_stab( mod.quality[["appr"]][["base+Rq"]])
    bayes_diag(       mod.quality[["appr"]][["base+Rq"]])
  }

  ## COMPARE      =======
  loo_compare(mod.quality[["appr"]][["base+Rq"]], mod.quality[["appr"]][["base"]])
  #' random intercept is generally not better than with


  # SENSITIVITY           ======
  ## Base model           ======
  mod.quality[["sens"]][["base"]] <- brms::brm(
    Sensitivity ~ Source + (1 | ResponseId)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.quality[["sens"]][["base"]], ndraws=50)
    bayes_chain_stab( mod.quality[["sens"]][["base"]])
    bayes_diag(       mod.quality[["sens"]][["base"]])
  }

  ## Base + random intercept for questions      =======
  mod.quality[["sens"]][["base+Rq"]] <- brms::brm(
    Sensitivity ~ Source + (1 | ResponseId) + (1 | Question)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.quality[["sens"]][["base+Rq"]], ndraws=50)
    bayes_chain_stab( mod.quality[["sens"]][["base+Rq"]])
    bayes_diag(       mod.quality[["sens"]][["base+Rq"]])
  }

  ## COMPARE      =======
  loo_compare(mod.quality[["sens"]][["base+Rq"]], mod.quality[["sens"]][["base"]])
  #' Random intercept does not really improve the model.


  # SAVE
  conditional_save(mod.quality, "model_quality_study")
}

#' Model selecting select models
#' The difference between the models with a random intercept and those
#' without is rather small, one model benefited by including a random intercept of question
#' but the other did not. To better compare the models, we will leave out the random intercept of the question.
#'
#' Stick with the "base" model.
mod.quality <- map(mod.quality, \(mod){
  mod[["base"]]
})


