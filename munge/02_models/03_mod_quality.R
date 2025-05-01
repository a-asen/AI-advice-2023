#* Quality of human and AI advice.


#* Detection of AI.

if( getOption("project_bayes_run_models") ){
  mod_quality <- list()

  # HELPFULNESS       ======
  ## Base model      =====
  mod_quality[["help"]][["base"]] <- brms::brm(
    Helpfulness ~ Source + (1 | ResponseId)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["help"]][["base"]], ndraws=50)
    bayes_chain_stab( mod_quality[["help"]][["base"]])
    bayes_diag(       mod_quality[["help"]][["base"]])
  }

  ## Base + random intercept for quesitons        ======
  mod_quality[["help"]][["base+Rq"]] <- brms::brm(
    Helpfulness ~ Source + (1 | ResponseId) + (1 | Question)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["help"]][["base+Rq"]], ndraws=50)
    bayes_chain_stab( mod_quality[["help"]][["base+Rq"]])
    bayes_diag(       mod_quality[["help"]][["base+Rq"]])
  }

  ## COMPARE       =====
  loo_compare(mod_quality[["help"]][["base"]], mod_quality[["help"]][["base+Rq"]])
  #' adding a random intercept for the questions DOES improve the model.
  #' Continue WITH


  # EFFECTIVENESS         ======
  ## Base model           ======
  mod_quality[["eff"]][["base"]] <- brms::brm(
    Effectiveness ~ Source + (1 | ResponseId)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["eff"]][["base"]], ndraws=50)
    bayes_chain_stab( mod_quality[["eff"]][["base"]])
    bayes_diag(       mod_quality[["eff"]][["base"]])
  }

  ## Base + random intercept for questions      ========
  mod_quality[["eff"]][["base+Rq"]] <- brms::brm(
    Effectiveness ~ Source + (1 | ResponseId) + (1 | Question)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["eff"]][["base+Rq"]], ndraws=50)
    bayes_chain_stab( mod_quality[["eff"]][["base+Rq"]])
    bayes_diag(       mod_quality[["eff"]][["base+Rq"]])
  }

  ## COMPARE      =======
  loo_compare(mod_quality[["eff"]][["base"]], mod_quality[["eff"]][["base+Rq"]])
  #' Adding a random intercept for the questions improve the model.


  # APPROPRIATENESS         ======
  ## Base model             =======
  mod_quality[["appr"]][["base"]] <- brms::brm(
    Appropriateness ~ Source + (1 | ResponseId)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["appr"]][["base"]], ndraws=50)
    bayes_chain_stab( mod_quality[["appr"]][["base"]])
    bayes_diag(       mod_quality[["appr"]][["base"]])
  }

  ## Base + random intercept for questions      =======
  mod_quality[["appr"]][["base+Rq"]] <- brms::brm(
    Appropriateness ~ Source + (1 | ResponseId) + (1 | Question)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["appr"]][["base+Rq"]], ndraws=50)
    bayes_chain_stab( mod_quality[["appr"]][["base+Rq"]])
    bayes_diag(       mod_quality[["appr"]][["base+Rq"]])
  }

  ## COMPARE      =======
  loo_compare(mod_quality[["appr"]][["base+Rq"]], mod_quality[["appr"]][["base"]])
  #' random intercept is generally not better than with


  # SENSITIVITY           ======
  ## Base model           ======
  mod_quality[["sens"]][["base"]] <- brms::brm(
    Sensitivity ~ Source + (1 | ResponseId)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["sens"]][["base"]], ndraws=50)
    bayes_chain_stab( mod_quality[["sens"]][["base"]])
    bayes_diag(       mod_quality[["sens"]][["base"]])
  }

  ## Base + random intercept for questions      =======
  mod_quality[["sens"]][["base+Rq"]] <- brms::brm(
    Sensitivity ~ Source + (1 | ResponseId) + (1 | Question)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["sens"]][["base+Rq"]], ndraws=50)
    bayes_chain_stab( mod_quality[["sens"]][["base+Rq"]])
    bayes_diag(       mod_quality[["sens"]][["base+Rq"]])
  }

  ## COMPARE      =======
  loo_compare(mod_quality[["sens"]][["base+Rq"]], mod_quality[["sens"]][["base"]])
  #' Random intercept does not really improve the model.


  # Save      ======
  #' Model selecting select models
  #' The difference between the models with a random intercept and those
  #' without is rather small, but some models greatly benefited by adding a
  #' random intercept. Thus, retaining the random intercept for the other models
  #' is fine, and then it also remain more comparable between models.

  mod.quality <- list(
    help = mod_quality[["help"]][["base+Rq"]],
    eff  = mod_quality[["eff"]][["base+Rq"]],
    appr = mod_quality[["appr"]][["base+Rq"]],
    sens = mod_quality[["sens"]][["base+Rq"]]
  )

  #' save only the relevant model:
  condition_save_data( mod.quality, "model_quality_study" )



  # SENSITIVITY           ======
  ## Base model           ======
  mod_quality[["full"]][["base"]] <- brms::brm(
     Rating ~ Source * Dimension + (1 | ResponseId)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["full"]][["base"]], ndraws=50)
    bayes_chain_stab( mod_quality[["full"]][["base"]])
    bayes_diag(       mod_quality[["full"]][["base"]])
  }

  ## Base + random intercept for questions      =======
  mod_quality[["sens"]][["base+Rq"]] <- brms::brm(
    Sensitivity ~ Source + (1 | ResponseId) + (1 | Question)
    , data = data[["quality"]]
    , backend = "cmdstanr", family = cumulative("probit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_quality[["sens"]][["base+Rq"]], ndraws=50)
    bayes_chain_stab( mod_quality[["sens"]][["base+Rq"]])
    bayes_diag(       mod_quality[["sens"]][["base+Rq"]])
  }

  ## COMPARE      =======
  loo_compare(mod_quality[["sens"]][["base+Rq"]], mod_quality[["sens"]][["base"]])
  #' Random intercept does not really improve the model.


}

