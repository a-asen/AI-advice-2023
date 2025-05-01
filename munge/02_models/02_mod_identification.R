#* Detection of AI.

if( getOption("project_bayes_run_models") ){
  mod_detect <- list()

  # Base model      =====
  mod_detect[["base"]] <- brms::brm(
    sum | trials(length) ~ true_answer + (1 | ResponseId)
    , data = data[["detect"]],
    , backend = "cmdstanr", family = binomial(link = "logit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_detect[["base"]], ndraws=50)
    bayes_chain_stab( mod_detect[["base"]])
    bayes_diag(       mod_detect[["base"]])
  }

  # Base model + random intercept for questions
  mod_detect[["base+Rques"]] <- brms::brm(
    sum | trials(length) ~ true_answer + (1 | ResponseId) + (1 | question)
    , data = data[["detect"]],
    , backend = "cmdstanr", family = binomial(link = "logit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_detect[["base+Rques"]], ndraws=50)
    bayes_chain_stab( mod_detect[["base+Rques"]])
    bayes_diag(       mod_detect[["base+Rques"]])
  }

  # COMPARE base and RANDOM question intercept
  loo_compare(mod_detect[["base"]], mod_detect[["base+Rques"]])
  #' adding a random intercept for the questions does not improve the model.
  #' Continue without.


  # AI expertice        ======
  # Control for AI expertice

  # Add AI expertice
  mod_detect[["+exp"]] <- brms::brm(
    sum | trials(length) ~ true_answer + expertice + (1 | ResponseId)
    , data = data[["detect"]],
    , backend = "cmdstanr", family = binomial(link = "logit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_detect[["+exp"]], ndraws=50)
    bayes_chain_stab( mod_detect[["+exp"]])
    bayes_diag(       mod_detect[["+exp"]])
  }

  # COMPARE: BASE vs +EXP
  loo_compare(mod_detect[["base"]], mod_detect[["+exp"]])
  #' Adding expertice does not really improve the model.


  # Interactive expertice
  mod_detect[["*exp"]] <- brms::brm(
    sum | trials(length) ~ true_answer * expertice + (1 | ResponseId)
    , data = data[["detect"]],
    , backend = "cmdstanr", family = binomial(link = "logit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnose
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_detect[["*exp"]], ndraws=50)
    bayes_chain_stab( mod_detect[["*exp"]])
    bayes_diag(       mod_detect[["*exp"]])
  }

  # COMPARE
  loo_compare(mod_detect[["+exp"]], mod_detect[["*exp"]])
  #' Interactive is better than simple effect.

  loo_compare(mod_detect[["base"]], mod_detect[["*exp"]])
  #' But interactive expertice is not better than the base model.


  #' save only the relevant model:
  mod.detect <- mod_detect[["base"]]
  condition_save_data( mod.detect, "model_identification_study" )
}
