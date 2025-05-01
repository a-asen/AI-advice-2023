#* Preference for human versus AI answer.

if( getOption("project_bayes_run_models") ){
  mod_pref <- list()

  # Base model
  mod_pref[["base"]] <- brms::brm(
    sum | trials(length) ~ model + (1 | ResponseId)
    , data[["pref"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_pref[["base"]], ndraws=50)
    bayes_chain_stab( mod_pref[["base"]])
    bayes_diag(       mod_pref[["base"]])
  }

  # random intercept for questions
  mod_pref[["base+Rques"]] <- brms::brm(
    sum | trials(length) ~ model + (1 | ResponseId) + (1 | question)
    , data[["pref"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod_pref[["base+Rques"]], ndraws=50)
    bayes_chain_stab( mod_pref[["base+Rques"]])
    bayes_diag(       mod_pref[["base+Rques"]])
  }


  loo_compare(mod_pref[["base"]], mod_pref[["base+Rques"]])
  #* adding a random intercept for the questions does not really change the model.
  #* Continue without.


  #* Checking effect of trust on model choice:
  # Add base comparison model
  mod_pref[["base_comp"]] <- brms::brm(
    sum | trials(length) ~ model + (1 | ResponseId)
    , data[["pref"]] |> filter(!is.na(trust))
    , backend = "cmdstanr", family = binomial(link = "logit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))


  # Model  + trust
  mod_pref[["+trust"]]  <- brms::brm(
    sum | trials(length) ~ model + trust + (1 | ResponseId)
    , data[["pref"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  if( getOption("project_bayes_diagnostics") ){
    pp_check(         mod_pref[["+trust"]], ndraws = 50)
    bayes_chain_stab( mod_pref[["+trust"]])
    bayes_diag(       mod_pref[["+trust"]])
  }


  loo_compare( mod_pref[["base_comp"]], mod_pref[["+trust"]] )
  #* unclear whether the trust is preferred

  # Model  * trust
  mod_pref[["*trust"]] <- brms::brm(
    sum | trials(length) ~ model * trust + (1 | ResponseId)
    , data[["pref"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , init = 0, chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  if( getOption("project_bayes_diagnostics") ){
    pp_check(         mod_pref[["*trust"]], ndraws = 50)
    bayes_chain_stab( mod_pref[["*trust"]])
    bayes_diag(       mod_pref[["*trust"]])
  }

  # COMPARE

  loo_compare(mod_pref[["+trust"]], mod_pref[["*trust"]])
  # Interactive trust is preferred above non-interactive

  loo_compare(mod_pref[["base_comp"]], mod_pref[["*trust"]])
  # But the interactive trust is not preferred above the base model


  mod.pref <- mod_pref[["base"]]
  condition_save_data(mod.pref, "model_preference_study")
}
