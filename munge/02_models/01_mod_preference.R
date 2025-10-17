#* Preference for human versus AI answer.

if( getOption("project_bayes_run_models") ){
  mod.pref <- list()

  # Base model
  mod.pref[["base"]] <- brms::brm(
    sum | trials(length) ~ 0 + model + (1 | ResponseId)
    , data[["pref"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.pref[["base"]], ndraws=50)
    bayes_chain_stab( mod.pref[["base"]])
    bayes_diag(       mod.pref[["base"]])
  }

  # random intercept for questions
  mod.pref[["base+Rques"]] <- brms::brm(
    sum | trials(length) ~ 0 + model + (1 | ResponseId) + (1 | question)
    , data[["pref"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.pref[["base+Rques"]], ndraws=50)
    bayes_chain_stab( mod.pref[["base+Rques"]])
    bayes_diag(       mod.pref[["base+Rques"]])
  }

  loo_compare(mod.pref[["base"]], mod.pref[["base+Rques"]])
  #* adding a random intercept for the questions does not really change the model.
  #* Continue without.


  #* Checking effect of trust on model choice:
  # Add base comparison model
  mod.pref[["base_comp"]] <- brms::brm(
    sum | trials(length) ~ 0 + model + (1 | ResponseId)
    , data[["pref"]] |> filter(!is.na(trust))
    , backend = "cmdstanr", family = binomial(link = "logit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  # Model  + trust
  mod.pref[["+trust"]]  <- brms::brm(
    sum | trials(length) ~ 0 + model + trust + (1 | ResponseId)
    , data[["pref"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  if( getOption("project_bayes_diagnostics") ){
    pp_check(         mod.pref[["+trust"]], ndraws = 50)
    bayes_chain_stab( mod.pref[["+trust"]])
    bayes_diag(       mod.pref[["+trust"]])
  }


  loo_compare( mod.pref[["base_comp"]], mod.pref[["+trust"]] )
  #* unclear whether the trust is preferred

  # Model  * trust
  mod.pref[["*trust"]] <- brms::brm(
    sum | trials(length) ~ 0 + model * trust + (1 | ResponseId)
    , data[["pref"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  if( getOption("project_bayes_diagnostics") ){
    pp_check(         mod.pref[["*trust"]], ndraws = 50)
    bayes_chain_stab( mod.pref[["*trust"]])
    bayes_diag(       mod.pref[["*trust"]])
  }

  # COMPARE interaction trust
  loo_compare(mod.pref[["+trust"]], mod.pref[["*trust"]])
  # Interactive trust is preferred above non-interactive

  # Compare interaction trust vs base.
  loo_compare(mod.pref[["base_comp"]], mod.pref[["*trust"]])
  #' The base fits the model slightly less than the interaction with trust,
  #' BUT the interaction has a fairly high error margin.


  conditional_save(mod.pref, "model_preference_study")
}

# For the analysis, we select the base model.
mod.pref <- mod.pref[["base_comp"]]

