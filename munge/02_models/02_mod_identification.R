#* Detection of AI.

if( getOption("project_bayes_run_models") ){
  mod.detect <- list()

  # Base model      =====
  mod.detect[["base"]] <- brms::brm(
    sum | trials(length) ~ 0 + true_answer + (1 | ResponseId)
    , data = data[["detect"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.detect[["base"]], ndraws=50)
    bayes_chain_stab( mod.detect[["base"]])
    bayes_diag(       mod.detect[["base"]])
  }

  # Base model + random intercept for questions
  mod.detect[["base+Rques"]] <- brms::brm(
    sum | trials(length) ~ 0 + true_answer + (1 | ResponseId) + (1 | question)
    , data = data[["detect"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.detect[["base+Rques"]], ndraws=50)
    bayes_chain_stab( mod.detect[["base+Rques"]])
    bayes_diag(       mod.detect[["base+Rques"]])
  }

  # COMPARE base and RANDOM question intercept
  loo_compare(mod.detect[["base"]], mod.detect[["base+Rques"]])
  #' adding a random intercept for the questions does not improve the model.
  #' Continue without.


  # AI expertice        ======
  # Control for AI expertice

  # Add AI expertice
  mod.detect[["+exp"]] <- brms::brm(
    sum | trials(length) ~ 0 + true_answer + mo(expertice) + (1 | ResponseId)
    , data = data[["detect"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.detect[["+exp"]], ndraws=50)
    bayes_chain_stab( mod.detect[["+exp"]])
    bayes_diag(       mod.detect[["+exp"]])
  }

  # COMPARE: BASE vs +EXP
  loo_compare(mod.detect[["base"]], mod.detect[["+exp"]])
  #' Adding expertice does not really improve the model.


  # Interactive expertice
   mod.detect[["*exp"]] <- brms::brm(
    sum | trials(length) ~ 0 + true_answer * mo(expertice) + (1 | ResponseId)
    , data = data[["detect"]]
    , backend = "cmdstanr", family = binomial(link = "logit")
    , chains = 6, iter = 6000 ) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))

  # Diagnose
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(   mod.detect[["*exp"]], ndraws=50)
    bayes_chain_stab( mod.detect[["*exp"]])
    bayes_diag(       mod.detect[["*exp"]])
  }

  bayes_tbl_sum(mod.detect$`*exp`, coef_calc = "plogis", coef_except = "expertice")

  # COMPARE
  loo_compare(mod.detect[["+exp"]], mod.detect[["*exp"]])
  #' Interactive is better than simple effect.

  loo_compare(mod.detect[["base"]], mod.detect[["*exp"]])
  #' Interactive with expertice IS better than the base model.


  #' save only the relevant model:
  conditional_save( mod.detect, "model_identification_study" )
}

#' While the modelswith "expertice" included tend to do slightly better,
#' the difference is not huge. MNoreover, since we do not have a large sample,
#' we will choose to keep things simple by using the simple model with a random
#' intercept for subejects
mod.detect <- mod.detect[["base"]]
