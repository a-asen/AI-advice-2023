m <- list()
m$d   <- as_draws_df(mod.detect)
m$p   <- as_draws_df(mod.pref)
m$q.h <- as_draws_df(mod.quality$help)
m$q.e <- as_draws_df(mod.quality$eff)
m$q.a <- as_draws_df(mod.quality$appr)
m$q.s <- as_draws_df(mod.quality$sens)


# Add differences between models (identification and preference study)
m$p <- m$p |>
  mutate(
    diff_3.0_3.5 = b_modelGPT3.0 - b_modelGPT3.5,
    diff_3.0_4.0 = b_modelGPT3.0 - b_modelGPT4.0,
    diff_3.5_4.0 = b_modelGPT3.5 - b_modelGPT4.0
  )

m$d <- m$d |>
  mutate(
    diff_3.0_3.5   = b_true_answerGPT3.0 - b_true_answerGPT3.5,
    diff_3.0_4.0   = b_true_answerGPT3.0 - b_true_answerGPT4.0,
    diff_3.0_human = b_true_answerGPT3.0 - b_true_answerHuman,
    diff_3.5_4.0   = b_true_answerGPT3.5 - b_true_answerGPT4.0,
    diff_3.5_human = b_true_answerGPT3.5 - b_true_answerHuman,
    diff_4.0_human = b_true_answerGPT4.0 - b_true_answerHuman
  )


# add intercepts (i.e., human advice rating) to the probit models
intercept_to_text <- function(data){
  data |>
    fmt_APA_numbers() |>
    (\(x) sprintf("$M = %.2f$, HDI = [%.2f, %.2f]", x["estimate"], x["hdi_lower"], x["hdi_upper"]))()
}

m$q.aI <- bayes_coef_intercept(mod.quality$appr, "pnorm") |> intercept_to_text()
m$q.eI <- bayes_coef_intercept(mod.quality$eff, "pnorm") |> intercept_to_text()
m$q.hI <- bayes_coef_intercept(mod.quality$help, "pnorm") |> intercept_to_text()
m$q.sI <- bayes_coef_intercept(mod.quality$sens, "pnorm") |> intercept_to_text()
