if( getOption("project_bayes_run_models") ){
  # Set date/time
  date_time <- ""
  if(  getOption("project_bayes_save_with_date_time") ) date_time <-
      getOption("project_date_time")


  # STUDY 1           =======
  # Model:
  s1[["bayes"]] <- brms::brm(
    sum | trials(length) ~ model * trust + age + gender + education
    + (1|ResponseId) + (1|question),
    s1[["data"]] ,
    backend = "cmdstanr", family = binomial(link = "logit"),
    init = 0, cores = 6, chains = 6, iter = 6000,
  )


  s1_mod <- list()
  s1_mod[["model"]] <- brms::brm(
    sum | trials(length) ~ model + (1|ResponseId) + (1|question),
    data = s1[["data"]],
    backend = "cmdstanr", family = binomial(link = "logit"),
    init = 0, cores = 6, chains = 6, iter = 6000,
  )
  s1_mod[["modelXtrust"]] <- brms::brm(
    sum | trials(length) ~ model * trust + (1|ResponseId) + (1|question),
    data = s1[["data"]],
    backend = "cmdstanr", family = binomial(link = "logit"),
    init = 0, cores = 6, chains = 6, iter = 6000,
  )
  s1_mod[["modelXtrust+occ"]] <- brms::brm(
    sum | trials(length) ~ model * trust + occupation + (1|ResponseId) + (1|question),
    data = s1[["data"]],
    backend = "cmdstanr", family = binomial(link = "logit"),
    init = 0, cores = 6, chains = 6, iter = 6000,
  )
  s1_mod[["modelXtrust+occ+edu"]] <- brms::brm(
    sum | trials(length) ~ model * trust + education + occupation + (1|ResponseId) + (1|question),
    data = s1[["data"]],
    backend = "cmdstanr", family = binomial(link = "logit"),
    init = 0, cores = 6, chains = 6, iter = 6000,
  )
  s1_mod[["modelXtrust+gen"]] <- brms::brm(
    sum | trials(length) ~ model * trust + gender + (1|ResponseId) + (1|question),
    data = s1[["data"]],
    backend = "cmdstanr", family = binomial(link = "logit"),
    init = 0, cores = 6, chains = 6, iter = 6000,
  )
  s1_mod[["modelXtrust+gen+age"]] <- brms::brm(
    sum | trials(length) ~ model * trust + age + gender  + (1|ResponseId) + (1|question),
    data = s1[["data"]],
    backend = "cmdstanr", family = binomial(link = "logit"),
    init = 0, cores = 6, chains = 6, iter = 6000,
  )
  bayes_coef_plot( s1_mod[["modelXtrust+ageXtrust"]] )

  s1_mod[["modelXtrust+ageXtrust"]] <- brms::brm(
    sum | trials(length) ~ model * trust + age * trust + (1|ResponseId) + (1|question),
    data = s1[["data"]],
    backend = "cmdstanr", family = binomial(link = "logit"),
    init = 0, cores = 6, chains = 6, iter = 6000,
  )
  bayes_coef_plot( s1_mod[["modelXtrust+ageXtrust"]] )

  # Diagnostics
  bayes_chain_stab( s1[["bayes"]] )
  bayes_coef_plot(  s1[["bayes"]] )

  # Add criterion
  s1[["bayes"]]  <-  s1[["bayes"]] |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  # Save
  if( getOption("project_bayes_save_to_file") ){
    save( s1 , file = paste0(
      script_relative_path, "data/study_1_data + model", date_time, ".RData")
    )
  }



  # STUDY 2         ======
  #' Here we model the ability to **correctly identify**.
  #' there are here, four categories: 3 AI and 1 Human.
  #' Although the logistic regression makes sense, "correct identification" is a bit more abstract.
  #' I am not entirely sure what that means for the other ancillary variables.
  s2[["bayes"]] <- brms::brm(
    sum | trials(length) ~ true_answer + expertice + age + gender + education +
      (1|ResponseId) + (1|question),
    data = s2[["data"]],
    backend = "cmdstanr", family = binomial(link = "logit"),
    init = 0, cores = 6, chains = 6, iter = 6000,
  )





  bayes_coef_plot( t1)

  # Diagnostics
  bayes_chain_stab(s2[["bayes"]])
  bayes_coef_plot( s2[["bayes"]])
  bayes_tbl_sum(   s2[["bayes"]])

  as.matrix(s2[["bayes"]]) -> test

  coef_hdi_text( test[,"b_expertice"] )
  coef_hdi_text( test[,"b_expertice"] + test[,] )


  # STUDY 3         ======

}