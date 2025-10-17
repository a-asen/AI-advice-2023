library(ProjectTemplate)
relative_path=""
load.project()

library(brms)
library(BayesFactor)
library(bayesplot)
library(gt)

#                   PARAMETERS                    ======
script_run_bayesian_models <- FALSE
#' **TRUE** will **RUN** the Bayesian models.
#' Depending on your computer, this might take some time.
#' **FALSE** will **NOT RUN** any Bayesian models, but will load them
#' from "export/paper_vars.RData".

script_save_bayesian_model <- TRUE
#' **TRUE** will SAVE the generated Bayesian models
#' *only if the "script_run_bayesian_models" is set to* **TRUE**.
#' **FALSE** will NOT SAVE the generated Bayesian models.¤

script_save_with_date_time <- TRUE
#' **TRUE** will save all generated output with a date and time.
#' This way you will not append previously generated data
#' **FALSE** will not save the generated output with a date and time.
#' *CAUSTION* This feature might append previously generated data/tables/pictures

if(script_save_with_date_time){
  toggle_date_time <- format( Sys.time(), "_%Y-%m-%d_%H-%M-%S_")
} else {
  toggle_date_time <- NULL
}


script_save_figures <- TRUE
#' **TRUE** will save figure
#' **FALSE** will *NOT* save figures

script_save_tables <- TRUE
#' **TRUE** will save tables
#' **FALSE** will *NOT* save tables


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Overview:
#' Study 1: Advice preference (Human vs. AI)
#' Study 2: AI detection
#' Study 3: AI model preference

# Study 1         ======
# prepare data
loss <- list()
loss[["raw_read"]] <- nrow(data_study1)

# Remove duplicated ResponseId
data_study1 <-
  data_study1 |>
  filter(!duplicated(ResponseId))
loss[["duplicated_ids"]] <- nrow(data_study1)

#' Filter non-consent people
data_study1_flt <-
  data_study1 |>
  mutate(ConsT = if_else(str_detect(Consent, "I agree to participate"), T, F))
loss[["non_consent"]] <- data_study1_flt |> filter(!ConsT)
data_study1_flt <- data_study1_flt |> filter(ConsT)

#' Summarise across participant and filter those who gave no answer
data_study1_flt <-
  data_study1_flt |>
  mutate(
    .before = 1,
    across( starts_with("GPT"), \(x){
      if_else( str_detect(x, "Human"), 0, if_else(str_detect(x, "AI"), 1, NA))
    }),
  ) |>
  rowwise() |>
  mutate(
    .before    = 1,
    overall    = mean(c_across(starts_with("GPT")), na.rm = T),
    sum_gpt4   = mean(c_across(starts_with("GPT4")), na.rm = T),
    sum_gpt3.5 = mean(c_across(starts_with("GPT3.5")), na.rm = T),
    sun_gpt3   = mean(c_across(starts_with("GPT3_")), na.rm = T),
  )

# Count loss
loss[["Non_responders"]] <- data_study1_flt |> filter(is.na(overall))
data_study1_flt <- data_study1_flt |> filter(!is.na(overall))


## Analysis        =======

# Distribution across evaluations.
data_study1_flt |>
  ggplot(aes(overall)) +
  geom_point(aes(y=0), position = position_jitter(,.1)) +
  geom_density(outline.type = "both") +
  geom_boxplot(aes(y=.5))


### Logistical regression model     ======
# Prepare the data
logic_data <-
  data_study1 |>
  pivot_longer(starts_with("GPT")) |>
  filter(!is.na(value)) |>
  mutate(
    value = if_else(str_detect(value, "Human"), 0, 1),
    model = case_when(
      str_detect(name, "GPT3_") ~ "GPT3.0",
      str_detect(name, "GPT3.5") ~ "GPT3.5",
      str_detect(name, "GPT4") ~ "GPT4.0",
    ),
    Gender = if_else(is.na(Gender), "Other", Gender),
    Trust  = case_when(
      Trust == "Very trustworthy" ~ 5,
      Trust == "Reasonably trustworthy" ~ 4,
      Trust == "Somewhat trustworthy" ~ 3,
      Trust == "Slightly trustworthy" ~ 2,
      Trust == "Neutral / Unsure" ~ 1,
      Trust == "No" ~ 0,
    ),
    Age = case_when(
      Age == "18 - 24" ~ 0,
      Age == "25 - 34" ~ 1,
      Age == "35 - 44" ~ 2,
      Age == "45 - 54" ~ 3,
      Age == "65 - 74" ~ 4,
    ),
    Education = case_when(
      Education == "Primary-School" ~ 0,
      Education == "High-School" ~ 1,
      Education == "Bachelor's degree" ~ 2,
      Education == "Other (Specify below)" ~ 2,
        #' These two are about equivalent
      Education == "Master's degree" ~ 3,
      Education == "Professional degree" ~ 3,
        #' Professional/Masters is equivalent ?
      Education == "Doctorate degree" ~ 4,
    )
  ) |>
  reframe(
    .by = c(ResponseId, model),
    gender = unique(Gender),
    trust = unique(Trust),
    age = unique(Age),
    education = unique(Education),
    sum = sum(value),
    length = length(value)
  )

### Models =====
if(script_run_bayesian_models){
  human_vs_ai_preference <- list()

  #### Absolute base model        =====
  human_vs_ai_preference[["m_R"]] <- brms::brm(
    sum | trials(length) ~ model + (1|ResponseId),
    logic_data,
    family = binomial(link = "logit"), init=0, cores = 6, chains=6, iter = 6000,
    backend = "cmdstanr"
  )
  # Add criterion
  human_vs_ai_preference[["m_R"]] <-
    human_vs_ai_preference[["m_R"]] |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))


  #### Model + age+gender     =====
  human_vs_ai_preference[["m_a_g_R"]] <- brms::brm(
    sum | trials(length) ~ model + age + gender + (1|ResponseId),
    logic_data,
    family = binomial(link = "logit"), init=0, cores = 6, chains=6, iter = 6000,
    backend = "cmdstanr"
  )
  # Add criterion
  human_vs_ai_preference[["m_a_g_R"]] <-
    human_vs_ai_preference[["m_a_g_R"]] |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  ##### COMPARE   =====
  loo_compare(human_vs_ai_preference[["m_R"]], human_vs_ai_preference[["m_a_g_R"]])
  #' Simple model prefered.



  #### Model + trust        ======
  # remove cases in which trust is missing (to compare)
  human_vs_ai_preference[["m_R__rm"]] <- brms::brm(
    sum | trials(length) ~ model + (1|ResponseId),
    logic_data |> filter(!(is.na(trust))),
    family = binomial(link = "logit"), init=0, cores = 6, chains=6, iter = 6000,
    backend = "cmdstanr"
  )
  # Add criterion
  human_vs_ai_preference[["m_R__rm"]] <-
    human_vs_ai_preference[["m_R__rm"]] |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  # trust model
  human_vs_ai_preference[["m_t_R__rm"]] <- brms::brm(
    sum | trials(length) ~ model + trust + (1|ResponseId),
    logic_data |> filter(!(is.na(trust))),
    family = binomial(link = "logit"), init=0, cores = 6, chains=6, iter = 6000,
    backend = "cmdstanr"
  )
  # Add criterion
  human_vs_ai_preference[["m_t_R__rm"]] <-
    human_vs_ai_preference[["m_t_R__rm"]] |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  ##### COMPARE     =====
  loo_compare(human_vs_ai_preference[["m_R__rm"]], human_vs_ai_preference[["m_t_R__rm"]])
  #' Simple model prefered.


  #### Model + education      ======
  #' Model + education
  human_vs_ai_preference[["m_e_R"]] <- brms::brm(
    sum | trials(length) ~ model + education + (1|ResponseId),
    logic_data,
    family = binomial(link = "logit"), init=0, cores = 6, chains=6, iter = 6000,
    backend = "cmdstanr"
  )
  # Add criterion
  human_vs_ai_preference[["m_e_R"]] <-
    human_vs_ai_preference[["m_e_R"]] |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))

  human_vs_ai_preference[["m_e_R__rm"]] <- brms::brm(
    sum | trials(length) ~ model + education + (1|ResponseId),
    logic_data |> filter(!is.na(trust)),
    family = binomial(link = "logit"), init=0, cores = 6, chains=6, iter = 6000,
    backend = "cmdstanr"
  )
  # Add criterion
  human_vs_ai_preference[["m_e_R__rm"]] <-
    human_vs_ai_preference[["m_e_R__rm"]] |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  ##### Compare ======
  loo_compare(human_vs_ai_preference[["m_R"]], human_vs_ai_preference[["m_e_R"]])


  #### Full         =====
  #' Model + age + gender + trust + education
  human_vs_ai_preference[["m_a_g_t_e_R__rm"]] <- brms::brm(
    sum | trials(length) ~ model + age + gender + trust + education + (1|ResponseId),
    logic_data |> filter(!(is.na(trust))),
    family = binomial(link = "logit"), init=0, cores = 6, chains=6, iter = 6000,
    backend = "cmdstanr"
  )
  # Add criterion
  human_vs_ai_preference[["m_a_g_t_e_R__rm"]] <-
    human_vs_ai_preference[["m_a_g_t_e_R__rm"]] |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  ##### Compare ======
  loo_compare(human_vs_ai_preference[["m_R__rm"]], human_vs_ai_preference[["m_a_g_t_e_R__rm"]])
  #' Not really better than the base model, but yet the more complex model
  #' suggests that the variable trust is a significant predictor. Moreover,
  #' the GPT3 (and the other models) are no longer preferred over human
  #' answers. Rather, peoples trust in LLMs predict their preference for AI
  #' answers.

  if(script_save_bayesian_model){
    save(human_vs_ai_preference,
         file = paste0("data/export/bayes_models", toggle_date_time,".Rdata"))
  }
} else {
  load("data/export/bayes_models_2024-12-23_10-47-43_.Rdata")
}

# Raw change in preference for AI (above human) over trust levels
logic_data |>
  ggplot(aes(trust, sum)) +
  stat_summary() +
  geom_smooth(method="lm") +
  stat_summary(geom="line")


## Visualization   ====
### Model comparison    ======
# The probability of choosing the AI answer above human answers.
human_vs_ai_preference[["m_a_g_t_e_R"]] |>
  as_tibble() |>
  mutate(
    .before=1,
    GPT3.0 = exp(b_Intercept) / ( 1 + exp(b_Intercept)),
    GPT3.5 = b_Intercept + b_modelGPT3.5,
    GPT3.5 = exp(GPT3.5) / ( 1 + exp(GPT3.5)),
    GPT4.0 = b_Intercept + b_modelGPT4.0,
    GPT4.0 = exp(GPT4.0) / ( 1 + exp(GPT4.0)),
  ) |>
  pivot_longer(c(GPT3.0, GPT3.5, GPT4.0)) |>
  ggplot(aes(name, value)) +
  stat_summary(fun.data = mean_se) +
  labs(y="Predicted Probability of choosing AI", x = "") +
  scale_y_continuous(breaks = seq(.2,1,.025),  labels = seq(20,100,2.5) |> paste0(" %")) +
  theme_bw()


### Trust level         ======
# The effec tof
human_vs_ai_preference[["m_a_g_t_e_R"]] |>
  as_tibble() |>
  mutate( trust_val = list(seq(0, 5, 1)) ) |>
  unnest(trust_val) |>
  mutate(pred = exp(b_Intercept + b_trust*trust_val) /
           ( 1 + exp(b_Intercept + b_trust*trust_val)) ) |>
  summarise(
    .by = trust_val,
    intr = mean(b_Intercept),
    m = mean(pred),
    q_l = quantile(pred, prob=.025),
    q_h = quantile(pred, prob=.975)
  ) |>
  ggplot(aes(trust_val, m)) +
  geom_smooth() +
  geom_ribbon(aes(ymin = q_l, ymax = q_h), alpha=0.2) +
  labs(y="Predicted Probability of choosing AI", x = "Trust level") +
  scale_y_continuous(breaks = seq(.2,1,.1),  labels = seq(20,100,10) |> paste0(" %"))




human_vs_ai_preference[["m_R"]] |>
  as_tibble() |>
  pivot_longer(c(b_Intercept, b_modelGPT3.5, b_modelGPT4.0)) |>
  mutate(pred = exp(value) / ( 1 + exp(value)) ) |>
  ggplot(aes(name, pred)) +
  stat_summary(fun.data = mean_cl_boot)




