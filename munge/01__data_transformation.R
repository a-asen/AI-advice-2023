# STUDY 1           ======
##  Count loss        =====
s1[["loss"]] <- list()
# Start loss count:
s1[["loss"]][["start_n"]] <-  nrow(s1[["raw_data"]])

### Duplicated ids    =====
# Remove duplicated ResponseId
s1[["pre_data"]] <- s1[["raw_data"]] |>
  filter(!duplicated(ResponseId))
# Count loss:
s1[["loss"]][["duplicated_ids"]] <- nrow( s1[["pre_data"]] )

### Non-consent       =====
# Exclude non-consent:
s1[["pre_data"]] <- s1[["pre_data"]] |>
  mutate(ConsT = if_else(str_detect(Consent, "I agree to participate"), T, F))
# Count loss:
s1[["loss"]][["non_consent"]] <- s1[["pre_data"]] |> filter(!ConsT) |> nrow()
# Filter data
s1[["pre_data"]] <- s1[["pre_data"]] |> filter(ConsT)

### Non-responses       ======
# Summarise preference across participant and filter those who gave no answer
s1[["pre_data"]] <-
  s1[["pre_data"]] |>
  mutate(
    .before = 1,
    across( starts_with("GPT"), \(x){
      case_when(
        str_starts(x, "Human") ~ 0,
        str_starts(x, "AI")    ~ 1,
        T ~ NA)} )
  ) |>
  rowwise() |>
  mutate(
    .before    = 1,
    overall    = mean(c_across(starts_with("GPT")), na.rm = T),
    sum_gpt4   = mean(c_across(starts_with("GPT4")), na.rm = T),
    sum_gpt3.5 = mean(c_across(starts_with("GPT3.5")), na.rm = T),
    sun_gpt3   = mean(c_across(starts_with("GPT3_")), na.rm = T),
  )

# Count loss:
s1[["loss"]][["non_responders"]] <-
  s1[["pre_data"]] |> filter( is.na(overall) ) |> nrow()
# Filter
s1[["pre_data"]] <- s1[["pre_data"]] |> filter(!is.na(overall))


## Prepare data       ======
s1[["data"]] <-
  s1[["pre_data"]] |>
  pivot_longer(starts_with("GPT")) |>
  filter(!is.na(value)) |>
  mutate(
    model = case_when(
      str_detect(name, "GPT3_")  ~ "GPT3.0",
      str_detect(name, "GPT3.5") ~ "GPT3.5",
      str_detect(name, "GPT4")   ~ "GPT4.0",
    ),
    question_num = str_extract(name, "Q\\d+") |> str_replace("Q", ""),
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
      Education == "High-School"    ~ 1,
      Education == "Bachelor's degree" ~ 2,
      Education == "Other (Specify below)" ~ 2, # (single case)
      #' These two are about equivalent
      Education == "Master's degree" ~ 3,
      Education == "Professional degree" ~ 3,
      #' Professional/Masters is equivalent ?
      Education == "Doctorate degree" ~ 4,
    ),
    Occupation = case_when(
      Current_state == "Working"           ~ "Working",
      Current_state == "Studying"          ~ "Studying",
      str_starts(Current_state, "Neither") ~ "Neither",
    )
  ) |>
  reframe(
    .by = c(ResponseId, model),
    question   = unique(question_num),
    gender     = unique(Gender),
    trust      = unique(Trust),
    age        = unique(Age),
    education  = unique(Education),
    occupation = unique(Occupation),
    sum        = sum(value),
    length     = length(value)
  )



# STUDY 2           ======
## Count loss       =====
s2[["loss"]] <- list()
# Start loss count:
s2[["loss"]][["start_n"]] <-  nrow( s2[["raw_data"]] )

### Duplicated Ids      ======
# Remove duplicated ResponseId
s2[["pre_data"]]  <- s2[["raw_data"]]  |>
  filter(!duplicated(ResponseId))
# Count loss
s2[["loss"]][["duplicated_ids"]] <- nrow( s2[["pre_data"]] )

### Non-consent     ======
# Exclude non-consent:
s2[["pre_data"]] <- s2[["pre_data"]] |>
  mutate(ConsT = if_else(str_detect(Consent, "I agree to participate"), T, F))
# Count loss
s2[["loss"]][["non_consent"]] <- s2[["pre_data"]] |> filter(!ConsT) |> nrow()
# Filter data
s2[["pre_data"]] <- s2[["pre_data"]] |> filter(ConsT)

### Non-responses      =====
# Summarise preference across participant and filter those who gave no answer
s2[["pre_data"]] <- s2[["pre_data"]]  |>
  mutate(
    .before = 1,
    across( starts_with("GPT"), \(x){ case_when(
      str_starts(x, "Human") ~ 0,
      str_starts(x, "AI")    ~ 1,
      T ~ NA)
    }),
    across( starts_with("Human"), \(x){ case_when(
      str_starts(x, "AI")     ~ 0,
      str_starts(x, "Human")  ~ 1,
      T ~ NA)
    }),
  ) |>
  rowwise() |>
  mutate(
    .before    = 1,
    overall    = mean(c_across(c(starts_with("Human_"), starts_with("GPT"))), na.rm = T),
    sum_gpt4   = mean(c_across(starts_with("GPT4")), na.rm = T),
    sum_gpt3.5 = mean(c_across(starts_with("GPT3.5")), na.rm = T),
    sun_gpt3   = mean(c_across(starts_with("GPT3_")), na.rm = T),
    sum_human  = mean(c_across(starts_with("Human_")), na.rm = T)
  )

# Count loss
s2[["loss"]][["non_responders"]] <-
  s2[["pre_data"]] |> filter(is.na(overall)) |> nrow()

# Filter
s2[["pre_data"]] <-
  s2[["pre_data"]] |> filter(!is.na(overall))


## Prepare data      ======
s2[["data"]] <-
  s2[["pre_data"]] |>
  pivot_longer(c(starts_with("Human_"), starts_with("GPT"))) |>
  filter(!is.na(value)) |>
  mutate(
    true_answer = case_when(
      str_detect(name, "GPT3_")  ~ "GPT3.0",
      str_detect(name, "GPT3.5") ~ "GPT3.5",
      str_detect(name, "GPT4")   ~ "GPT4.0",
      str_detect(name, "Human_") ~ "Human",
    ),
    question_num = str_extract(name, "Q\\d+") |> str_replace("Q", ""),
    Gender = if_else(is.na(gender), "Other", gender),
    Expertice  = case_when(
      str_starts(`AI expertice`, "Advanced")  ~ 3,
      str_starts(`AI expertice`, "Advanced")  ~ 3,
      str_starts(`AI expertice`, "Regular")   ~ 2,
      str_starts(`AI expertice`, "Casual")    ~ 1,
      str_starts(`AI expertice`, "Novice")    ~ 1,
      str_starts(`AI expertice`, "None")      ~ 0
    ),
    Age = case_when(
      age == "18 - 24" ~ 0,
      age == "25 - 34" ~ 1,
      age == "35 - 44" ~ 2,
      age == "45 - 54" ~ 3,
      age == "65 - 74" ~ 4,
    ),
    Education = case_when(
      education == "Primary-School" ~ 0,
      education == "High-School" ~ 1,
      education == "Other (Specify below)" ~ 2, # (single case)
        # "Vocation school" similar to high school ?
      education == "Bachelor's degree" ~ 2,
      education == "Master's degree" ~ 3,
      education == "Professional degree" ~ 3,
      #' Professional/Masters is equivalent ?
      education == "Doctorate degree" ~ 4,
    ),
    Occupation = case_when(
      str_starts(Current_state, "Working") ~ "Working",
      str_starts(Current_state, "Student") ~ "Student",
      str_starts(Current_state, "None,") ~ "Other",
    )
  ) |>
  reframe(
    .by = c(ResponseId, true_answer),
    gender      = unique(gender),
    question    = unique(question_num),
    expertice   = unique(Expertice),
    age         = unique(Age),
    education   = unique(Education),
    occupation  = unique(Occupation),
    sum         = sum(value),
    length      = length(value)
  )

