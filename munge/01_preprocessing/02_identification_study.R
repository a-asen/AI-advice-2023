#* Identify advice as AI vs. Human


## Count loss       =====
loss[["detect"]] <- list()
# Start loss count:
loss[["detect"]][["start_n"]] <-  nrow( raw_data[["detect"]] )

### Duplicated Ids      ======
# Remove duplicated ResponseId
pre_data[["detect"]]  <- raw_data[["detect"]]  |>
  filter(!duplicated(ResponseId))
# Count loss
loss[["detect"]][["duplicated_ids"]] <- nrow( pre_data[["detect"]] )

### Non-consent     ======
# Exclude non-consent:
pre_data[["detect"]] <- pre_data[["detect"]] |>
  mutate(ConsT = if_else(str_detect(Consent, "I agree to participate"), T, F))
# Count loss
loss[["detect"]][["non_consent"]] <- pre_data[["detect"]] |> filter(!ConsT) |> nrow()
# Filter data
pre_data[["detect"]] <- pre_data[["detect"]] |> filter(ConsT)

### Non-responses      =====
# Summarise preference across participant and filter those who gave no answer
pre_data[["detect"]] <- pre_data[["detect"]]  |>
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
loss[["detect"]][["non_responders"]] <-
  pre_data[["detect"]] |> filter(is.na(overall)) |> nrow()

# Filter
pre_data[["detect"]] <-
  pre_data[["detect"]] |> filter(!is.na(overall))


## Prepare data      ======
data[["detect"]] <-
  pre_data[["detect"]] |>
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
    ),
    Country = case_when(
      Country == "Other (Specify below)" ~ Country_9_TEXT,
      T ~ Country )
  ) |>
  reframe(
    .by = c(ResponseId, true_answer),
    gender      = unique(gender),
    question    = unique(question_num),
    sum         = sum(value),
    length      = length(value),
    expertice   = unique(Expertice),
    age         = unique(Age),
    education   = unique(Education),
    occupation  = unique(Occupation),
    country     = unique(Country),
  )

