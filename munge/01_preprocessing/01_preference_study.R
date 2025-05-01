##  Count loss        =====
# Start loss count:
loss[["pref"]][["start_n"]] <-  nrow( raw_data[["pref"]] )

### Duplicated ids    =====
# Remove duplicated ResponseId
pre_data[["pref"]] <- raw_data[["pref"]] |>
  filter(!duplicated(ResponseId))
# Count loss:
loss[["pref"]][["duplicated_ids"]] <- nrow( pre_data[["pref"]] )

### Non-consent       =====
# Exclude non-consent:
pre_data[["pref"]] <- pre_data[["pref"]] |>
  mutate(ConsT = if_else(str_detect(Consent, "I agree to participate"), T, F))
# Count loss:
loss[["pre_data"]][["non_consent"]] <- pre_data[["pref"]] |> filter(!ConsT) |> nrow()
# Filter data
pre_data[["pref"]] <- pre_data[["pref"]] |> filter(ConsT)

### Non-responses       ======
# Summarise preference across participant and filter those who gave no answer
pre_data[["pref"]] <-
  pre_data[["pref"]] |>
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
loss[["pref"]][["non_responders"]] <-
  pre_data[["pref"]] |> filter( is.na(overall) ) |> nrow()
# Filter
pre_data[["pref"]] <- pre_data[["pref"]] |> filter(!is.na(overall))


## Prepare data       ======
data[["pref"]] <-
  pre_data[["pref"]] |>
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
    ),
    Country = case_when(
      Country == "Other (Specify below)" ~ Country_9_TEXT,
      T ~ Country )
  ) |>
  reframe(
    .by = c(ResponseId, model),
    question   = unique(question_num),
    sum        = sum(value),
    length     = length(value),
    trust      = unique(Trust),
    gender     = unique(Gender),
    age        = unique(Age),
    education  = unique(Education),
    occupation = unique(Occupation),
    country    = unique(Country)
  )



