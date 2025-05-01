#* Evaluate the quality of the advice, human and AI.


##  Count loss        =====
loss[["quality"]] <- list()
# Start loss count:
loss[["quality"]][["start_n"]] <-  nrow( raw_data[["quality"]])

### Duplicated ids    =====
# Remove duplicated ResponseId
pre_data[["quality"]] <- raw_data[["quality"]] |>
  filter(!duplicated(ResponseId))
# Count loss:
loss[["quality"]][["duplicated_ids"]] <- nrow( pre_data[["quality"]] )

### Non-consent       =====
# Exclude non-consent:
pre_data[["quality"]] <- pre_data[["quality"]] |>
  mutate(ConsT = if_else(str_detect(Consent, "I agree to participate"), T, F))
# Count loss:
loss[["quality"]][["non_consent"]] <- pre_data[["quality"]] |> filter(!ConsT) |> nrow()
# Filter data
pre_data[["quality"]] <- pre_data[["quality"]] |> filter(ConsT)

### Non-responses       ======
# Summarise preference across participant and filter those who gave no answer
data[["quality"]] <-
  pre_data[["quality"]] |>
  pivot_longer(c(starts_with("GPT"), starts_with("Human"))) |>
  separate_wider_delim(name, "_", names_sep = "_", names = c("Source", "Question", "Number")) |>
  filter(!is.na(value)) |>
  mutate(
    name_Number = case_when(
      name_Number == 1 ~ "Helpfulness",
      name_Number == 2 ~ "Effectiveness",
      name_Number == 3 ~ "Appropriateness",
      name_Number == 4 ~ "Sensitivity")
    , Gender = if_else( is.na(Gender), "Other", Gender)
    , Age = case_when(
      Age == "18 - 24" ~ 0,
      Age == "25 - 34" ~ 1,
      Age == "35 - 44" ~ 2,
      Age == "45 - 54" ~ 3,
      Age == "65 - 74" ~ 4)
    , Education = case_when(
      Education == "Primary-School" ~ 0
      , Education == "High-School" ~ 1
      # "Vocation school" similar to high school ?
      , Education == "Bachelor's degree" ~ 2
      , Education == "Master's degree" ~ 3
      , Education == "Professional degree" ~ 3
      #' Professional/Masters is equivalent ?
      , Education == "Doctorate degree" ~ 4
      , T ~ NA)
    , Occupation = case_when(
      str_starts(Current_state, "Working") ~ "Working"
      , str_starts(Current_state, "Student") ~ "Student"
      , str_starts(Current_state, "None,") ~ "Other" )
    , Country = case_when(
      Country == "Other (Specify below)" ~ Country_9_TEXT
      , T ~ Country )
    , value = as.numeric(value) |> ordered()
  ) |>
  pivot_wider(names_from=name_Number, values_from=value) |>
  select(ResponseId, Gender, Age, Education, Occupation, Country,
         name_Source, name_Question, Helpfulness, Effectiveness, Appropriateness, Sensitivity) |>
  rename(
    Source = name_Source, Question = name_Question
  )


