
tbls[["indentification+preference"]] <-
  mod.detect |>
  bayes_tbl_sum(apa_table = T, add_loo_R2 = T, coef_calc = "plogis") |>
  bayes_tbl_add_sig() |>
  rename_with(~paste0("det_", .x), c(-group, -var)) |>
  mutate(var = case_when(
    var   == "true_answerGPT3.0" ~ "GPT3.0"
    , var == "true_answerGPT3.5" ~ "GPT3.5"
    , var == "true_answerGPT4.0" ~ "GPT4.0"
    , var == "true_answerHuman"  ~ "Human"
    , str_starts(var, "Sigma ")  ~ "Sigma (Subject)"
    , T ~ var
  )) |>
  left_join(
    mod.pref |>
      bayes_tbl_sum(apa_table = T, coef_calc = "plogis", add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("pref_", .x), c(-group, -var)) |>
      mutate(var = case_when(
        var   == "modelGPT3.0"      ~ "GPT3.0"
        , var == "modelGPT3.5"      ~ "GPT3.5"
        , var == "modelGPT4.0"      ~ "GPT4.0"
        , str_starts(var, "Sigma ") ~ "Sigma (Subject)"
        , T   ~ var
      ))
    , by = c("group", "var")
  ) |>
  mutate(e = "") |>
  gt(groupname_col = "group") |>
  tab_spanner("Identification study", starts_with("det_")) |>
  tab_spanner("Preference study", starts_with("pref_")) |>
  cols_move(e, det_p) |>
  tab_bayes_generics() |>
  fmt_missing()

conditional_save(
  tbls[["indentification+preference"]]
  , "Mod-Bayes--Identification and preference study summary"
)
