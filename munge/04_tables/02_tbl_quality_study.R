

tbls[["advice_quality"]] <-
  mod.quality$appr |>
  bayes_tbl_sum(apa_table = T, add_loo_R2 = T) |>
  bayes_tbl_add_sig() |>
  rename_with(~paste0("appr_", .x), c(-group, -var)) |>
  mutate(var = case_when(
    var   == "SourceGPT3"   ~ "GPT3.0"
    , var == "SourceGPT3.5" ~ "GPT3.5"
    , var == "SourceGPT4"   ~ "GPT4.0"
    , str_starts(var, "Sigma\\ \\(Res") ~ "Sigma (Subject)"
    , T ~ var
  )) |>
  left_join(
    mod.quality$eff |>
      bayes_tbl_sum(apa_table = T, add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("eff_", .x), c(-group, -var)) |>
      mutate(var = case_when(
        var   == "SourceGPT3"   ~ "GPT3.0"
        , var == "SourceGPT3.5" ~ "GPT3.5"
        , var == "SourceGPT4"   ~ "GPT4.0"
        , str_starts(var, "Sigma\\ \\(Res") ~ "Sigma (Subject)"
        , T ~ var
      ))
    , by = c("group", "var")
  ) |>
  left_join(
    mod.quality$help |>
      bayes_tbl_sum(apa_table = T, add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("help_", .x), c(-group, -var)) |>
      mutate(var = case_when(
        var   == "SourceGPT3"   ~ "GPT3.0"
        , var == "SourceGPT3.5" ~ "GPT3.5"
        , var == "SourceGPT4"   ~ "GPT4.0"
        , str_starts(var, "Sigma\\ \\(Res") ~ "Sigma (Subject)"
        , T ~ var
      ))
    , by = c("group", "var")
  ) |>
  left_join(
    mod.quality$sens |>
      bayes_tbl_sum(apa_table = T, add_loo_R2 = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("sens_", .x), c(-group, -var)) |>
      mutate(var = case_when(
        var   == "SourceGPT3"   ~ "GPT3.0"
        , var == "SourceGPT3.5" ~ "GPT3.5"
        , var == "SourceGPT4"   ~ "GPT4.0"
        , str_starts(var, "Sigma\\ \\(Res") ~ "Sigma (Subject)"
        , T ~ var
      ))
    , by = c("group", "var")
  ) |>
  mutate(e = "") |>
  gt(groupname_col = "group") |>
  tab_spanner("Appropriateness", starts_with("appr_")) |>
  tab_spanner("Effectiveness", starts_with("eff_")) |>
  tab_spanner("Helpfulness", starts_with("help_")) |>
  tab_spanner("Sensitivity", starts_with("sens_")) |>
  tab_bayes_generics(pre_footnote = "
  Leave-one-out (LOO) R² and R² may not be accurate for ordinal modals,
  as they assume the dependent variable to be continuous. ") |>
  fmt_missing()

tbls[["advice_quality"]] |>
  conditional_save("Mod-Bayes--Quality study summary")
