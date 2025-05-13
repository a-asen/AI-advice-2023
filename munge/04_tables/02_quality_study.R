
tbls[["advice_quality"]] <-
  bayes_tbl_sum(mod.quality$help, apa_table = T) |>
  bayes_tbl_add_sig() |>
  rename_with(~paste0("help_", .x), c(-group, -var)) |>
  mutate(var = case_when(
    var == "SourceGPT3.5" ~ "GPT3.5"
    , var == "SourceGPT4" ~ "GPT4.0"
    , var == "SourceHuman"  ~ "Human"
    , str_starts(var, "Sigma\\ \\(Res") ~ "Sigma (Subject)"
    , T ~ var
  )) |>
  left_join(
    bayes_tbl_sum(mod.quality$eff , apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("eff_", .x), c(-group, -var)) |>
      mutate(var = case_when(
        var == "SourceGPT3.5" ~ "GPT3.5"
        , var == "SourceGPT4" ~ "GPT4.0"
        , var == "SourceHuman"  ~ "Human"
        , str_starts(var, "Sigma\\ \\(Res") ~ "Sigma (Subject)"
        , T ~ var
      ))
    , by = c("group", "var")
  ) |>
  left_join(
    bayes_tbl_sum(mod.quality$appr , apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("appr_", .x), c(-group, -var)) |>
      mutate(var = case_when(
        var == "SourceGPT3.5" ~ "GPT3.5"
        , var == "SourceGPT4" ~ "GPT4.0"
        , var == "SourceHuman"  ~ "Human"
        , str_starts(var, "Sigma\\ \\(Res") ~ "Sigma (Subject)"
        , T ~ var
      ))
    , by = c("group", "var")
  ) |>
  left_join(
    bayes_tbl_sum(mod.quality$sens , apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("sens_", .x), c(-group, -var)) |>
      mutate(var = case_when(
        var == "SourceGPT3.5" ~ "GPT3.5"
        , var == "SourceGPT4" ~ "GPT4.0"
        , var == "SourceHuman"  ~ "Human"
        , str_starts(var, "Sigma\\ \\(Res") ~ "Sigma (Subject)"
        , T ~ var
      ))
    , by = c("group", "var")
  ) |>
  mutate(e = "") |>
  gt(groupname_col = "group") |>
  tab_spanner("Helpfullness", starts_with("help_")) |>
  tab_spanner("Effectiveness", starts_with("eff_")) |>
  tab_spanner("Appropriateness", starts_with("appr_")) |>
  tab_spanner("Sensitivity", starts_with("sens_")) |>
  tab_bayes_generics(pre_footnote = "
  Leave-one-out (LOO) R² and R² may not be accurate for ordinal modals,
  as they assume the dependent variable to be continuous. ") |>
  fmt_missing() |>
  conditional_save("Mod-Bayes--Quality study summary")
