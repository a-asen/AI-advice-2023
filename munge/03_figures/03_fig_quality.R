  # Data figure   ======
figs[["Quality_of_advice"]] <-
  data[["quality"]] |>
  pivot_longer(c(Helpfulness, Effectiveness, Appropriateness, Sensitivity)) |>
  mutate(Rating = as.numeric(value),
         Source = fct_relevel(Source, "Human")) |>
  ggplot(aes(Source, Rating, col=name)) +
  scale_colour_manual(values = gen_col("brgo")) +
  stat_summary(fun.data = mean_se, position = position_dodge(.4), linewidth = .2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .4, position = position_dodge(.4)) +
  stat_summary(aes(group = name), fun.data = mean_cl_boot, geom = "line", position = position_dodge(.4), alpha = .5) +
  labs(col = "Category", x = "") +
  theme(axis.text.x.bottom = element_text(size = 11))

conditional_save(
  figs[["Quality_of_advice"]]
  , name = "Quality of the Advice"
)


# Model figure      ======
figs[["Quality_of_advice--model"]] <-
  conditional_effects(mod.quality$help)$Source |>
  as_tibble() |>
  bind_rows(
    conditional_effects(mod.quality$eff)$Source |>
      as_tibble()
  ) |>
  bind_rows(
    conditional_effects(mod.quality$appr)$Source |>
      as_tibble()
  ) |>
  bind_rows(
    conditional_effects(mod.quality$sens)$Source |>
      as_tibble()
  ) |>
  mutate(category = case_when(
    Helpfulness     == 1 ~ "Helpfulness",
    Effectiveness   == 1 ~ "Effectiveness",
    Appropriateness == 1 ~ "Appropriateness",
    Sensitivity     == 1 ~ "Sensitivity" )
    , effect1__ = fct_relevel(effect1__, "Human")
  ) |>
  ggplot(aes(effect1__, estimate__, col = category, group = category)) +
  geom_point(position = position_dodge(.4)) +
  geom_line(position = position_dodge(.4), alpha = .5) +
  scale_colour_manual(values=gen_col("brgo")) +
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width = .4,
                 position = position_dodge(.4)) +
  labs(y = "Rating", x = "", col = "Category") +
  theme(axis.text.x.bottom = element_text(size = 11))


conditional_save(
  figs[["Quality_of_advice--model"]]
  , name = "Quality of the Advice--model"
)
