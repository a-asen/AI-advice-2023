
figs[["Identification_of_Sources"]] <-
  marginal_effects(mod.detect)$true_answer |>
  as_tibble() |>
  mutate(effect1__ = fct_relevel(effect1__, "Human")) |>
  ggplot(aes(effect1__, estimate__)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width = .2) +
  geom_hline(yintercept=.5, linetype="dotted", col="red") +
  labs(y = "Probability of identifying advice source", x = "Sources") +
  scale_y_continuous(breaks = seq(0,1,.05), labels = paste0(seq(0,100, 5), " %")) +
  coord_cartesian(ylim = c(.4,1))

condition_save_figure(
  figs[["Identification_of_Sources"]]
  , name = "Identification of AI and Human advice")

