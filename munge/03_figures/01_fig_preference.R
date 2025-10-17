figs[["Advice_preference"]] <-
  conditional_effects(mod.pref)$model |>
  as_tibble() |>
  ggplot(aes(effect1__, estimate__)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width = .2) +
  geom_hline(yintercept=.5, linetype="dotted", col="red") +
  labs(y = "Preferance Over Human Answer", x = "") +
  scale_y_continuous(breaks = seq(.3,1,.05), labels = paste0(seq(30,100, 5), " %")) +
  coord_cartesian(ylim = c(.5,1)) +
  theme(axis.text.x.bottom = element_text(size = 10))

conditional_save(
  figs[["Advice_preference"]]
  , name = "Preference for AI vs. Human advice"
)


