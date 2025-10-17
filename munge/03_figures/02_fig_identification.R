figs[["Identification_of_Sources"]] <-
  conditional_effects(mod.detect)$true_answer |>
  as_tibble() |>
  ggplot(aes(effect1__, estimate__)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width = .2) +
  geom_hline(yintercept=.5, linetype="dotted", col="red") +
  labs(y = "Identification of Source", x = "") +
  scale_y_continuous(breaks = seq(0,1,.05), labels = paste0(seq(0,100, 5), " %")) +
  coord_cartesian(ylim = c(.4,1)) +
  theme(axis.text.x.bottom = element_text(size = 10))

conditional_save(
  figs[["Identification_of_Sources"]]
  , name = "Identification of AI and Human advice"
)
