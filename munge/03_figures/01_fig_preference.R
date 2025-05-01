
figs[["Advice_preference"]] <-
  plot(marginal_effects(mod.pref), plot=FALSE)[[1]] +
  labs(y = "Probability of choosing AI", x = "Model") +
  scale_y_continuous(breaks = seq(.3,1,.05), labels = paste0(seq(30,100, 5), " %")) +
  coord_cartesian(ylim = c(.5,1)) +
  geom_hline(yintercept=.5, linetype="dotted", col="red")

condition_save_figure(
  figs[["Advice_preference"]]
  , name = "Preference for AI vs. Human advice")

