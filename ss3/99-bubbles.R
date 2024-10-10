d <- readRDS("~/Downloads/SS3_N_A0.rds")

library(ggplot2)
library(dplyr)

d |>
  ggplot(aes(Yr, variable, size = value, colour = value)) +
  geom_point(pch = 21) +
  geom_point(pch = 19, alpha = 0.07) +
  geom_abline(intercept = seq(-500, 0, 1), slope = 0.1, colour = "grey60", lty = 2) +
  theme(axis.title.x = element_blank()) +
  ylab("Age") +
  facet_wrap(~Sex) +
  scale_size_area(max_size = 12) +
  guides(colour = "none", size = "none") +
  coord_cartesian(expand = FALSE, xlim = range(d$Yr), ylim = c(0.8, 6.2)) +
  scale_x_continuous(breaks = seq(1940, 2020, 10))
ggsave("figs/bubble.png", width = 9, height = 3.5)
