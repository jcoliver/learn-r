# Plot Anscombe's quartets with and without points
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-07-10

library(ggplot2)

aq <- read.csv(file = "data/anscombes-quartet.csv")

# To ensure identical axes, will explicitly set x & y limits for *both* plots

# Draw four plots, sans points
lines_only <- ggplot(data = aq,
                     mapping = aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ dataset, nrow = 2, ncol = 2) +
  xlim(c(min(aq$x), max(aq$x))) +
  ylim(c(min(aq$y), max(aq$y))) +
  theme_bw()
lines_only
ggsave(filename = "output/anscombes-lines.png",
       plot = lines_only)

# Draw same four plots, this time adding points
lines_points <- ggplot(data = aq,
          mapping = aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(mapping = aes(color = as.factor(dataset))) +
  facet_wrap(~ dataset, nrow = 2, ncol = 2) +
  xlim(c(min(aq$x), max(aq$x))) +
  ylim(c(min(aq$y), max(aq$y))) +
  theme_bw() +
  theme(legend.position = "none")
lines_points
ggsave(filename = "output/anscombes-lines-points.png",
       plot = lines_points)
