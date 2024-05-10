# Heatmap image for hex
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-08-28

library(ggplot2)

x <- rep(1:20, times = 20)
y <- rep(1:20, each = 20)
df <- data.frame(x = x, y = y)

df$z <- sqrt((df$x - 6)^2 + (df$y - 6)^2)
df$z <- df$z + rnorm(n = nrow(df))
ggplot(data = df, mapping = aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1)
