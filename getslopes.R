slopes <- numeric()

for (i in 1:150) {
  years <- seq(1, 125)
  fit <- lm((NormalizedTopicFreqs[i, ] * 100) ~ years)
  slopes <- c(slopes, fit$coefficients[2])
}

# Run interactively:
# max(slopes)
# min(slopes)
# mean(slopes)
# median(slopes)
# sum(slopes > 0)
# sum(slopes < 0)