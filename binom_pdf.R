n <- 500 ## The number of trials.
prob <- 0.1 ## The probability of success on each trial.
k <- seq(0, n, by = 1) ## First axis of plot.
mean = n*prob
variance = n*prob*(1-prob)
k = seq(mean-variance, mean+variance, by=1)

pdf <- dbinom(x = k, size = n, prob = prob) ## Second axis of plot for pdf.
cdf <- pbinom(q = k, size = n, prob = prob) ## Second axis of plot for cdf.
## Inspect the pdf and cdf.
plot(x = k, y = pdf, type = "s", las = 1, main = "pdf of Binomial distribution")
plot(x = k, y = cdf, type = "s", las = 1, main = "cdf of Binomial distribution")