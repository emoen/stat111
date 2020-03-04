mu = 50 ## The mean
sd = 15## The standard deviation
L <- 91 ## The number of points to use in the plotting-vector.
lower_quantile <- 0.001 ## Where to start.
upper_quantile <- 0.999 ## Where to end.
x_lower <- qnorm(p = lower_quantile, mean = mu, sd = sd) ## Lower end of our plot
x_upper <- qnorm(p = upper_quantile, mean = mu, sd = sd) ## Upper end of our plot
x <- seq(from = x_lower, to = x_upper, length.out = L) ## First axis of plot.
pdf <- dnorm(x = x, mean = mu, sd = sd) ## Second axis of plot for pdf.
cdf <- pnorm(q = x, mean = mu, sd = sd) ## Second axis of plot for cdf.
## Inspect the pdf and cdf.
plot(x = x, y = pdf, type = "l", las = 1, main = "pdf of Normal distribution")
plot(x = x, y = cdf, type = "l", las = 1, main = "cdf of Normal distribution")