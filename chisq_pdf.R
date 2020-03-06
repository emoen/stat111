## 1.a
n <- 10000 ## The number of trials.
freedom = 6
k <- seq(1, n, by = 1) ## First axis of plot.

sample <- rchisq(n = n, df = freedom) ## Second axis of plot for pdf.
hist(sample, main = "sample from chisq dist")

nn = 10000
y = rep(0, 10000)
s=6
# 1. b(ii)
for (i in 1:nn) {
    # 1. b(i)
    x = rnorm(s, mean=0, sd=1)
    y_i = sum(x*x)
    y[i] =y_i
}
hist(y, main="hist of sum(x*x) of 6 norm(0,1) 10 000 times")

plot(x = k, y = sample, type = "s", las = 1, main = "sample from chisq dist")
plot(x = k, y = cdf, type = "s", las = 1, main = "cdf of Binomial distribution")

# 1. c Sammenlign histogrammene til Z og Y . Konklusjon?
# De ser nesten identisk ut.
#################################
## 2.a
