## 1.a
n <- 10000 ## The number of trials.
freedom = 6
k <- seq(1, n, by = 1) ## First axis of plot.

sample <- rchisq(n = n, df = freedom) ## Second axis of plot for pdf.
hist(sample, main = "sample from chisq dist")

nn = 10000
y = rep(0, nn)
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
# -De ser nesten identisk ut.
#################################
## 2.a
n = 1000
sample = rlnorm(n, meanlog = 0, sdlog = 1)
hist(sample, main = "sample from lognorm dist")

#2.b 
hist(log(sample), main="sampe from ln(Z)")
#2.c Sammenlign histogrammene til Z og Y . Konklusjon. 
# -Y ser ut som n(0,1) så Z= ln(Y) - lognorm fordeling.

#################################
##3.a 
n=1000
maxi = 5
delta=1
y = rep(0,n)
# (ii)
for (j in 1:n) {
    #(i)
    sub_sample = runif(maxi, min = 0, max = delta)
    y[j] = max(sub_sample)
}
hist(y, main="max of 5 samples from U(1)")
#3.b Sammenlign histogrammet til Y med den verdien du har valgt for θ. Konklusjon?
# - de fleste verdiene ligger nærmt delta. Som forventet.