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

#################################
##4.a 
maxi = 20
sigma = 1
sub_sample = rnorm(maxi, mean=0, sd=sigma)
sample_mean = mean(sub_sample)
standard_error = sqrt(sum((sub_sample-sample_mean)^2)) / sqrt(maxi)
#kritiske verdier for chi sq dist for 95% konf

# 95% conf:
alpha = 1-0.95
two_point_five_left=qchisq(alpha/2, df=(maxi-1))
ninty_seven_point_five_left=qchisq(1-(alpha/2), df=(maxi-1))
#(8.906516, 32.85233)

#estimage of sigma^2
sigma_hat_low = ((maxi-1)*standard_error^2)/ninty_seven_point_five_left
sigma_hat_high = ((maxi-1)*standard_error^2)/two_point_five_left

sqrt(sigma_hat_low)
sqrt(sigma_hat_high)
# Answere: using standard error: (0.7402341, 1.421668) is 95% CI for sigma
# Note: (0.7604904, 1.460572) - is estimate of using sigma=1 as in place of standard error


#4.b
B=999
all_mean_hat=rep(0,B)
for (i in 1:B) {
    boot_sample = sample(sub_sample, replace = TRUE)
    boot_mean = mean(boot_sample)
    all_mean_hat[i] = boot_mean
}
#Formula on 8.5 page 413
all_boot_mean = mean(all_mean_hat)
s2_boot = 1/(maxi-1)*sum((all_mean_hat-all_boot_mean)^2)
s_boot = sqrt(s2_boot)
s_boot
all_boot_mean
#s_boot is estaimated at: 1.367971, or 1.373108, or...

#confidence interval
z=1.96 # 95% n(0,1)
lower = all_boot_mean - (z*s_boot)
upper = all_boot_mean + (z*s_boot)
lower
upper
# (-2.423707,2.684063) is a 95% CI

# extras
bias = abs(mean(sub_sample)-(mean(all_mean_hat))
