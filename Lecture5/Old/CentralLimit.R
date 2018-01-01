##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##----CLT and Confidence Intervals----
##
##--------------------------------------------

x = c(rnorm(1000),rnorm(1000,mean=3,sd=0.5))
## This is not normal
plot(density(x), 
     xlab = 'non-Normal variable',
     main = 'Density of non-Normal distribution') 

# generate 500 samples of size 50
x_samples = lapply(1:500, function(i) sample(x, size=50, replace=TRUE))
x_means = lapply(x_samples, mean)
x_means = unlist(x_means)
breaks = seq(min(x_means), max(x_means), length.out = 30)
hist(x_means, breaks = breaks,
     main = 'Histogram of resampled means',
     xlab = 'Mean of non-Normal variable')
qqnorm(unlist(x_means)) # Yay normality!

pop_mean_estimate = mean(x_means)
pop_mean_estimate
pop_mean_sd = sd(x_means)
pop_mean_sd

actual_mean = mean(x)
actual_mean

# Create an alpha-level confidence interval
alpha = 0.95
half_width = qnorm((1+alpha)/2, mean=pop_mean_estimate, sd = pop_mean_sd) - pop_mean_estimate
half_width

ci_low = pop_mean_estimate - half_width
ci_high = pop_mean_estimate + half_width

print(paste('The actual mean is',round(actual_mean,3)))
print(paste('The',alpha,'level CI is (',round(ci_low,3),',',round(ci_high,3),').'))

## Plot the result with CI shown
hist(x_means, breaks = breaks,
     main = 'Histogram of resampled means',
     xlab = 'Mean of non-Normal variable')
abline(v = pop_mean_estimate, lwd = 2, col = 'red')
abline(v = ci_low, lwd = 2, col = 'red', lty = 3)
abline(v = ci_high, lwd = 2, col = 'red', lty = 3)
