##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
## CLT
##
##--------------------------------------------

plot.hist = function(x, bins = 60){
  brks = seq(min(x), max(x), length.out = bins)
  hist(unlist(x_means), main = 'Histogram of sample mean', breaks = brks)
}

gen.sample = function(n, mean = 3, sd = 0.5){
  x = c(rnorm(n),rnorm(n, mean=mean, sd=sd))
  plot(density(x), main = 'Density estimate of sample') # Definitely not normal
  x
}

sample.means = function(x, n, size){
  x_samples = lapply(1:n, function(i) sample(x, size=size, replace=TRUE))
  lapply(x_samples, mean)
}

##----CLT and Confidence Intervals----
x = gen.sample(1000) # Definitely not normal

# generate 500 samples
x_means = sample.means(x, 500, size = 50 )
plot.hist(unlist(x_means))
qqnorm(unlist(x_means), main = 'Quantiles of standard Normal vs. sample mean') # Yay normality!

pop_mean_estimate = mean(unlist(x_means))
pop_mean_estimate
pop_mean_sd = sd(unlist(x_means))
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


## ----- Try with a larger sample of 100 with more samples -----
x = gen.sample(10000) 
x_means = sample.means(x, 5000, size = 500)
plot.hist(unlist(x_means))
qqnorm(unlist(x_means), main = 'Quantiles of standard Normal vs. sample mean') # Yay normality!

pop_mean_estimate = mean(unlist(x_means))
pop_mean_estimate
pop_mean_sd = sd(unlist(x_means))
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

