##--------------------------------------------
##
## Introduction to Hypothesis testing
##
## Class: PCE 350 Data Science Methods Class
##
##--------------------------------------------

# You can imagine that there is a inherent variation in the population,
#  this is the standard deviation we have found above.

# But also, there is some variation in finding the mean of the outcomes.
#  The more coin flips, the closer the mean will be to 0.5... see the plot of
#  the running average above.  This plot scales with 1/sqrt(n).


##----Prob between two points on a Normal-----

prob_normal = function(a, b, mean=0, sd=1){
  stopifnot(a<=b) # Test input condition
  return(pnorm(b,mean,sd) - pnorm(a,mean,sd))
}

#One tailed
prob_normal(20.1262055, Inf, 15, 4) # 10% of the area lies to the left of 20.1262055 on N(15,4)

prob_normal(-Inf,Inf)
prob_normal(-1,1)
prob_normal(-2,2)
prob_normal(-3,3)


##----Cutoff function on a Normal----

# If we want a cutoff area percentage on a normal distribution,
#  we need the total areal to the left (and right if 2-tailed),
#  what x-value gives us that area?

cutoff_stat = function(alpha, mean=0, sd=1, one_tailed=TRUE){
  stopifnot((alpha>0) & (alpha<1))
  if (one_tailed){
    return(qnorm(1-alpha, mean, sd))
  }else{
    return(qnorm(1-(alpha/2), mean, sd))
  }
}

cutoff_stat(0.1, 15, 4)

##-----------T-Test----------------
pop_A = rnorm(25, mean=150, sd = 7)
pop_B = rnorm(25, mean=140, sd = 4)

plot.t <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 20){
  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  par(mfrow = c(2, 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols[1]), xlab = cols[1])
  abline(v = mean(a), lwd = 4, col = 'red')
  hist(b, breaks = breaks, main = paste('Histogram of', cols[2]), xlab = cols[2])
  abline(v = mean(b), lwd = 4, col = 'red')
  par(mfrow = c(1, 1))
}
plot.t(pop_A, pop_B)

# p-value for t-test
?t.test
# One-tailed test: is mean(pop_A) > mean(pop_B)?
#H_o: mean(pop_A) <= mean(pop_B)
#H_a: mean(pop_A) > mean(pop_B)
t.test(pop_A, pop_B, alternative = "greater")


# Two-tailed test: is mean(pop_A) != mean(pop_B)
#H_o: mean(pop_A) = mean(pop_B)
#H_a: mean(pop_A)!= mean(pop_B)
t.test(pop_A, pop_B, alternative = "two.sided")

## -----Using the GaltonFamilies dataset - 1886 -------------
library(HistData)
head(GaltonFamilies)

## Is the average height of men different from women?
male = GaltonFamilies[GaltonFamilies$gender == 'male',]
female = GaltonFamilies[GaltonFamilies$gender == 'female',]
plot.t(male$childHeight, female$childHeight, cols = c('Hight of males', 'Heigh of female'))
t.test(male$childHeight, female$childHeight, alternative = "two.sided")

## Or fathers and their sons
## Welche's test for paired data
plot.t(male$father, male$childHeight, cols = c('Hight of fathers', 'Heigh of sons'))
t.test(male$father, male$childHeight, alternative = "two.sided", paired = TRUE)


##----Chi-squared Test----

ab_data = data.frame(occurrence=c(55,43,22),
                     expected_per=c(0.6,0.3,0.1))
ab_data$expected_occurrence = 120 * ab_data$expected_per
ab_data

chisq.test(ab_data$occurrence, p = ab_data$expected_per)
1-pchisq(13.708, df=2) # 13.708 from slides



##-----Fisher's Exact in R-----
mat_test = matrix(c(2,3,3,4), nrow=2, byrow=TRUE)
mat_test
fisher.test(mat_test, alternative = "less")

# Explicit
fisher_outcome_p = function(a,b,c,d){
  n = choose(a+c,a)*choose(b+d,b)
  d = choose(a+b+c+d, a+b)
  return(n/d)
}

# One exact outcome:
#do.call(fisher_outcome_p, as.list(c(mat_test)))


# or
all_outcomes = list(c(2,3,3,4),c(1,4,4,3),c(0,5,5,2))
outcome_probs = lapply(all_outcomes, function(x) do.call(fisher_outcome_p, as.list(c(x))))
p = sum(unlist(outcome_probs))
p


##-----K-S Test-----

norm1 = rnorm(100,mean=0,sd=1)
norm2 = rnorm(100,mean=0,sd=1)

## Visual test of normality
par(mfrow = c(1, 2))
qqnorm(norm1, main = 'Q-Q plot of norm1')
qqnorm(norm2, main = 'Q-Q plot of norm2')
par(mfrow = c(1, 1))

## Or, plot one distribution against another.
plot(sort(norm1), sort(norm2), main = 'Plot of norm1 vs. norm2', 
     xlab = 'Quantiles of norm1', ylab = 'Qunatiles of norm2')
abline(a = 0.0, b = 1.0, lty = 2, col = 'blue')

## Plot the cdfs
plot(ecdf(norm1), col='blue', main ='CDFs of samples', 
     xlab = 'Value', ylab = 'Cumulative density')
lines(ecdf(norm2), col='red')

# Have to standardize the x-values
x_seq = seq(-3,3,len=100)
y_cdf1 = sapply(x_seq, function(x){
  sum(norm1<x)/length(norm1)
})
y_cdf2 = sapply(x_seq, function(x){
  sum(norm2<x)/length(norm1)
})

plot(x_seq,y_cdf1, col='blue', pch=16, main ='CDFs of standardized samples', 
     xlab = 'Value', ylab = 'Cumulative density')
points(x_seq,y_cdf2,col='red', pch=16)

## Find the max deviation
k_s_stat = max(abs(y_cdf1-y_cdf2))
k_s_stat
# where does it occur?
k_index = which.max(abs(y_cdf1-y_cdf2))
k_s_x = x_seq[k_index]
lines(c(k_s_x,k_s_x), c(y_cdf1[k_index],y_cdf2[k_index]),
      col='black', lwd=2)

# Create k-s statistic function
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}

##----Repeat N Times-----
N = 1000
k_s_rep = sapply(1:N, function(i){
  dist_a = rnorm(100,mean=0,sd=1)
  dist_b = rnorm(100,mean=0,sd=1)
  return(ks_stat(-3, 3, dist_a, dist_b))
})

hist(k_s_rep, breaks=30, freq=FALSE, xlab = 'K-S statistic',
     main = 'Histogram of k-s statistic')
lines(density(k_s_rep))

##----Empirical Two Tailed KS test-----
# Alternative hypothesis is that the k-s- statistic
#  is greater than the "expected value".

dist1 = rnorm(1000, mean=0.05, sd = 1)
dist2 = rnorm(1000, mean=0, sd = 1)

# We hypothesize that dist2 is normal(0,1), like we know dist1 is.

k_s_stat = ks_stat(-5,5, dist1, dist2)

# What should the distribution be?
# i.e. what is the expected value?
k_s_hypothesis = sapply(1:500, function(i){
  dist_a = rnorm(1000,mean=0,sd=1)
  dist_b = rnorm(1000,mean=0,sd=1)
  return(ks_stat(-3, 3, dist_a, dist_b))
})

hist(k_s_hypothesis, breaks=30, xlab = 'K-S statistic',
     main = 'Histogram of k-s statistic')

empirical_p_value = sum(k_s_hypothesis>k_s_stat)/500
empirical_p_value


##-----Shapiro-Wilk's Test----
dist_a = rnorm(100,mean=0,sd=1)
shapiro.test(dist_a)
# Can NOT reject null that dist_a is from a normal population.

##----Look at Normal Quantile-Quantile Plot----
qqnorm(dist_a, pch=16)
abline(0,1, lwd=2, col="red")


