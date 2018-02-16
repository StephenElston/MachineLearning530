## --------------------------------------------------
## --- Explore Simple Binary Bayesian Analysis ----
##
library(LearnBayes)
## Set the prior for texting drivers based on the 
## National stats
beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=0.3))
beta.par ## The parameters of my Beta distribution


## Get data on texting drivers and look at the distributions
par(mfrow = c(3,1))
## First 20 cars with 2 texting
beta.par + c(2, 18)
triplot(beta.par, c(2, 18))
## Next 20 cars with 4 texting
beta.par + c(2 + 4, 18 + 16)
triplot(beta.par, c(2 + 4, 18 + 16))
## Next 20 cars with 1 texting
beta.par + c(2 + 4 + 1, 18 + 16 + 19)
triplot(beta.par, beta.par + c(2 + 4 + 1, 18 + 16 + 19))
par(mfrow = c(1,1))


## Simulate from the prior and posterior and 
## compute confidence intervals
prior.post = function(sample, prob = 0.10){
  quants = quantile(sample, c(prob/2, 1 - prob/2))
  breaks = seq(min(sample), max(sample), length.out = 41)
  hist(sample, breaks = breaks, 
       main = 'Distribution of samples \n with 90% HDI',
       xlab = 'Sample value',
       ylab = 'Density')
  abline(v = quants[1], lty = 3, col = 'red', lwd = 3)
  abline(v = quants[2], lty = 3, col = 'red', lwd = 3)
}

par(mfrow = c(2,1))
prior.sample <- rbeta(10000, beta.par[1], beta.par[2])
prior.post(prior.sample, prob = 0.8)
print(paste('Mean of prior = ', as.character(mean(prior.sample))))
beta.post.par <- beta.par + c(2 + 4 + 1, 18 + 16 + 19)
post.sample <- rbeta(10000, beta.post.par[1], beta.post.par[2])
prior.post(post.sample)
print(paste('Mean of posterior = ', as.character(mean(post.sample))))
par(mfrow = c(1,1))



## What is the probability of texting drivers out of 100
## next 100 trials?
n <- 100
s <- 0:n
pred.probs <- pbetap(beta.par + c(2 + 4 + 1, 18 + 16 + 19), n, s)
plot(s, pred.probs, type="h", 
     main = 'Probability distribution of success in trail',
     xlab = 'Successes')
discint(cbind(s, pred.probs), 0.90)