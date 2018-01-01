##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Introduction to Bayesian models and MCMC ----
##

## -----------------------------------------
## --- Explore the Beta distribution ---
# Beta depends on two parameters, alpha and beta
alpha = c(0.5,1,2,3,4)
beta = alpha
x = seq(0.001,0.999,length=100)

par(mfrow = c(5,5), mar=c(2,1,2,1)) # mar = c(bottom, left, top, right)
sapply(alpha, function(a){
  sapply(beta, function(b){
    plot_title = paste("(a,b)=(",a,",",b,")")
    plot(x,dbeta(x,a,b),xlab="",ylab="",
         main=plot_title, type="l", lwd=2)
  })
})

# Set plot options back to normal
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))


## --------------------------------------------------
## --- Explore Simple Binary Bayesian Analysis ----
##
library(LearnBayes)
## I think the chance of rain is 0.2 with
## with a probability at the 75% point of 0.28
## Compute my Beta prior
beta.par <- beta.select(list(p=0.5, x=0.2), list(p=0.75, x=.28))
beta.par ## The parameters of my Beta distribution

## But there 6 sunny days and 4 rain days in the next 10 days
## And another 6 sunny days and 4 rain days in the following 10 days
## And another 12 sunny days and 8 rain days in the following 20 days
## And another 12 sunny days and 8 rain days in the following 20 days
## What does my posterior look like?
## 
par(mfrow = c(4,1))
beta.par + c(6, 4)
triplot(beta.par, c(6, 4))
beta.par + c(6 + 6, 4 + 4)
triplot(beta.par, c(6 + 6, 4 + 4))
beta.par + c(6 + 6 + 12, 4 + 4 + 8)
triplot(beta.par, c(6 + 6 + 12, 4 + 4 + 8))
beta.par + c(6 + 6 + 12 + 12, 4 + 4 + 8 +8)
triplot(beta.par, c(6 + 6 + 12 + 12, 4 + 4 + 8 + 8))
par(mfrow = c(1,1))


## Simulate from the posterior and 
## compute confidence intervals
beta.post.par <- beta.par + c(6 + 6 + 12 + 12, 4 + 4 + 8 + 8)
post.sample <- rbeta(10000, beta.post.par[1], beta.post.par[2])
par(mfrow = c(1,2))
quants = quantile(post.sample, c(0.05, 0.95))
breaks = seq(min(post.sample), max(post.sample), length.out = 41)
hist(post.sample, breaks = breaks, 
     main = 'Distribution of samples \n with 90% HDI',
     xlab = 'Sample value',
     ylab = 'Density')
abline(v = quants[1], lty = 3, col = 'red', lwd = 3)
abline(v = quants[2], lty = 3, col = 'red', lwd = 3)
qqnorm(post.sample)
par(mfrow = c(1,1))
quants

## Check on the model
predplot(beta.par, 6 + 8 + 12 + 12, 4 + 2 + 8 + 8)

## What is the probability of observing 0-8 successes in the
## next 60 trials?
n <- 60
s <- 0:n
pred.probs <- pbetap(beta.par, n, s)
plot(s, pred.probs, type="h", 
     main = 'Probability distribution of success in trail',
     xlab = 'Successes')
discint(cbind(s, pred.probs), 0.90)



## ------------------------------------------
##--- Discrete priors ------ 
#
# Create a uniform prior
p <- seq(0, 1, by = 0.01)
prior <- 1 / 101 + 0 * p

## With 20 successes and 12 failures find posterior
library(LearnBayes)
tries = c(20, 12)
post <- pdisc(p, prior, tries)
HCI = discint(cbind(p, post), 0.90) # 90 percent HCI

## Plot the prior and posterior
par(mfrow = c(2,1))
plot(p, prior, type="h",main="Posterior Distribution")
Main = paste("Posterior Distribution with N =",
             as.character(sum(tries)), 'Z =',
             as.character(tries[1]),
             'mu = ',
             as.character(tries[1]/sum(tries)))
plot(p, post, type="h",main=Main)
abline(v = HCI$set[1], lty = 3, col = 'red', lwd = 3)
abline(v = HCI$set[length(HCI$set)], , lty = 3, col = 'red', lwd = 3)
abline(v = tries[1]/sum(tries), col = 'red', lwd = 3)
par(mfrow = c(1,1))

## Print the 90 percent HCI 
HCI

## Make a prediction for the distribution of the 
## next 20 observations
n <- 20
s <- 0:20
pred.probs <- pdiscp(p, post, n, s)
plot(s, pred.probs, type="h",
     main="Predictive Distribution")




## ---------------------------------------------------
## --- Compare Multiple Parameter Estimation by MCMC ------
# Create a 'truth' distribution and plot some samples from it.
library(ggplot2)
library(MASS)
random_points = mvrnorm(10000, mu=c(0.5,0.5), Sigma=matrix(c(1,0.6,0.6,1), nrow=2))
plot(random_points[,1], random_points[,2], xlim=c(-4,4), ylim=c(-4,4), col=rgb(0,0,0,0.25),
     main = 'Draws from a bivariate Normal distribution')

# Now let's try to recreate that distribution via MCMC...

# Given a point, our value at that point(x,y) will be the 
# value of the distribution at x,y:
likelihood = function(x,y){
  sigma = matrix(c(1,0.6,0.6,1), nrow=2)
  mu = c(0.5,0.5)
  dist = c(x,y) - mu
  value = (1/sqrt(4*pi^2**det(sigma))) * exp((-1/2) * t(dist) %*% ginv(sigma) %*% t(t(dist)) )
  return(value)
}

# Where to start:
x_chain = 4
y_chain = -4
# Chain length:
chain_length = 10000

#Evaluate current position:
current_val = likelihood(x_chain,y_chain)
current_val

# Standard deviation of how far out to propose:
proposal_sd = .1

# Keep track of things:
accept_count = 0
reject_count = 0


for (n in 1:(chain_length-1)){ # chain length minus 1 because we already have a point (the starting point)
  proposed_x = x_chain[n] + rnorm(1, mean=0, sd=proposal_sd)
  proposed_y = y_chain[n] + rnorm(1, mean=0, sd=proposal_sd)
  proposed_val = likelihood(proposed_x, proposed_y)
  
  # Accept according to probability:
  if (runif(1) < (proposed_val/current_val)){
    x_chain = c(x_chain, proposed_x)
    y_chain = c(y_chain, proposed_y)
    current_val = proposed_val
    accept_count = accept_count + 1
  }else{
    x_chain = c(x_chain, x_chain[n])
    y_chain = c(y_chain, y_chain[n])
    reject_count = reject_count + 1
  } 
}

plot(x_chain, y_chain, col=rgb(0,0,0,0.25), xlim=c(-4,4), ylim=c(-4,4),
     main="MCMC values for a Bivariate Normal", xlab="x", ylab="y")

# Burn in problem.  Solution?  Throw away first part of chain.
num_burnin = round(0.1*chain_length)
num_burnin

plot(x_chain[num_burnin:chain_length], y_chain[num_burnin:chain_length],
     col=rgb(0,0,0,0.25), xlim=c(-4,4), ylim=c(-4,4),
     main="MCMC values for a Bivariate Normal with burn-in", xlab="x", ylab="y")

# Estimate bivariate mean from chain:
mcmc_mean = c(mean(x_chain), mean(y_chain))
mcmc_mean

# Acceptance/Reject rate:
accept_count/chain_length
reject_count/chain_length

# Always look at the chain, we would like random noise centered around means
par(mfrow = c(2,1))
plot(x_chain, type="l", main = 'X chain', ylab = 'Value')
plot(y_chain, type="l", main = 'Y chain', ylab = 'Value')
par(mfrow = c(1,1))

## Look at a shorter segment of the chain
# Always look at the chain, we would like random noise centered around means
par(mfrow = c(2,1))
plot(x_chain[1000:2000], type="l", main = 'X chain', ylab = 'Value')
plot(y_chain[1000:2000], type="l", main = 'Y chain', ylab = 'Value')
par(mfrow = c(1,1))

## Try some other sds for the MCMC!!!!!!!!!!!!!!!



## -------------------------------------------------
## --- Explore the MCMC method ----
## --- Chi Squared Example -----------
#
# Model the minimum and maxium speed of autos on a highway.
# There are two parameters in the model, the min and the max
#
# The minmaxpost functon computes the log likelihood
# of the max and min of the speed. Theta is a list of two
# mu and log(sigma).
library(LearnBayes)
minmaxpost <- function(theta, data){
  mu <- theta[1]
  sigma <- exp(theta[2])
  dnorm(data$min, mu, sigma, log=TRUE) +
    dnorm(data$max, mu, sigma, log=TRUE) +
    (data$n - 2) * log(pnorm(data$max, mu, sigma) -
                       pnorm(data$min, mu, sigma))
}

# Some data and compute the posterior using the Laplace method
data <- list(n=10, min=52, max=84)
fit <- laplace(minmaxpost, c(70, 2), data)
fit

# Plot to see the sampling
mycontour(minmaxpost, c(45, 95, 1.5, 4), data,
          xlab=expression(mu), ylab=expression(paste("log ",sigma)))
mycontour(lbinorm, c(45, 95, 1.5, 4),
            list(m=fit$mode, v=fit$var), add=TRUE, col="red",
            main = 'Contours of posterior with Normal approx in red')


## Random walk M-H sampling
## Compute the randow walk fit
mcmc.fit <- rwmetrop(minmaxpost,
                     list(var=fit$v, scale=3),
                     c(70, 2),
                     10000,
                     data)
mcmc.fit$accept  # What is the acceptance ratio

## Countour plot of the outcome
mycontour(minmaxpost, c(45, 95, 1.5, 4), data,
          xlab=expression(mu),
          ylab=expression(paste("log ",sigma)))
points(mcmc.fit$par)

## What does the distibution of one of our model parameters look like.
## Look at the distribution of the upper 75% quartile
mu <- mcmc.fit$par[, 1]
sigma <- exp(mcmc.fit$par[, 2])
P.75 <- mu + 0.674 * sigma
plot(density(P.75),
       main="Posterior Density of Upper Quartile")

