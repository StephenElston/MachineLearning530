##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##-----------  More Bayes Models ----
##
## --------- Multilevel Models ----------
##
## Baseball data
##
library(LearnBayes)
d <- data.frame(Name=c("Clemente", "Robinson", "Howard", "Johnstone",
                       "Berry", "Spencer", "Kessinger", "Alvarado", "Santo",
                       "Swaboda", "Petrocelli", "Rodriguez", "Scott", "Unser",
                       "Williams", "Campaneris", "Munson", "Alvis"),
                Hits=c(18, 17, 16, 15, 14, 14, 13, 12, 11,
                        11, 10, 10, 10, 10, 10, 9, 8, 7),
                At.Bats=45)

## Use the Laplace method
laplace.fit <- laplace(betabinexch,
                       c(0, 0),
                       d[, c("Hits", "At.Bats")])
laplace.fit

## Use MCMC method
mcmc.fit <- rwmetrop(betabinexch,
                     list(var=laplace.fit$var, scale=2),
                     c(0, 0),
                     5000,
                     d[, c("Hits", "At.Bats")])
mcmc.fit$par[1:20]
mcmc.fit$accept

## Plot Laplace fit with the MCMC
## samples superimposed.
mycontour(betabinexch, c(-1.5, -0.5, 2, 12),
          d[, c("Hits", "At.Bats")],
          xlab="Logit ETA", ylab="Log K")
with(mcmc.fit, points(par))

## Build a table of the results for each player
eta <- with(mcmc.fit, exp(par[, 1]) / (1 + exp(par[, 1])))
eta[1:20]
K <- exp(mcmc.fit$par[, 2])
K[1:20]
p.estimate <- function(j, eta, K){
  yj <- d[j, "Hits"]
  nj <- d[j, "At.Bats"]
  p.sim <- rbeta(5000, yj + K * eta, nj - yj + K * (1 - eta))
  quantile(p.sim, c(0.05, 0.50, 0.95))
  }
E <- t(sapply(1:18, p.estimate, eta, K))
rownames(E) <- d[, "Name"]
round(E, 3)


## Plot the results
plot(d$Hits / 45, E[, 2], pch=19,
     ylim=c(.15, .40),
     xlab="Observed AVG", ylab="True Probability",
     main="90 Percent Probability Intervals")
for (j in 1:18) lines(d$Hits[j] / 45 * c(1, 1), E[j, c(1, 3)])
abline(a=0, b=1, col="blue")
abline(h=mean(d$Hits) / 45, col="red")
legend("topleft", legend=c("Individual", "Combined"),
        lty=1, col=c("blue", "red"))



## -----------------------------------------
## ------ Bayes factor ---------------------
##
## Create the fire data set
fire.counts <- c(75, 88, 84, 99, 79, 68, 86, 109, 73, 85, 101, 85,
                 75, 81, 64, 77, 83, 83, 88, 83, 78, 83, 78, 80,
                 82, 90, 74, 72, 69, 72, 76, 76, 104, 86, 92, 88)
hist(fire.counts, probability=TRUE, ylim=c(0, .08))
x <- 60:110
lines(x, dpois(x, lambda=mean(fire.counts)), col="red")
lines(x, dnorm(x, mean=mean(fire.counts), sd=12), col="blue")
lines(x, dnorm(x, mean=mean(fire.counts), sd=6), col="green")
legend("topright", legend=c("M1: Poisson(theta)",
                              "M2: N(theta, 12)",
                              "M3: N(theta, 6)"),
         col=c("red", "blue", "green"), lty=1)


## Create the first model
model.1 <- function(theta, y){
                     sum(log(dpois(y, theta))) +
                    dgamma(theta, shape=280, rate=4)
}

## Use Laplace method
library(LearnBayes)
log.pred.1 <- laplace(model.1, 80, fire.counts)$int
log.pred.1

## Create some other models
model.2 <- function(theta, y){
                              sum(log(dnorm(y, theta, 6))) +
                              dgamma(theta, shape=280, rate=4)
}
log.pred.2 <- laplace(model.2, 80, fire.counts)$int
log.pred.2

model.3 <- function(theta, y){
                              sum(log(dnorm(y, theta, 12))) +
                              dgamma(theta, shape=280, rate=4)
  }
log.pred.3 <- laplace(model.3, 80, fire.counts)$int
log.pred.3

## Compute the Bayes factors
exp(log.pred.1 - log.pred.2)
exp(log.pred.1 - log.pred.3)



##---------------------------------------------------
##---------- Hypothysis test on models using HDI -----
##
## Is Seattle rainfall significantly more than Spokane?
## 
library(LearnBayes)
## Choose a Beta with a wide spread
beta.par <- beta.select(list(p=0.5, x=0.3), list(p=0.75, x=.4))
beta.par ## The parameters of my Beta distribution

## Rain in 60 days for Seattle 
beta.post.sea = beta.par + c(36, 24)
triplot(beta.par, c(36, 24))

## Rain in 60 days for Spokane 
beta.post.skn = beta.par + c(20, 40)
triplot(beta.par, c(20, 40))

## Simulate from the posterior and 
## compute confidence intervals

plot.post = function(beta.post.par, HCI = 0.05){
  post.sample <- rbeta(10000, beta.post.par[1], beta.post.par[2])
  quants = quantile(post.sample, c(HCI/2, 1 - HCI/2))
  breaks = seq(min(post.sample), max(post.sample), length.out = 41)
  hist(post.sample, breaks = breaks, 
       main = paste('Distribution of samples \n with ', as.character(100 * (1 - HCI)), '% HDI', sep = ''),
       xlab = 'Sample value',
       ylab = 'Density',
       xlim = c(0,1))
  abline(v = quants[1], lty = 3, col = 'red', lwd = 3)
  abline(v = quants[2], lty = 3, col = 'red', lwd = 3)
  quants
}

par(mfrow = c(2,1))
lapply(list(beta.post.sea, beta.post.skn), plot.post, HCI = 0.05)
par(mfrow = c(2,1))

par(mfrow = c(2,1))
lapply(list(beta.post.sea, beta.post.skn), plot.post, HCI = 0.1)
par(mfrow = c(2,1))




### ------------------------------------------------
## Model diagnostics with coda package
##
## Set up the data set as a regression problem
require('rjags')
N <- 1000
x <- 1:N
epsilon <- rnorm(N, 0, 1)
y <- x + epsilon


## Run the jags model
path = 'C:\\Users\\StevePC2\\Documents\\Git\\DataScience350\\Lecture9' # SET YOUR PATH HERE!!
full.path = file.path(path, 'example.bug')
jags.mod.reg <- jags.model(full.path,
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 1000)

## Compute some samples
samples <- coda.samples(jags.mod.reg,
                        c('a', 'b'),
                        1000)

library(coda)
plot(samples) # Plot the result
summary(samples) # Summary statistics
cumuplot(samples) # Cumulative mean for each chain
gelman.plot(samples) # Gelman convergence plot

## Look at the autocorrelation of the chain
autocorr.diag(samples)
autocorr.plot(samples)

## What is the effective size of the sample?
effectiveSize(samples) 
rejectionRate(samples)





