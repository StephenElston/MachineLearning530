##--------------------------------------------
##
## Exploring Data in R (lecture 1)
##
## Class: PCE Data Science 350
##
## Contains examples of:
##
## -Working with Distributions
##
##--------------------------------------------

##-----Exploring data Visually----
# Bernoulli (Binomial with n = 1)
p = 0.75
n = 1000
bern_samples = rbinom(n, 1, p) # Compute random draws
bern_sample_mean = sum(bern_samples)/length(bern_samples)
print(paste('p =', as.character(p), '   Sample mean = ', as.character(bern_sample_mean)))
bern_sample_var = bern_sample_mean * (1-bern_sample_mean)
bern_var = p*(1-p)
print(paste('Brenoulli variance = ', as.character(bern_var), '   Sample varience = ', as.character(bern_sample_var)))
hist(bern_samples)


# Binomial
N = c(5, 25, 75) # parameter list
binom_samples = lapply(N, function(x) rbinom(n, x, p))  # Compute list of random draws

binom_sample_means = lapply(binom_samples, mean)  # Compute list of sample means
binom_means = N*p
data.frame(BinomialMean = binom_means, SampleMean = unlist(binom_sample_means))

binom_sample_vars = lapply(binom_samples, var) # Compute list of sample variance
binom_vars = N*p*(1-p)
data.frame(BinomialVariance = binom_vars, SampleVariance = unlist(binom_sample_vars))
par(mfrow=c(1,3))
invisible(lapply(binom_samples, function(x) hist(x))) # histograms of random draws
par(mfrow=c(1,1))

# Compare Normal Approximation to binomial
par(mfrow=c(1,3))
for (i in 1:3){
  hist(binom_samples[[i]], main=paste(N[i],'Experiments'), freq=FALSE)
  x_norm = seq(0,N[i], by = 0.025)
  y_norm = dnorm(x_norm, mean=binom_means[i], sd=sqrt(binom_vars[i]))
  lines(x_norm, y_norm)
}
par(mfrow=c(1,1))


#Normal approximation to p(x=4)
pnorm(4.5, mean=binom_means[[1]], sd=sqrt(binom_vars[[1]])) -
  pnorm(3.5, mean=binom_means[[1]], sd=sqrt(binom_vars[[1]]))
#Empirical p(x=4)
sum(binom_samples[[1]]==4)/1000

#Normal approximation to p(x=18)
pnorm(18.5, mean=binom_means[[2]], sd=sqrt(binom_vars[[2]])) -
  pnorm(17.5, mean=binom_means[[2]], sd=sqrt(binom_vars[[2]]))
#Empirical p(x=18)
sum(binom_samples[[2]]==18)/1000

#Normal approximation to p(x=55)
pnorm(55.5, mean=binom_means[[3]], sd=sqrt(binom_vars[[3]])) -
  pnorm(54.5, mean=binom_means[[3]], sd=sqrt(binom_vars[[3]]))
#Empirical p(x=18)
sum(binom_samples[[3]]==55)/1000

# Poisson Distribution
lambda = c(1,5,25,100) # list of parmeters
poisson_samples = lapply(lambda, function(x) rpois(n, x))
poisson_sample_means = lapply(poisson_samples, mean) # Compute sample mean
poisson_sample_vars = lapply(poisson_samples, var) # Compute sample variance
data.frame(PoissonMean = lambda, SampleMean = unlist(poisson_sample_means), SampleVar = unlist(poisson_sample_vars))

#Normal approximation
par(mfrow=c(2,2))
for (i in 1:4){
  hist(poisson_samples[[i]], main=paste('Lambda=',lambda[i]), freq=FALSE)
  x_norm = seq(0,10*lambda[i], by = 0.025)
  y_norm = dnorm(x_norm, mean=lambda[i], sd=sqrt(lambda[i]))
  lines(x_norm, y_norm)
}
par(mfrow=c(1,1))


# The Uniform Distribution
n = 1000
uniform_samples = runif(n)

plot(density(uniform_samples), main = 'Density of Uniform distribution') ## density plot
lines(c(0,0),c(0,1), col = 556)
lines(c(0,1),c(1,1), col = 556)
lines(c(1,1),c(0,1), col = 556)


# Simulation with different n
n = c(100, 1000, 10000, 100000)
unifs = lapply(n, runif)


unif.hist = function(x, n) { 
  title = paste('Histogram of Uniform dist with', as.character(n), 'draws')
  breaks = seq(0, 1, length.out = 51)
  hist(x, breaks = breaks, main = title) 
  h = n/50
  lines(c(0,0),c(0,h), col = 'red')
  lines(c(0, 1),c(h, h), col = 'red')
  lines(c(1,1),c(0,h), col = 'red')
}

par(mfrow = c(2,2))
Map(unif.hist, unifs, n)
par(mfrow = c(1,1))


# The Normal Distribution
# Try different parameters
n = 100000
n_params = list(c(0,1),c(5,1),c(0,0.1),c(4,4))
norm_samples = lapply(n_params,function(x) rnorm(n,mean=x[1], sd=sqrt(x[2])))

par(mfrow=c(1,1))
colors = c('black', 'red', 'blue', 'green')
plot(density(norm_samples[[1]]), xlim=c(-3,9),ylim=c(0,1.25),
     col=colors[1], lwd=2, xlab='x', ylab='y', main='Plot of Normals')
for (i in 2:4){
  lines(density(norm_samples[[i]]), col=colors[i], lwd=2)
}
grid()
legend('topright',c('N(0,1)','N(5,1)','N(0,0.1)','N(4,4)'),
       lwd=2,lty=1,col=colors)


# Try different numbers of random draws
n = c(100, 1000, 10000, 100000)
norms = lapply(n, function(x) rnorm(x))


norm.hist = function(x, n) { 
  xbar = as.character(round(mean(x), digits = 3))
  title = paste('Histogram of std Normal with', as.character(n), 
                'draws \n', 'sample mean =', xbar)
  breaks = seq(min(x), max(x), length.out = 61)
  hist(x, breaks = breaks, main = title) 
}

par(mfrow = c(2,2))
Map(norm.hist, norms, n)
par(mfrow = c(1,1))
