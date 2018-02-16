##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Regression with R Part 2 ----
##
##----- Gaulton's family data `1883 ---
##
require(HistData)
names(GaltonFamilies)

## Subset the data
males = GaltonFamilies[GaltonFamilies$gender == 'male',]

## Model with intercept and one independent variable
lm.males = lm(childHeight ~ father, data = males)
summary(lm.males)

## Bootstrap and plot the linear model
require(simpleboot)
lm.males.boot = lm.boot(lm.males, R = 10000)
plot(lm.males.boot)

## Plot the histogram of the bootstrapped coeficents
plot.dist <- function(a, name = 'Intercept', nbins = 80, p = 0.05){
  maxs = max(a)
  mins = min(a)
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, 
       main = paste('Histogram of distribution of the', name), 
       xlab = paste('Values of', name))
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}

## View distribution of model coeficients
intercept = sapply(1:length(lm.males.boot$boot.list),
                   function(x) lm.males.boot$boot.list[[x]]$coef[1])
slope = sapply(1:length(lm.males.boot$boot.list),
               function(x) lm.males.boot$boot.list[[x]]$coef[2])

par(mfrow = c(1,2))
plot.dist(intercept, name = 'intercept')
plot.dist(slope, name = 'slope')
par(mfrow = c(1,1))

## Model with intercept and two independent variables
lm.males.2 = lm(childHeight ~ father + mother, data = males)
summary(lm.males.2)

## Compute the bootstrap and of the model
lm.males.boot2 = lm.boot(lm.males.2, R = 10000)

## View the distribution of the intercept
intercept = sapply(1:length(lm.males.boot2$boot.list),
                   function(x) lm.males.boot2$boot.list[[x]]$coef[1])
plot.dist(intercept)

## Extract the bootstrap distribtion
## of the slope parameters
lm.coef = function(lm, ncoefs = 2){
  ln = length(lm$boot.list)
  print(ln)
  slopes = data.frame(rep(0, ln))
  for(i in 1:ncoefs){
    slopes[,i] = sapply(1:length(lm$boot.list),
                     function(x) lm$boot.list[[x]]$coef[i + 1])
  }
  slopes
}
slopes = lm.coef(lm.males.boot2)

## Plot the distribution of the slope parameters
plot.slopes = function(slopes, ncoefs = 2){
  par(mfrow = c(ncoefs,1))
  for(i in 1:ncoefs){
    name = paste('Distribution of b', as.character(i))
    plot.dist(slopes[,i], name = name)
  }
  par(mfrow = c(1,1))
}
plot.slopes(slopes)


## Model with intercept and two independent variable
## and interaction term
lm.males.3 = lm(childHeight ~ father + mother + father * mother, data = males)
summary(lm.males.3)

## Compute the bootstrap and of the model
lm.males.boot3 = lm.boot(lm.males.3, R = 10000)

## Plot the distribution of the intercept and 
## slope coeficients
intercept = sapply(1:length(lm.males.boot3$boot.list),
                   function(x) lm.males.boot3$boot.list[[x]]$coef[1])
plot.dist(intercept)
slopes = lm.coef(lm.males.boot3, ncoefs = 3)
plot.slopes(slopes, ncoefs = 3)


