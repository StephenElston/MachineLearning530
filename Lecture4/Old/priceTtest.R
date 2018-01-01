## Check for normality.
par(mfrow = c(1,2))
qqnorm(auto.price$price)
qqnorm(auto.price$lnprice)
par(mfrow = c(1,1))  

## A quick and dirty approach to t-test of auto price
##
## First creat two vectors of the log price for turbo and standard cars.
std = auto.price[auto.price$aspiration == 'std',]$lnprice
turbo = auto.price[auto.price$aspiration == 'turbo',]$lnprice

## Have a quick look at the data
par(mfrow = c(2,1))
hist(std)
hist(turbo)
par(mfrow = c(1,1))

## Looks like turbo cars cost more, so do the one-sided
## t-test to see if this difference is significant.\
t.test(turbo, std, alternative = 'greater')

## The p-value is < 0.05 so we are fairly confident that 
## turbo cars are significantly more expensive. 
## Also, note the difference in the means is about 
## 3.4 with the 95% confidence interval of .2 to Infinity.