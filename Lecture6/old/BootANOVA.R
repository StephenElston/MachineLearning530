## Read the dataset.
read.auto <- function(path = 'SET-YOUR-PATH-HERE'){
  ## Read the csv file
  filePath <- file.path(path, 'Automobile price data _Raw_.csv')
  auto.price <- read.csv(filePath, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  ## Compute a new feature; log of pirce
  auto.price$lnprice = log(auto.price$price)## Remove rows with missing values
  auto.price = auto.price[complete.cases(auto.price),]
  auto.price  
}
auto.price = read.auto(path = 'C:/Users/Steve/GIT/DataScience350/Lecture1')


plot.boot <- function(a, vars1, vars2, nbins = 80, p = 0.05){
  # Function to plot and print the 
  # results of the bootstrap resampling
  ##
  ## First step, make a histogram of the 
  ## bootstrap values.
  maxs = max(a)
  mins = min(a)
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, main = paste('Difference in means\n'
                                        , vars1, '-', vars2,
                                        '\nwith p = ',
                                         as.character(p)), 
                                         xlab = 'Difference in means')
  ## Compute, plot and print the mean and upper 
  # and lower confidence intervals.
  mean.a = mean(a)
  ci.lower = quantile(a, probs = p/2)
  ci.upper = quantile(a, probs = (1 - p/2))
  abline(v = mean.a, lwd = 4, col = 'red')
  abline(v = 0, lwd = 4, col = 'blue')
  abline(v = ci.lower, lty = 3, col = 'red', lwd = 3)  
  abline(v = ci.upper, lty = 3, col = 'red', lwd = 3)
  print(paste(vars1,'-', vars2,
              as.character(signif(mean.a, 5)),
              as.character(signif(ci.upper, 5)),
              as.character(signif(ci.lower, 5))))
}

boot.multiple = function(df, varb = 'body.style', col = 'lnprice', R = 10000, p = 0.05, threshold = 4){
  ## Function to bootstrap all the pairwise differences
  ## of means for a specified variable. 
  library(simpleboot)
  ## Find the unique values of the stratification variable
  levels = unique(df[, varb]) 
  combs = combn(levels, 2) # Find all pair-wise combinations
  npair = ncol(combs) # Number of pair-wise combinations
  par(mfrow = c(2, ceiling(npair/2)))
  print(' Level pair     mean     upper ci    lower ci')
  for(i in 1:npair){ # Loop over pair-wise combinations
    # Gather the values for each level in the pair
    name1 = combs[1,i] 
    name2 = combs[2,i]
    temp1 = df[df[,varb] == name1, col]
    temp2 = df[df[,varb] == name2, col]
    # Check that there are enough values in the level
    if(length(temp1) > threshold & length(temp2) > threshold){
      a = two.boot(temp1, temp2, mean, R = R) # Bootstrap
      plot.boot(a$t, name1, name2, p = p) # Plot
    }
  }
  par(mfrow = c(1,1))
}

table(auto.price$body.style) # Check the frequencies of the categories.
boot.multiple(auto.price) # Bootstrap the difference in means

## Interate through all the variables with 
## using an anonymous function and lapply 
## to reduce lines of code and improve readability
## and maintainability.
varbs = c('body.style', 'engine.type', 'drive.wheels', 'num.of.cylinders')
lapply(varbs, function(x) {
              print('')
              print(paste('Bootstrap comparison of', x))
              table(auto.price[, x])
              boot.multiple(auto.price, varb = x, 
                            p = 0.01) # More conservative with smaller probability
             })
              
## As opposed to all the cut and paste shown below!              
table(auto.price$engine.type) # Check the frequencies of the categories.
boot.multiple(auto.price, varb = 'engine.type', p = 0.01) # Bootstrap the difference in means

table(auto.price$drive.wheels) # Check the frequencies of the categories.
boot.multiple(auto.price, varb = 'drive.wheels', p = 0.01) # Bootstrap the difference in means

table(auto.price$num.of.cylinders) # Check the frequencies of the categories.
boot.multiple(auto.price, varb = 'num.of.cylinders', p = 0.01) # Bootstrap the difference in means