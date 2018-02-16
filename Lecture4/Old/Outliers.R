
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

## Create a plot function
plot.auto = function(col, df = auto.price, ycol = 'price'){
  require(ggplot2)
  ggplot(df, aes_string(col, ycol)) +
    geom_point(aes(size = 2, shape = factor(aspiration), 
                   color = factor(fuel.type)), alpha = 0.3) +
    ggtitle(paste(ycol, 'verses', col))
}

## Plot a few features with a strong relationship to auto price
plot.cols = c('city.mpg', 'engine.size', 'curb.weight')
lapply(plot.cols, plot.auto)

## The relationship is clearer went plotted against log of auto price
lapply(plot.cols, plot.auto, ycol = 'lnprice')

## Tag the outliers
auto.price$outliers = FALSE
outliers = which(auto.price$engine.size < 80 | 
                   auto.price$engine.size > 220 |
                   auto.price$curb.weight > 3600 |
                   auto.price$city.mpg < 15 |
                   auto.price$city.mpg > 40)
auto.price$outliers[outliers] = TRUE
## Print the outlier rows
auto.price[auto.price$outliers == TRUE, ]

## plot the result
plot.auto.outlier = function(col, df = auto.price, ycol = 'price'){
  require(ggplot2)
  ggplot(df, aes_string(col, ycol)) +
    geom_point(aes(size = factor(outliers), shape = factor(aspiration), 
                   color = factor(fuel.type)), alpha = 0.3) +
    ggtitle(paste(ycol, 'verses', col, 'with outliers highlighted'))
}
lapply(plot.cols, plot.auto.outlier, ycol = 'lnprice')

## Update the filter criteria for engine.size and fuel.type
auto.price$outliers = FALSE
outliers = which((auto.price$engine.size < 80 | 
                   auto.price$engine.size > 190 |
                   auto.price$curb.weight > 3600 |
                   auto.price$city.mpg < 15 |
                   auto.price$city.mpg > 40) &
                   auto.price$fuel.type != 'diesel')
auto.price$outliers[outliers] = TRUE

## Print and plot the outlier rows
auto.price[auto.price$outliers == TRUE, ]
lapply(plot.cols, plot.auto.outlier, ycol = 'lnprice')

## How many 3 cylinder cars are there?
auto.price[auto.price$num.of.cylinders == 'three', ]

## How many Hondas?
auto.price[auto.price$make == 'honda', ]
