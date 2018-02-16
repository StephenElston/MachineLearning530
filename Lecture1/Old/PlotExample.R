##--------------------------------------------
##
## Exploring Data in R (lecture 1)
##
## Class: PCE Data Science 350
##
## Contains examples of:
##
## - Visualization and data exploration using ggplot2
##   and auto price data.
##
##--------------------------------------------

##  Read the csv file into a data frame
read.auto <- function(path = 'SET-YOUR-PATH-HERE'){
  ## Function to read the csv file
  filePath <- file.path(path, 'Automobile price data _Raw_.csv')
  auto.price <- read.csv(filePath, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm',
               'highway.mpg', 'city.mpg', 'compression.ratio',
               'engine.size', 'curb.weight', 'height', 'width',
               'length', 'wheel.base', 'normalized.losses',
               'symboling')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  auto.price
  
}

Auto.Price = read.auto(path = '.') ## Read the csv file

## Create a simple scatter plot
require(ggplot2)
p1 = ggplot(Auto.Price, aes(city.mpg, price)) + geom_point()

## Use asthetics to highlight the data

## Use a larger point size
p1 + geom_point(aes(size = 2))

## Asthetic: Use color to show another dimension
p1 + geom_point(aes(size = 2, color = factor(aspiration)))

## Asthetic: transparency
p1 + geom_point(aes(size = 2, color = factor(aspiration), alpha = 0.2))

## Asthetic: Add shape to show another dimenstion
p1 + geom_point(aes(size = 2, color = factor(aspiration), alpha = 0.2, shape = factor(fuel.type)))


## Scatter plot matrix
pairs(~ price + city.mpg + engine.size + curb.weight + length,
      data=Auto.Price, main="Simple Scatterplot Matrix of Auto Data")


## Histogram with different bin types
require(gridExtra)
h = ggplot(Auto.Price, aes(price)) 

n = 19
bw = (max(Auto.Price$price, na.rm = TRUE) - min(Auto.Price$price, na.rm = TRUE))/n
title = paste('Histogram with', as.character(n), 'bins')
h1 = h + geom_histogram(binwidth = bw, na.rm = TRUE) + ggtitle(title)

n = 59
bw = (max(Auto.Price$price, na.rm = TRUE) - min(Auto.Price$price, na.rm = TRUE))/n
title = paste('Histogram with', as.character(n), 'bins')
h2 = h + geom_histogram(binwidth = bw, na.rm = TRUE) + ggtitle(title)

grid.arrange(h1, h2, nrow = 1) ## Plot the histograms


## Box plots of categorical variables
bp1 = ggplot(Auto.Price, aes(fuel.type, price)) + geom_boxplot(notch = TRUE)
bp2 = ggplot(Auto.Price, aes(body.style, price)) + geom_boxplot()
grid.arrange(bp1, bp2, nrow = 1) ## Plot the box plots


## Violin plots of categorical variables
vp1 = ggplot(Auto.Price, aes(fuel.type, price)) + geom_violin()
vp2 = ggplot(Auto.Price, aes(body.style, price)) + geom_violin()
grid.arrange(vp1, vp2, nrow = 1) ## Plot the box plots


## Normal q-q plot
par(mfrow=c(1,2))
qqnorm(Auto.Price$price, main = 'Normal Q-Q Plot of price')
qqline(Auto.Price$price)
qqnorm(log(Auto.Price$price), main = 'Normal Q-Q Plot of log price')
qqline(log(Auto.Price$price))
par(mfrow=c(1,1))


## Conditioned (faceted) plots
## Histogram with one conditioning variable
h2 + facet_grid(. ~ aspiration)

## Histogram with two conditioning variables
h2 + facet_grid(fuel.type ~ aspiration)

## Scatter plot with one conditioning variable
p1 + facet_grid(. ~ aspiration)

## Scatter plot with two conditioning variables
c1 = p1 + facet_grid(fuel.type ~ aspiration)
c1

## Scatter plot with three conditioning variables
p1 + facet_grid(fuel.type ~ aspiration + drive.wheels)

## Conditioning with two variables plus asthetic
c1 + geom_point(aes(color = factor(drive.wheels)))

