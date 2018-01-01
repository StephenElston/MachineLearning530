##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Regression with R Part 2 ----
##
##----- Model matrix with categorical variables ---
##

auto.price = read.csv('C:\\Users\\StevePC2\\Documents\\Git\\DataScience350\\Lecture1\\Automobile price data _Raw_.csv',
                header = TRUE, stringsAsFactors = FALSE)

require(dplyr)
auto.price = select(data, -symboling, -normalized.losses)

## Coerce some character columns to numeric
numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)

## Remove cases or rows with missing values. In this case we keep the 
## rows which do not have nas. 
auto.price = auto.price[complete.cases(auto.price), ]

## Use log of price
auto.price$price = log(auto.price$price)


numcols <- c('wheel.base', 'length', 'width', 'height', 'curb.weight',
             'engine.size', 'bore', 'stroke', 'compression.ratio',
             'horsepower', 'peak.rpm', 'city.mpg', 'highway.mpg')
auto.price[, numcols] = lapply(auto.price[, numcols], scale)

unique(auto.price$body.style) # How many values?

## First attempt to build a simple model matrix
mod.mat = model.matrix(price ~ body.style, data = auto.price)
head(mod.mat)  # Note the use of contrast to intercept

## Or, code without the intercept
mod.mat = model.matrix(price ~ body.style + 0, data = auto.price)
head(mod.mat)

mod.mat = model.matrix(price ~ . - 1, data = auto.price)
ncol(mod.mat)
head(mod.mat)


MTM = t(mod.mat) %*% mod.mat
dim(MTM)

svdM = svd(MTM)

plot(1:nrow(MTM), svdM$d)

svdM$d[1:25]



