##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Solution to Home Work #6  ----
##

## Read the dataset.
read.auto <- function(path = 'SET-YOUR-PATH-HERE'){
  ## Read the csv file
  filePath <- file.path(path, 'Automobile price data _Raw_.csv')
  auto.price <- read.csv(filePath, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm',
               'city.mpg', 'highway.mpg', 'curb.weight', 
               'wheel.base', 'width', 'engine.size', 
               'length', 'height')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  ## Compute a new feature; log of pirce
  auto.price$lnprice = log(auto.price$price)## Remove rows with missing values
  auto.price = auto.price[complete.cases(auto.price),]
  ## Scale the numeric columns
  auto.price[, numcols] = data.frame(lapply(auto.price[, numcols], scale))
  auto.price
}

auto.price = read.auto(path = 'C:/Users/Steve/GIT/DataScience350/Lecture1')

## Create the model matrix from the model formula
mod.mat = model.matrix(lnprice ~ make + fuel.type +
                         aspiration + body.style + drive.wheels +
                         length + curb.weight + engine.type +
                         num.of.cylinders + engine.size + city.mpg,
                         data = auto.price)
dim(mod.mat) # Check the dimensions
head(mod.mat) # Quick sanity check

## Compute the SVD of the model matrix
mod.SVD = svd(mod.mat)

## Have a look at the singular values
mod.SVD$d

plot.sing = function(u){
  ln = length(u)
  plot(1:ln, u, col = 'red',
       main = ('Singular values'),
       xlab = 'Singular value order',
       ylab = 'Singular value')
}
plot.sing(mod.SVD$d)

## Set some singular values to 0 and create the inverse matrix
thresh = 1.0
d.th = sapply(mod.SVD$d, function(x) ifelse(x > thresh, 1/x, 0))
d.th # Check the values
plot.sing(d.th) # Plot the values
dInv = diag(d.th)

## Compute the pseudeo inverse
pInv = mod.SVD$v %*% t(dInv) %*% t(mod.SVD$u[, 1:42])

## Find the model coeficients
beta = pInv %*% auto.price$lnprice
beta

## Compute the predictions
pred = mod.mat %*% beta

## Compute and plot the residuals and compute SSE
resid = auto.price$lnprice - pred
plot(auto.price$lnprice, resid)
sum(resid*resid)


## ----- New threshold
## Set some singular values to 0 and create the inverse matrix
thresh = 5
d.th = sapply(mod.SVD$d, function(x) ifelse(x > thresh, 1/x, 0))
d.th # Check the values
plot.sing(d.th) # Plot the values
dInv = diag(d.th)

## Compute the pseudeo inverse
pInv = mod.SVD$v %*% t(dInv) %*% t(mod.SVD$u[, 1:42])


## Find the model coeficients
beta = pInv %*% auto.price$lnprice
beta

## Compute the predictions
pred = mod.mat %*% beta

## Compute and plot the residuals
resid = auto.price$lnprice - pred
plot(auto.price$lnprice, resid)
sum(resid*resid)

## ----- New threshold -------------
## Set some singular values to 0 and create the inverse matrix
thresh = 0.2
d.th = sapply(mod.SVD$d, function(x) ifelse(x > thresh, 1/x, 0))
d.th # Check the values
plot.sing(d.th) # Plot the values
dInv = diag(d.th)

## Compute the pseudeo inverse
pInv = mod.SVD$v %*% t(dInv) %*% t(mod.SVD$u[, 1:42])


## Find the model coeficients
beta = pInv %*% auto.price$lnprice
beta

## Compute the predictions
pred = mod.mat %*% beta

## Compute and plot the residuals
resid = auto.price$lnprice - pred
plot(auto.price$lnprice, resid)
sum(resid*resid)