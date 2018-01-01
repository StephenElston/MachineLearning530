##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Introduction to time series analysis ----
##

## --- R time series objects.
#
vec = 1:365  # A vector of values
class(vec) # Vector is an atomic class in R

## Create a ts class object from the vector
## by adding time series attributes
vec.ts = ts(vec, start = 1990, freq = 365)
class(vec.ts)  # Check the class
attributes(vec.ts) # Note the time series attributes
plot(vec.ts) # Note the x-axis is the time attribute

## ---------- Properties of simple time series ----
#
plot.acf <- function(df, col = 'remainder', is.df =TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
  par(mfrow = c(1,1))
}


dist.ts = function(df, col = 'residual', bins = 40){
  par(mfrow = c(1,2))
  temp = as.vector(df)
  breaks = seq(min(temp), max(temp), length.out = (bins + 1))
  hist(temp, breaks = breaks, main = paste('Distribution of ', col), xlab = col)
  qqnorm(temp, main = paste('Normal Q-Q plot of ', col))
  par(mfrow = c(1,1))
}

## Investigate the time series properties of white noise
ts.white = function(n, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  ts(rnorm(n, mean = mean, sd = sd), start = start, freq = 12)
}
white = ts.white(300)
plot(white, main = 'White noise time series')
dist.ts(white, col = 'white noise')
plot.acf(white, col = 'white noise', is.df = F)

## Investigate the time series properties of random walk
ran.walk = function(n, freq = 12, start = 1990, sd = 1.0, mean = 0.0){
  norms = rnorm(n, mean = mean, sd = sd)
  ts(cumsum(norms), start = start, freq = 12)
}
ranWalk = ran.walk(300)
plot(ranWalk, main = 'Random walk time series')
dist.ts(ranWalk, col = 'random walk')
plot.acf(ranWalk, col = 'random walk', is.df = F)


## ---- Investigate time series properties of 
## trend + white noise
ts.trend = function(n, slope = 0.01, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  temp = seq(0, slope * n, length.out = n) + 
          rnorm(n, mean = mean, sd = sd)
  ts(temp, start = start, freq = 12)
}
trend = ts.trend(300, slope = 0.01)
plot(trend, main = 'Trend + white noise time series')
dist.ts(trend, col = 'trend + white noise')
plot.acf(trend, col = 'trend + white noise', is.df = F)


## --- Decomposition of the time series into components
ts.decomp <- function(df, col = 'elec.ts', span = 0.5, Mult = TRUE, is.df = TRUE){
  # if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with lowess span = ', as.character(span)))
  fit$time.series
}
temp = ts.decomp(trend, is.df = FALSE)
plot.acf(temp[,3], is.df = FALSE)

## Use a first order difference series to 
## remove the trend
ts.diff <- function(ts, lag = 1){
  diff(ts, lag = lag)
}
diff.out = ts.diff(trend, lag = 1)
plot(diff.out, main = 'Difference series of trend + white noise time series')
plot.acf(diff.out, col = 'difference of trend + white noise', is.df = F)

## Take first order difference of random walk
diff.walk = ts.diff(ranWalk, lag = 1)
plot(diff.walk, main = 'Difference series of random walk time series')
dist.ts(diff.walk, col = 'random walk')
plot.acf(diff.walk, col = 'difference of random walk ', is.df = F)



## --- Investigate time series properties of 
## trend + white noise + seasonal
ts.season = function(n, slope = 0.01, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
   temp = seq(0, slope * n, length.out = n) + 
    rnorm(n, mean = mean, sd = sd) +
    2 * sin(0:(n -1) * pi / freq) +
     cos(0:(n -1) * pi / freq)
  ts(temp, start = start, freq = 12)
}
season = ts.season(300, slope = 0.01)
plot(season, main = 'Trend + white noise + seasonal time series')
plot.acf(season, col = 'trend + white noise + seasonal', is.df = F)
dist.ts(season, col = 'trend + white noise + seasonal')
season.decomp = ts.decomp(season, is.df = FALSE)
plot.acf(season.decomp[,3], is.df = FALSE)


## ---- Simple ARMA models ------
## Simulate an ARMA process
arma.sim = function(ar = c(0.9), ma = c(0), n = 300, mean = 1.0){
  ar1.model = list(ar = ar, ma = ma)
  print(ar1.model)
  ar1 = mean + arima.sim(model = ar1.model, n = n)
  ar1
}


## Function for ARIMA model estimation
ts.model = function(ts, col = 'remainder', order = c(0,0,1)){
  mod = arima(ts, order = order, include.mean = FALSE)
  print(mod)
  mod
}

## --- AR(1) process
arMod = arma.sim()
plot(arMod, main = 'Plot of AR(1) model time series')
plot.acf(arMod, col = 'AR(1) model', is.df = F)
mod.est = ts.model(arMod, col = 'AR(1) process', order = c(1,0,0))
plot.acf(mod.est$resid[-1], col = 'AR(1) estimate', is.df = F)

## --- MA(1) process
arMod = arma.sim(ar = c(0.001), ma = (0.9))
plot(arMod, main = 'Plot of MA(1) model time series')
plot.acf(arMod, col = 'MA(1) model', is.df = F)
mod.est = ts.model(arMod, col = 'MA(1) process', order = c(0,0,1))
plot.acf(mod.est$resid[-1], col = 'MA(1) estimate', is.df = F)

## ---- ARMA(1,1) process
arMod = arma.sim(ar = c(0.9), ma = (0.9))
plot(arMod, main = 'Plot of ARMA(1,1) model time series')
plot.acf(arMod, col = 'ARMA(1,1) model', is.df = F)
mod.est = ts.model(arMod, col = 'ARMA(1,1) process', order = c(1,0,1))
plot.acf(mod.est$resid[-1], col = 'ARMA(1,1) estimate', is.df = F)

## ARMA model of trend difference process
plot.acf(diff.out, col = 'difference of trend + white noise', is.df = F)
diff.est = ts.model(diff.out, col = 'ARMA(1,1) process', order = c(1,0,1))
plot.acf(diff.est$resid[-1], col = 'ARMA(1,1) estimate', is.df = F)

## ARMA model of the random walk difference process 
plot.acf(diff.walk, col = 'difference of random walk ', is.df = F)
walk.est = ts.model(diff.walk, col = 'ARMA(1,1) process', order = c(1,0,1))
plot.acf(walk.est$resid[-1], col = 'ARMA(1,1) estimate', is.df = F)


## --- ARIMA modeling of trend series
arima.est = ts.model(trend, col = 'ARIMA process model', order = c(1,1,1))
plot.acf(arima.est$resid[-1], col = 'ARIMA model result', is.df = F)]

## ARIMA modeling of random walk with first order diff
arima.walk = ts.model(ranWalk, col = 'ARIMA process model', order = c(0,1,0))
plot.acf(arima.walk$resid[-1], col = 'ARIMA model result', is.df = F)

## ARIMA modeling of random walk with ARIMA(1,1,0)
arima.walk = ts.model(ranWalk, col = 'ARIMA(1,1,0) process model', order = c(1,1,0))
plot.acf(arima.walk$resid[-1], col = 'ARIMA(1,1,0) model result', is.df = F)



### --------------- Optional
ar.sim = function(ts.in, freq = 12, start = 1990, AR1 = 0.7, sd = 1.0, mean = 0.0){
  n = length(ts.in)
  w = rnorm(n = n, mean = mean, sd = sd)
  for(t in 2:n) ts.in[t] = 0.7 * ts.in[t - 1] + w[t]
  ts(ts.in, start = start, freq = freq)
}


### ------- Real world data sets ------------
#
# --------Electricity, beer, and chocolate ----
#
# Load the data from the Internet
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/cbe.dat"
CBE = read.table(www, header = TRUE)
head(CBE)

elec.ts = ts(CBE[,3], start = 1958, freq = 12)
beer.ts = ts(CBE[,2], start = 1958, freq = 12)
lnbeer.ts = ts(log(CBE[,2]), start = 1958, freq = 12)
choclate.ts = ts(CBE[,1], start = 1958, freq = 12)
aus.ts = cbind(elec.ts, beer.ts, lnbeer.ts, choclate.ts)

## First look at the series
plot(aus.ts)


## --- The beer consumption time series
## Find an ARIMA model of beer
beer.arima = ts.model(aus.ts[,'beer.ts'], col = 'ARIMA model for beer', order = c(1,1,1))

## End with ARIMA(2,1,1) model
beer.arima = ts.model(aus.ts[,'beer.ts'], col = 'ARIMA model for beer', order = c(2,1,1))
plot.acf(beer.arima$resid[-1], col = 'beer ARIMA model result', is.df = F)
dist.ts(beer.arima$resid[-1])

## Compare the ARIMA simulated series with the actual
ts.sim = function(mod, ts){
  n = length(ts)
  start = attributes(out[,3])$tsp[1]
  freq = attributes(out[,3])$tsp[3]
  out = arima.sim(as.list(coef(mod)), n = n)
  out = ts(out, freq = freq, start = start)
  par(mfrow = c(2,1))
  par(mar = c(5,5,2,5))
  plot(ts, ylab = 'Original time series', xlab = '',
       main = 'Original and simulated time series')
  plot(out, ylab = 'Simulated time series')
  par(mfrow = c(1,1))
  out
}
temp = ts.sim(beer.arima, aus.ts[,'beer.ts'])


## Decompose the beer series
beer.decomp = ts.decomp(aus.ts, col = 'beer.ts')
plot.acf(beer.decomp, col = 'remainder')

## Model of the remainder
beer.arima = ts.model(beer.decomp[,'remainder'], col = 'remainder', order = c(0,0,0))
plot.acf(beer.arima$resid[-1], col = 'beer ARIMA model result', is.df = F)
dist.ts(beer.arima$resid[-1])

temp = ts.sim(beer.arima, beer.decomp[,'remainder'])

## -- Decompose the log of the beer series
lnbeer.decomp = ts.decomp(aus.ts, col = 'lnbeer.ts')
plot.acf(lnbeer.decomp, col = 'remainder')

## Model of the remainder of log(beer)
lnbeer.arima = ts.model(lnbeer.decomp[,'remainder'], col = 'remainder', order = c(0,0,0))
plot.acf(lnbeer.arima$resid[-1], col = 'beer ARIMA model result', is.df = F)
dist.ts(lnbeer.arima$resid[-1])
temp = ts.sim(lnbeer.arima, beer.decomp[,'remainder'])


## --- Forecast beer production
library(forecast)
fit.beer = auto.arima(aus.ts[, 'lnbeer.ts'], max.p=3, max.q=3,
                       max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=0, start.q=0, start.P=0, start.Q=0)
summary(fit.beer)

## Make the forecast for the next year
beer.forecast = forecast(fit.beer, h=12)
summary(beer.forecast)
plot(beer.forecast)