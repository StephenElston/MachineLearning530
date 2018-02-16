##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Regression with R ----
##
##--------------------------------------------

sim.reg.data <- function(x1, y1, x2, y2, n, sd){
  w <- rnorm(n, mean = 0, sd = sd)
  data.frame(
    x = seq(from = x1, to = x2, length.out = n),
    y = (seq(from = y1, to = y2, length.out = n) + w)
  )
}


sim.reg.outlier <- function(x1, y1, x2, y2, n, sd, ox, oy){
  w <- rnorm(n, mean = 0, sd = sd)
  df <- data.frame(
    x = c(seq(from = x1, to = x2, length.out = n), ox),
    y = c((seq(from = y1, to = y2, length.out = n) + w), oy)
  )
  df[order(df$x),]
}


plot.reg <- function(df){
  require(ggplot2)
  ggplot(df, aes(x, y)) + 
    geom_point(size = 2) +
    ggtitle('X vs. Y')
}


plot.reg <- function(df){
  require(ggplot2)
  require(gridExtra)
  mod <- lm(y ~ x, data = df)
  df$score <- predict(mod)
  df$resids <- df$y - df$score
  
  ## compute the known 
  df$x.known = seq(0, max(df$x), length.out = nrow(df))
  
  p1 <- ggplot(df, aes(x, y)) + 
    geom_point(size = 2) +
    geom_line(aes(x, score, color = 'Red')) +
    geom_line(aes(x.known, x.known, color = 'DarkBlue')) + 
    ggtitle('X vs. Y with regression')
  
  p2 <- ggplot(df, aes(resids)) +
    geom_histogram() + 
    ggtitle('Distribution of residuals')
  
  grid.arrange(p1, p2, nrow = 2)
  
  print(paste('Intercept =', as.character(mod$coefficients[1])))
  print(paste('Slope =', as.character(mod$coefficients[2])))
  
  yBar <- mean(df$y)
  SSE <- sum((df$resids)^2)
  SST <- sum((df$y - yBar)^2)
  SSR <- sum((df$score - yBar)^2)
  n = nrow(df)
  adjR2  <- 1.0 - (SSE/SST) * ((n - 1)/(n - 3))
  print(paste('Sum of residuals = ', as.character(sum(df$resids))))
  print('SSE        SST         SSR')
  print(paste(as.character(signif(SSE, 5)), '  ',
              as.character(signif(SST, 5)), '  ',
              as.character(signif(SSR, 5))))
  print(paste('Adjusted R^2 =', as.character(adjR2)))
}


demo.reg <- function(){
  sd <- c(1, 5, 10)
  for(i in 1:3){
    regData <-  sim.reg.data(0, 0, 10, 10, 25, sd[i])
    plot.reg(regData)
  }
}


demo.outlier <- function(){
  ox <- c(0, 0, 5)
  oy <- c(20, -20, 20)
  for(i in 1:3){
    regData <-  sim.reg.outlier(0, 0, 10, 10, 25, 1, ox[i], oy[i])
    plot.reg(regData)
  }
}



##----- Regression with R -------------
##----- Gaulton's family data `1883 ---
##
require(HistData)
names(GaltonFamilies)

## Subset the data
males = GaltonFamilies[GaltonFamilies$gender == 'male',]

## Plot the data and correlations
pairs(~ father + mother + childHeight, 
      data = males)
cols = c('father', 'mother', 'childHeight')
cors = cor(males[, cols], method = 'pearson')
require(corrplot)
corrplot.mixed(cors, upper = "ellipse")

## Make a detailed scatter plot
require(ggplot2)
p1 = ggplot(males, aes(father, childHeight)) + 
  geom_point(size = 2, alpha = 0.3)
p1

## --- First model, with mean or intercept only ------
lm.males.mean = lm(childHeight ~ 1, data = males)
summary(lm.males.mean)

## Examine the fit
males$predicted.mean = predict(lm.males.mean, newdata = males)
ggplot(males, aes(father, childHeight)) + 
  geom_point(size = 2, alpha = 0.3) +
  geom_line(aes(father,predicted.mean), color = 'red')

## Examine the residuals
res = lm.males.mean$residuals
par(mfrow = c(1,2))
qqnorm(res)
breaks = seq(max(res), min(res), length.out = 31)
hist(res, breaks = breaks)
par(mfrow = c(1,1))


## ---- Second model with intercept and one independent variable
lm.males = lm(childHeight ~ father, data = males)
summary(lm.males)

xvals = seq(max(males$father), min(males$father), length.out = 101)
males$predicted.1 = predict(lm.males, newdata = males)
ggplot(males, aes(father, childHeight)) + 
  geom_point(size = 2, alpha = 0.3) + 
  geom_line(aes(father, predicted.1), color = 'red') +
  geom_line(aes(father,predicted.mean), color = 'DarkBlue', lty = 4, size = 1)

plot(lm.males) # Diagnostic plots


## --- Third model with two independent variables
lm.males.2 = lm(childHeight ~ father + mother, data = males)
summary(lm.males.2)

plot(lm.males.2)
 


