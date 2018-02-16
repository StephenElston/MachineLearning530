##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Regression with R Part 2 ----
##
## ----- Feature Selection Stepwise Regression and ANOVA -------
##
## Start by subsetting the dataset and adding some
## new feature columns.
require(HistData)
males = GaltonFamilies[GaltonFamilies$gender == 'male',]
males.ext = males[, c('mother', 'father', 'childHeight')]
males.ext[, c('mother.sqr', 'father.sqr', 'motherfather')] = 
  data.frame(males$mother^2, males$father^2, 
             males$mother * males$father)

## Compute a simple baseline model
lm.simple = lm(childHeight ~ mother + father, data = males.ext)
summary(lm.simple)

## Compute a model using all features
lm.males = lm(childHeight ~ ., data = males.ext)
summary(lm.males)
plot(lm.males)

## Stepwise regression for feature selection
library(MASS)
lm.step = stepAIC(lm.males, direction = 'both')
lm.step$anova # ANOVA of the result 
summary(lm.step) # Summary of the best model

## Try again starting with a less over-determined model
lm.males = lm(childHeight ~ mother + father + father.sqr + mother.sqr, data = males.ext)
summary(lm.males)

## Apply step wise regression to the new model
lm.step = stepAIC(lm.males, direction = 'both')
lm.step$anova
summary(lm.step)
plot(lm.step)

## Compare to simple model
summary(lm.simple)

## Did any of this make a difference? 
anova(lm.males, lm.simple)
anova(lm.step, lm.simple)



