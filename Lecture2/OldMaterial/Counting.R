##--------------------------------------------
##
## Counting/Probability R code (lecture 2)
##
## Class: PCE Data Science Methods Class
##
## Contains examples of:
##
## -Counting, Probability
##
## -More on Distributions
##
##--------------------------------------------

library(MASS) # Contains the function 'fractions()', 

##-----Sandwich Count----
breads = c('white', 'wheat', 'italian', 'sevengrain')
meats = c('ham', 'turkey', 'chicken', 'pastrami', 'meatballs')
toppings = c('mustard', 'mayo', 'salt_pepper', 'oil_vinegar')

sandwiches = expand.grid(breads,
                         meats,
                         toppings)
nrow(sandwiches)
sandwiches


##-----Two Dice Example ------
twoDice = expand.grid(1:6,1:6)
twoDice

twoDice$sum = twoDice$Var1 + twoDice$Var2
twoDice$isdouble = twoDice$Var1 == twoDice$Var2 ## == is logical equals
twoDice

# Count different sums
sumCounts = table(twoDice$sum)
sumCounts

# Count doubles
doubles = sum(twoDice$isdouble)
doubles

# Probability of a double:
fractions(doubles/nrow(twoDice))

# Probabilities of sums:
sumProb = fractions(table(twoDice$sum)/nrow(twoDice)) # type ?fractions for detail
barplot(sumProb)


