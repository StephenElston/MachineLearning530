##--------------------------------------------
##
## R code review (lecture 1)
##
## Class: PCE Data Science Methods Class
##
## Contains examples of:
##
## - Matrices, Dataframe, dplyr, apply functions.
## - Summary statistics
##
##--------------------------------------------

##----Import Libraries----
#library(logging)

##----Set working directory-----
setwd('E:/Work/Teaching/PCE_Data_Science/1_Intro_Lecture')

##----Matrix Review-----
A_matrix = matrix(4, nrow=4, ncol=3) # Makes use of broadcasting
A_matrix
B_matrix = matrix(1:12, nrow=4, ncol=3)
B_matrix

A_matrix + B_matrix # Elementwise
A_matrix * B_matrix # Elementwise
A_matrix %*% B_matrix # Error in matrix multiplication
A_matrix %*% t(B_matrix)

##----Dataframe Review----
x_values = seq(from=as.Date('2015-01-01'),
               to=as.Date('2015-02-12'),
               by = 3)

df = data.frame('dates' = x_values,
                'x1'    = runif(15,-10,20),
                'x2'    = 1:15,
                'x3'    = strsplit('MississippiMath','')[[1]])

## Functions applied to a column of a data frame
df$x3 = as.character(df$x3)
df$x3 = tolower(df$x3)

## Examine data frames
str(df)
head(df)
tail(df, n=10)

# Subset data frames
str(df[1:10,])
str(df[ ,c('dates', 'x1')])


## Functional Programming with R
##----Apply Function Review-----
# Please read the first SO answer of this question:
# http://stackoverflow.com/questions/3505701/r-grouping-functions-sapply-vs-lapply-vs-apply-vs-tapply-vs-by-vs-aggrega

# apply is basic functional operator
# matrix example
class(B_matrix)
attributes(B_matrix)
typeof(B_matrix)
dim(B_matrix)
res1 = apply(B_matrix, 1, median) # Across rows (2 = across columns)
res1
class(res1)
attributes(res1)
typeof(res1)
length(res1)

# lapply is basic functional operator for lists
# data frame example using annonomous function
res = lapply(df, function(x) if(is.numeric(x)) mean(x) )
res
class(res) 
class(res[[2]]) # class of element 2 of the list
u.res = unlist(res) # change the class
u.res 
u.res['x1'] # index single element of the vector
u.res[1] # the first index of the vector
res[[2]][1] # the same value from the list


# Try this with an expression
exprs = quote(if(is.numeric(x)) mean(x))
exprs
lapply(df, function(x) exprs)

# Or a named function
mean.x = function(x) exprs
lapply(df, mean.x)



### --- Summary statistics
##  Read the csv file into a data frame
read.auto <- function(path = 'SET-YOUR-PATH-HERE'){
  ## Function to read the csv file
  filePath <- file.path(path, 'Automobile price data _Raw_.csv')
  auto.price <- read.csv(filePath, header = TRUE, 
                         stringsAsFactors = TRUE)
  
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

str(Auto.Price) # Look inside data frame
summary(Auto.Price) # Some basic statistics

# Which columns have missing values
lapply(Auto.Price, function(x) any(is.na(x)))

# compute the covariance 
numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm',
             'city.mpg', 'compression.ratio', 'engine.size', 
             'curb.weight', 'length')
cov.auto = cov(Auto.Price[numcols], na.rm = TRUE)
cov.auto


# compute correlation
cor.auto = cor(Auto.Price[numcols])
cor.auto


