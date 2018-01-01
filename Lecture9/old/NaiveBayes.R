##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Introduction to Naive Bayes ----
##
## Load the data
require(mlbench)
data(HouseVotes84)
head(HouseVotes84)

## Examine the data ste
require(ggplot2)
Map(function(x, y)
  ggplot(HouseVotes84, aes_string(x)) +
    geom_bar() +
    facet_grid(. ~ Class) +
    ggtitle(y),
  list('V1', 'V2', 'V3', 'V4', 'V5'), 
  list(' handicapped-infants',
        'water-project-cost-sharing',
        'adoption-of-the-budget-resolution',
        'physician-fee-freeze',
        'el-salvador-aid'))

## Compute the model
require(e1071)
model <- naiveBayes(Class ~ ., data = HouseVotes84)

## Look at the results for the first 10 legistators
party = predict(model, HouseVotes84[1:10,])
nums = predict(model, HouseVotes84[1:10,], type = "raw")
data.frame(party = HouseVotes84$Class[1:10], predicted = party, Democrat = nums[,1], Republican = nums[,2])

## Look at the accuracy of the model
pred <- predict(model, HouseVotes84)
table(pred, HouseVotes84$Class)

## using laplace smoothing:
model <- naiveBayes(Class ~ ., data = HouseVotes84, laplace = 3)
pred <- predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)

## ---------------------------------------
## ------ learning in naive Bayes ------
## Naive Bayes works well with small data sets
for(i in c(3, 5, 9, 17)){
  model <- naiveBayes(Class ~ ., data = HouseVotes84[, 1:i], laplace = 3)
  pred <- predict(model, HouseVotes84[,2:i])
  print(table(pred, HouseVotes84$Class))
}



## -------- Census Data ----------------------
## Want to classify people into high/low income
##
## Load the dataset
path = 'C:\\Users\\StevePC2\\Documents\\Git\\DataScience350\\Lecture9' # SET YOUR PATH HERE!!
full.path = file.path(path, 'Adult Census Income Binary Classification dataset.csv')
Income = read.csv(full.path,
                  header = TRUE)

require(e1071)
income.mod = naiveBayes(income ~ ., data = Income, laplace = 3)
income.mod

pred <- predict(income.mod, Income[,-15])
table(pred, Income$income)


for(i in c(2000, 8000, 32561)){
  income.mod = naiveBayes(income ~ ., data = Income[1:i, ], laplace = 2)
  pred <- predict(income.mod, Income[1:i,-15])
  print(table(pred, Income$income[1:i]))
}

