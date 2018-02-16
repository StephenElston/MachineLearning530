##
## Simulation Example
##
## Class: PCE 350, Data Science Methods
##
## Creator: Steve Elston
##
## Simulation of profits for sandwich shop.
## Shop has 3 types of bread.
## Selling a sandwich = $1 profit
## Out of bread, customer leaves = $0 profit
## Each wasted bread = -$0.20 profit
##
## WARNING. This code has a known bug. It will crash
## for certain combinations of simulation arguments. 

sim.demand <- function(n){
  bread <- runif(n) # Probabilities of bread choice 
  ifelse(bread <= 0.5, 'white', 
         ifelse(bread <= 0.75, 'wheat', 'multi'))
}

demand <- sim.demand(100)
table(demand)


sim.profit <- function(n = 100, sd = 30, bake = 120, test = FALSE){
  # number of bread by type
  baked <- c(rep('white', times = bake/2), 
             rep('wheat', times = bake/4),
             rep('multi', times = bake/4))
  baked <- as.data.frame(table(baked))
  names(baked) <- c('bread', 'baked')
  
  arrivals <- 0
  while(arrivals < 1) arrivals <- rnorm(1, mean = n, sd = sd)
  demand <- sim.demand(arrivals) # demand by bread type
  baked$demand <- as.data.frame(table(demand))[,2]
  baked$shortfall <- baked$demand - baked$baked
  baked$profit <- baked$demand - 
                    ifelse(baked$shortfall < 0, 0.2 * -baked$shortfall,
                           baked$shortfall )

  if(test) print(baked)
  data.frame(profit = sum(baked$profit), missed = sum(baked$shortfall))
}

sim.profit(test = TRUE)

plot.profit <- function(df, bins = 50){
  require(ggplot2)
  require(gridExtra)
  bw <- (max(df$profit) - min(df$profit))/(bins - 1)
  h1 <- ggplot(df, aes(profit)) + geom_histogram(binwidth = bw) +
    ggtitle('Distributions of profits') + xlab('Profits')
  bw <- (max(df$missed) - min(df$missed))/(bins - 1)
  h2 <- ggplot(df, aes(missed)) + geom_histogram(binwidth = bw) +
    ggtitle('Distributions of people missed') + xlab('Missed')
  grid.arrange(h1, h2, nrow = 1)
}


dist.profit <- function(reps = 1000, n = 100, sd = 20, bake = 120){
  dist <- data.frame(profit = rep(0, times = reps),
                     missed = rep(0, times = reps))
  for(i in 1:reps){
    dist[i, ] <- sim.profit(n = n, sd = sd, bake = bake)
  }
  plot.profit(dist)
  
  data.frame(MeanProfits = round(mean(dist$profit), 0),
    stdProfits = round(sqrt(var(dist$profit)), 0),
    MeanMissed = round(mean(dist$missed), 0),
    stdMissed = round(sqrt(var(dist$missed)), 0) )
}


## How long to simulations take?
system.time(
  dist.profit()
  )

## Or, get a more detailed, but slower much slower, view
library(microbenchmark)
microbenchmark(
    dist.profit(),
    times = 20
  )


## Test a function
test.demand <- function(){
  set.seed(2345)
  res <- c("white", "white", "wheat", "white", "white", "white", 
           "wheat", "multi", "white", "wheat", "white", "white", 
           "white", "white", "white", "white", "wheat", "white",
           "wheat", "white")
  demand  <- sim.demand(20)
  if(!any(demand != res)) print('sim.demand funciton works!')
  else print('ERROR: sim.demand failed')
}

