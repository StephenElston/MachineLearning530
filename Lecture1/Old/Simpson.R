##--------------------------------------------
##
## Exploring Data (lecture 1)
##
## Class: PCE Data Science 350
##
## Contains examples of:
##
## - Simpson's Paradox
##   Data from June 1991
##   based on example from Tom Moore, Grinnell College
##   http://www.math.grinnell.edu/~mooret/reports/SimpsonExamples.pdf
##
##--------------------------------------------

## Read the csv file
Airlines = read.csv('Airline.csv', header = TRUE) 

## Create a frequency table by airline
## Which airline is 'better'
require(dplyr)
airline.summary = Airlines %>% group_by(Airline) %>% 
            summarize(PrecentDelay = 100 * sum(Delayed)/(sum(OnTime) + sum(Delayed)))
airline.summary

## Create a frequency table by airport
## How do delays change with airport
airport.summary = Airlines %>% group_by(AirportCode) %>% 
  summarize(PrecentDelay = 100 * sum(Delayed)/(sum(OnTime) + sum(Delayed))) %>%
  arrange(PrecentDelay)
airport.summary
 
## Create a summary table by airport and airline
airport.by.airline = Airlines %>% group_by(AirportCode, Airline) %>% 
  summarize(TotalFlights = sum(OnTime) + sum(Delayed),
            PrecentDelay = 100 * sum(Delayed)/(sum(OnTime) + sum(Delayed))) %>%
            arrange(AirportCode)
airport.by.airline


## Look at the full table with delay added
Airlines = Airlines %>% group_by(AirportCode, Airline) %>%
  mutate(PercentDelay = 100 * sum(Delayed)/(sum(OnTime) + sum(Delayed)))
Airlines
