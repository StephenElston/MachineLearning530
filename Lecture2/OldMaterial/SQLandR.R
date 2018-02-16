##--------------------------------------------
##
## Using SQL from R
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------
getwd()
#setwd('C:/Users/Steve/Dropbox/UW/DataSci350/Lecture 2')

##-----Getting/Storing Data-----

# txt files
?read.table

# csv files. Is wraper on read.table
?read.csv # Note the option stringsAsFactors = FALSE

# web/html
?readLines

## Example: get a data frame
read.auto <- function(path = '.'){
  require(stringr)
  ## Function to read the csv file
  filePath <- file.path(path, 'Automobile price data _Raw_.csv')
  auto.price <- read.csv(filePath, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  auto.price
}

## Read the csv file
## Note that SQL databases don't like '.' characters in column names
Auto.Price = read.auto(path = 'C:/Users/Steve/GIT/DataScience350/Lecture1') ## Read the csv file
nams <- names(Auto.Price)
names(Auto.Price) <- gsub('\\.', '_', nams) ## replace '.' with '_'


##-----SQLite Access-----
## Set up the connection to the database
library(RSQLite)
# Name of the db
db.name = 'auto_db'

# Create the connection
db_conn = dbConnect(dbDriver("SQLite"), db_name)

# Write dataframe to a table
dbWriteTable(db_conn,"Auto_Price", Auto.Price, overwrite = TRUE)

# Simple query database
query = 'SELECT * FROM Auto_Price LIMIT 5;'
test = dbSendQuery(db_conn, query)
fetch(test)

## Query to find turbo cars. Note the excape required
## arround the '. 
query = 'SELECT * FROM Auto_Price WHERE aspiration = \'turbo\';'
turbo.q = dbSendQuery(db_conn, query)
turbo = fetch(high.milage.q)

## Query to find high milage cars
query = 'SELECT * FROM Auto_Price WHERE city_mpg > 24;'
high.milage.q = dbSendQuery(db_conn, query)
high.milage = fetch(high.milage.q)

## make a plot of the high milage subset
require(ggplot2)
ggplot(high.milage, aes(city_mpg, price)) + 
  geom_point(aes(size = 2, color = factor(fuel_type))) +
  ggtitle('Price vs. city mpg for high milage autos')
  
# Disconnect, because we have clean R code.
dbDisconnect(db_conn)


## Example: Use an existing database
## Create the database from the 

con = dbConnect(dbDriver("SQLite"), dbname = 'nyc_flights/nycflights13.sqlite')
alltables = dbListTables(con)
alltables

## Look at a few tables
query = 'SELECT * FROM flights LIMIT 5;'
test = dbSendQuery(con, query)
fetch(test)

query = 'SELECT * FROM airports LIMIT 5;'
test = dbSendQuery(con, query)
fetch(test)


##----Try/Catch Pattern ----
#
#  The tryCatch is used to create robust, produciton quality, R code
#  It should be used a lot more. This pattern shows how to use it:
#
#  result = tryCatch({
#                              your code
#                     },
#                     error=function(error_condition){
#                               message('Your error message here')
#                               message(error_condition)
#                     },
#                     warning=function(warning_condition){
#                               message('Your warning message here')
#                               message(warning_condition)
#                     },
#                     finally={
#                               Always execute these commands to cleanup.
#                     }
#                   )
#


execute.query <- function(query, db = 'nyc_flights/nycflights13.sqlite'){
  stopifnot(is.character(query))
  if(!file.exists(db)) stop('ERROR, database not found')
  tryCatch({
                    con = dbConnect(dbDriver("SQLite"), dbname = 'nyc_flights/nycflights13.sqlite')
                    message('Opened database connection')
                    dbGetQuery(con, query)
                  },
         error = function(error_condition){
                    message('ERROR: The query has failed')
                    message(error_condition)
                 },
         warning = function(warning_condition){
                     message('WARRNING: warrning conditon for query')
                     message(error_condition) 
                 },
         finally = {
                     dbDisconnect(con)
                     message('\nDatabase connection closed')
                  }
    )
}

## Test the function
query = 'SELECT * FROM flights LIMIT 5;'
execute.query(query)

## A query that fails
query = 'SELECT * FROM no_table LIMIT 5;'
execute.query(query)


