##----Missing Data Demo -----
## Class: PCE Data Science Methods Class
## Lecture 2
#
# NA vs. NaN vs. NULL
#
# NA: Not Available, results from missing data or an
#       out of bounds reference (Logical, niether T or F)
#
# NaN:  Not A Number: results from performing an illegal
#         mathematical action. (Numeric placeholder)
#
# NULL: This is operational.  R returns this when referencing
#         non-existent columns.  R also uses a NULL assignment
#         to remove objects. (NULL Class).
#       If you've had set theory before, think of this as the
#         'empty-set'
#

# NA vs. NaN vs. NULL
class(NA)
class(NaN)
class(NULL)

# NaNs:
0/0
sqrt(-1)
log(-1)
asin(2)
NaN > 1 

## Note that na.rm is quite general, NaN is not a type of NA
sum(c(1,2,NaN))
sum(c(1,2,NaN), na.rm=TRUE)  

#NAs:
c(1,2)[3]          # third argument missing
as.numeric('data') # R is 'missing' the number available
NA > 1
sum(c(1,2,NA))
sum(c(1,2,NA), na.rm=TRUE)

# NULLs:
t = data.frame('a'=1:4, 'b'=runif(4))
t
t$c
t$a = NULL
t
s = c(1,2,NULL)
s


# To illustrate the helpfulness of Multiple Imputation,
#   we will test this out on a contrived data set
library(Amelia)

n = 1000
full_data = data.frame('A'=runif(n),
                       'B'=rnorm(n),
                       'C'=rpois(n, 5))

# true mean of A = 0.5
# true mean of B = 0
# true mean of C = 5
sample_means = apply(full_data, 2, mean)
sample_means
sample_sds = apply(full_data, 2, sd)
sample_sds

# Remove some data:
data = full_data
data$A[sample(1:n, round(n*0.05))] = NA
data$B[sample(1:n, round(n*0.15))] = NA
data$C[sample(1:n, round(n*0.5))] = NA

# Removal of missing data (by entry only)
#  This is only works because our statistic is calculated
#          on each row separately.
means_rem_entry = apply(data, 2, function(x) mean(x, na.rm=TRUE))
means_rem_entry
sd_rem_entry = apply(data, 2, function(x) sd(x, na.rm=TRUE))
sd_rem_entry

## Removal of missing data (by row)
data2 = data[complete.cases(data),]
means_rem_rows = apply(data2, 2, mean)
means_rem_rows
sd_rem_rows = apply(data2, 2, sd)
sd_rem_rows


## Substitue a value
data3 = data.frame(lapply(data, function(x) {x[which(is.na(x))] = 0; x}))
means_subs = apply(data3, 2, mean)
means_subs
sd_subs = apply(data3, 2, sd)
sd_subs


## Now try impuation with amelia
library(Amelia)
amelia_data = amelia(data)[1]$imputations # Amelia spits out WAY too much information.

# Calculate samples means
imp_means = lapply(amelia_data, function(x) apply(x,2,function(y) mean(y, na.rm=TRUE)))
imp_means
avg_imp_means = apply( do.call(rbind, imp_means), 2, function(y) mean(y, na.rm=TRUE))
avg_imp_means

# Calculate samples sds
imp_sds = lapply(amelia_data, function(x) apply(x,2,function(y) sd(y, na.rm=TRUE)))
imp_sds
avg_imp_sds = apply(do.call(rbind, imp_means), 2,function(y) sd(y, na.rm=TRUE))
avg_imp_sds
