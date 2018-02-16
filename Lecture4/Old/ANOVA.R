##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
## ANOVA
##
##--------------------------------------------

##-----ANOVA Example-----
df = data.frame('group'=c(rep(1,50),
                          rep(2,50),
                          rep(3,60),
                          rep(4,40)),
                'val' = c(rnorm(50, mean=0, sd=1),
                          rnorm(50, mean=0, sd=1),
                          rnorm(60, mean=0.5, sd=1),
                          rnorm(40, mean=0, sd=1)))
df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)

## Boxplots of the groups
boxplot(df$val ~ df$group, main = 'Boxplot by group', xlab = 'Group')

## Compute, print the summary and plot the ANOVA model
df_aov = aov(val ~ group, data = df)
summary(df_aov)
plot(df_aov)

# we get statistics on the groups and total residuals:
# DF = degrees of freedom
# Sum Sq = sum of squares
# Mean Sq = Mean Squared Error
# F -value = our statistic based on a ratio of Mean Square Errors
# Pr(>F) = p-value for the NULL hypothesis that all groups are the same.

# BUT WHICH GROUP IS DIFFERENT? ANOVA does not tell us which.  We need 
#  to further test to find out.

tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
# This test is built on the distribution of testing two means (say mean1 and mean2),
#   and the statistic is given by (mean1 - mean2)/sd(both samples)
#   Know that this statistic has a known (albeit uncommon) distribution and this allows us
#   (or R) to create multiple hypotheses (pairwise tests).
plot(tukey_anova)

## Try more samples
df = data.frame('group'=c(rep(1,500),
                          rep(2,500),
                          rep(3,600),
                          rep(4,400)),
                'val' = c(rnorm(500, mean=0, sd=1),
                          rnorm(500, mean=0, sd=1),
                          rnorm(600, mean=0.5, sd=1),
                          rnorm(400, mean=0, sd=1)))
df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)

## Boxplots of the groups
boxplot(df$val ~ df$group, main = 'Boxplot by group', xlab = 'Group')

## Compute, print the summary and plot the ANOVA model
df_aov = aov(val ~ group, data = df)
summary(df_aov)
plot(df_aov)

## And the Tukey ANOVA
tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)


## Try greater effect size
df = data.frame('group'=c(rep(1,50),
                          rep(2,50),
                          rep(3,60),
                          rep(4,40)),
                'val' = c(rnorm(50, mean=0, sd=1),
                          rnorm(50, mean=0.5, sd=1),
                          rnorm(60, mean=1.0, sd=1),
                          rnorm(40, mean=0, sd=1)))
df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)

## Boxplots of the groups
boxplot(df$val ~ df$group, main = 'Boxplot by group', xlab = 'Group')

## Compute, print the summary and plot the ANOVA model
df_aov = aov(val ~ group, data = df)
summary(df_aov)
plot(df_aov)

## And the Tukey ANOVA
tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
plot(tukey_anova)


##------- Simple Applicaiton -------------
##
## 20 subjects are divided into three diet groups 
## plus a control group (no diet). There are 5 subjects
## in each group. Weight loss of each subject is recorded
## in units of pounds lossed. Are any of these diets 
## effective?
##
diet = data.frame(Loss = c(c(8,9,6,7,3),
                           c(2,4,3,5,1),
                           c(3,5,4,2,3),
                           c(2,2,-1,0,3)),
                  group = c(rep('LowCalorie', 5),
                            rep('LowFat', 5),
                            rep('LowCarbohydrate', 5),
                            rep('Control', 5)))
diet$group = factor(diet$group) # Make the group column a factor
str(diet)
require(dplyr)
diet %>% group_by(group) %>% 
  summarise(subjects = n(), mean = mean(Loss), median = median(Loss), sd = sd(Loss))

## Visualize the data
require(ggplot2)
ggplot(diet, aes(group, Loss)) + geom_point(size = 5, alpha = 0.3) +
  ggtitle('Weight loss by diet for 20 subjects') +
  xlab('Diet') + ylab('Weight loss')

## Create and summarize an ANOVA model
diet.anova = aov(Loss ~ group, data = diet)
summary(diet.anova)
plot(diet.anova)

## Create and summarize a Tukey Honest Significant Difference model
tukey.diet = TukeyHSD(diet.anova)  # Tukey's Range test:
tukey.diet
plot(tukey.diet)
