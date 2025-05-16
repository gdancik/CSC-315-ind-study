################################################################
# Contrasts in linear models, and the relationship between 
# the t-test and linear regression
################################################################

library(ggplot2)
library(dplyr)

## We will compare the GPAs of drinkers to non-drinkers from a previous class ##
survey <- read.delim("http://pastebin.com/raw/QDSga7qF")
s <- split(survey$College.GPA, survey$Alcohol > 0)
s

nondrinkers <- s$`FALSE` ## corresponds to survey$Alcohol > 0 being FALSE
drinkers <- s$`TRUE` ## corresponds to survey$Alcohol > 0 being TRUE

## Test for difference in means, using two-sample t-test. 
## By default, this performs the "Welch two-sample t-test", 
## which assumes unequal (different) variances (or standard deviations) 
## for each group
t.test(nondrinkers, drinkers) 

## we can assume equal variances by setting the var.equal argument to TRUE
result <- t.test(nondrinkers, drinkers, var.equal = TRUE) 
result

##############################################################
# Let's formulate this comparison in terms of a linear model 
# We want to predict GPA based on drinking status
# The explanatory variable (drinker) is
#    0 - nondrinker
#    1 - drinker
##############################################################

# look at values
drinker <- as.integer(survey$Alcohol > 0)
drinker

# add values to data frame
df <- mutate(survey, drinker = as.integer(Alcohol > 0))

# confirm that drinkers = 1, non-drinkers = 0
select(df, Alcohol, drinker) %>% View()

# generate scatterplot with fitted regression line
ggplot(df, aes(drinker, College.GPA)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  theme_classic() + 
  xlab("drinker (0 = No, 1 = Yes)") +
  ggtitle("Relationship between College GPA and drinking status")

# fit the linear model
fit1 <- lm(College.GPA ~ drinker, data = survey)


#################################################
# Now compare p-values of t-test to the p-values
# from the linear model (y = a + bx) which tests 
# against H0: b = 0 vs. HA: b != 0
#################################################
result$p.value
summary(fit1)

#########################################################
## These analyses are the same! 
## A two-sample t-test with equal variances gives the 
## same p-value as a linear model where the two groups
## are coded as x = 0 and x = 1

## What is the interpretation of the slope of this 
## linear model?

# The slope is the difference in mean GPAs between
# nondrinkers and drinkers. On average, the GPA
# of drinkers is ~0.13 less than the GPA of 
# non-drinkers
########################################################


# compare difference in means
diff(result$estimate) # difference in estimates from t-test
fit1$coefficients[2]  # slope of linear model

######################################################
# In the above analysis, the slope of the linear 
# model is equal to the difference in means between 
# the two groups
#######################################################

###########################################################
## There are other ways of coding the explanatory
## variable for this problem (where drinkers are assigned 
## one numeric value and non-drinker are assigned another). 
## In the above code, we used 0 for non-drinkers and 1 
## for drinkers. The 'contrast' type determines the values
## that are used for each group
###########################################################

# we can also treat the explanatory variable as a factor
drinking.status <- factor(survey$Alcohol > 0)
drinking.status  # levels are FALSE/TRUE

# change levels to be informative (FALSE = nondrinker,
# TRUE = drinker)
levels(drinking.status) <- c("NonDrinker", "Drinker")
drinking.status

# get the groups (levels)
groups <- levels(drinking.status)
groups

##########################################################
# Treatment contrasts: explanatory variable is 0 for the
# reference (first) group and 1 for the second group 
##########################################################

# The 'contr.treatment' function retruns a matrix of 
#  contrasts, using the "treatment contrast" by default.
#   In this case, the first level of the factor is a reference value, 
#   and additional levels are contrasted with the reference
#   In this case, we have "NonDrinker (reference) = 0, Drinker = 1
contr.treatment(groups)

# Note that the explanatory variable (drinking.status) is a 
# factor; we can specify how to define the contrasts by setting
# the 'contrasts' argument to 'lm'
fit.treatment <- lm(survey$College.GPA ~ drinking.status,
                    contrasts = list(drinking.status = "contr.treatment"))

summary(fit.treatment)

## the slope is equal to the difference in means
coefficients(fit.treatment)[2]

# Note, treatment contrast is the default, so we get
# the same result if we use the following and do not 
# explicitly set the contrast:

fit.treatment <- lm(survey$College.GPA ~ drinking.status)
summary(fit.treatment)

######################################################
# Sum contrasts: explanatory variable is -1 for the 
# first group and +1 for the second group (the values 
# will sum to 0)
######################################################

# NonDrinker (1st level) = 1, Drinker (2nd level) = -1
contr.sum(groups)

fit.sum <- lm(survey$College.GPA ~ drinking.status,
                   contrasts = list(drinking.status = "contr.sum"))

# What does the slope represent? And why? 
coefficients(fit.sum)[2]

# Note that the p-value for the slope is the same as before
summary(fit.sum)

###################################################
# Indicator variables: use an indicator for each
# group (0 = not in group, 1 = is in group)
# Note that this model has 2 'slope' variables 
# and no 'intercept', i.e., y = b1x1 + b2x2, 
# where b1 is the mean for group 1, and b2 is 
# the mean for group 2
###################################################

# in lm function, the -1 says to remove the intercept term
fit.indicator <- lm(survey$College.GPA ~ -1 + drinking.status)

# get the means for each group
coefficients(fit.indicator)

# summarize the results
summary(fit.indicator)

#####################################################
# The p-values for each coefficient (b) test against
# H0: b = 0, but this is not of interest to us.
# The hypothesis test of interest is whether 
# b1-b2 = 0, which can be calculated manually or by 
# using additional packages (we will not worry about
# this for now). 
#####################################################

#####################################################
# This last coding of the explanatory variable in a 
# linear model is what we will use to identify 
# differentially expressed genes using limma
#####################################################

