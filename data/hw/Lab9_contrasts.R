##############################################################
# Name:
# CSC-315
# Lab #9: Contrasts in Linear Models
#############################################################

##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps below and to explicitly answer the following questions
##########################################################################

library(ggplot2)
library(dplyr)

# 1) The code below reads in our class survey data and performs a 
#   2-sample t-test to evaluate whether there is a statistically
#   significant difference in the hours of sleep between 
#   'Cat' and 'Dog' people. Based on the code below, 

library(readr)
survey <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/csc315_survey_fall2023.csv")
s <- split(survey$sleep, survey$CatOrDogPerson)
res <- t.test(s$Cat, s$Dog, var.equal = TRUE)

#   (a) find the p-value and state your conclusion regarding the null 
#       hypothesis of H0: mu_cat - mu_dog = 0


#   (b) calculate the difference in mean hours of sleep between
#   groups, using the formula: 
#     mean hours of sleep for dog people - mean hours of sleep for cat people


# 2) Fit a linear model that predicts Hours of Sleep based on 
#   whether an individual is a cat or a dog person. You should use
#   the treatment contrast where 'cat person' is the reference (x = 0) and 
#   'dog person' is the treatment (x = +1)

  
# (a) Find and interpret the y-intercept of the regression line in the
#      context of this problem.


# (b) Find and interpret the slope of the regression line in the context of 
#     this problem


# (c) What is the p-value for the hypothesis test that there is a
#     significant difference in Hours of Sleep between the two groups?
#     (show this result in R, based on the linear model). Note: the 
#     p-value from the linear model should match the p-value from the
#     two-sample t-test from problem 1(a) above.

# 3) We can also fit a linear model using the 'sum' contrasts, which 
#    is done below. This model has the form:
#
#    y = a + bx, where x = 1 for a Cat person and -1 for a Dog person

fit <- lm(sleep ~ CatOrDogPerson, data = survey, 
          contrasts = list(CatOrDogPerson = 'contr.sum'))

#  a) Show that the y-intercept is the average (mean) of the 
#     the group means, i.e.,the y-intercept is equal to 
#     [mean(sleep_cat_person) + mean(sleep_dog_person)] / 2
#     by calculating and displaying this value, and also 
#     displaying the y-intercept.

#  b) Find and interpret the slope of the regression line in the 
#     context of this problem


# 4) The code below fits a linear model using indicator variables, and
#    then displays the coefficients

fit <- lm(sleep ~ -1 + CatOrDogPerson, data = survey)
fit$coefficients

# (a) What does the coefficient 'CatOrDogPersonCat' represent?


# (b) What does the coefficient 'CatOrDogPersonDog' represent?


