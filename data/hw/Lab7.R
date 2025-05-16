####################################################################
# Name:
# Lab 7: Hypothesis testing for population means.

# For these questions, we will assume that the Central 
# Limit Theorem applies (i.e., that the populations are normally 
# distributed or that 'n' is sufficiently large), and that the 
# samples are representative of the population of interest. 

# Turn in a Notebook that answers the questions below
####################################################################

library(readr)
library(ggplot2)
library(dplyr)

# Read in our survey data
survey <- read_csv('https://gdancik.github.io/CSC-315/data/datasets/csc315_survey_fall2023.csv')

##########################################################################
# 1) Assume that the mean amount of sleep an adult gets is 8 hours 
#    per night. Is there evidence that college students  
#    get a different amount of sleep? (Here we assume that our 
#    survey results are representative of all college students).

# (a) State the null and alternative hypotheses (done for you):

# H0: mu_sleep = 8
# HA: mu_sleep != 8

# where mu_sleep is the mean amount of sleep a CSC 315 student gets
# per night.

# (b) Calculate / find the test statistic (and specify the degrees of freedom)

# (c) Find the p-value using the t.test function

# (d) Find the p-value 'manually' based on the test statistic and
#     appropriate degrees of freedom 

# (e) State the conclusion regarding the null and alternative hypotheses in 
#     the context of this problem.
##########################################################################

##########################################################################
# 2) Is there evidence that the amount of sleep that a 'cat' person gets
#    differs from that of a 'dog' person?

# (a) State the null and alternative hypotheses (done for you):

#   H0: mu_cat - mu_dog = 0
#   HA: mu_cat - mu_dog != 0,

#   where mu_cat and mu_dog are the mean hours of sleep for students preferring
#   cats and dogs, respectively

# (b) Create side-by-side boxplots (using ggplot) showing hours of sleep for 'cat' 
#     and 'dog' people. Make sure to label the y-axis and give the chart a title.

# (c) We will now formally test the hypotheses that mean amount of sleep
#     is different between 'cat' and 'dog' people. The command 
#     t.test(x,y) will perform a two-sample t-test for the null 
#     hypothesis that the 'x' and 'y' populations have the same mean, where 'x' 
#     is a vector of observations from the first population and 'y' is a vector of
#     observations from the second population.
#     Use the t.test function to find the test statistic and the 
#     corresponding degrees of freedom. Note that in your call to t.test,
#     'x' is a vector of hours of sleep for 'cat' people and 'y' is a 
#     vector of hours of sleep for 'dog' people, which you can get using the 'split'
#     function.

# (d) Find the p-value from the result of the t.test function

# (e) Find the p-value 'manually' based on the test statistic and
#     appropriate degrees of freedom from the t.test result (which 
#     is stored in the $parameter object)

# (f) State the conclusion regarding the null and alternative hypotheses in 
#     the context of this problem.

# (g) What would it mean in the context of this problem if a Type I 
#     error occurred?
##########################################################################

##########################################################################
# 3:  Find the p-values associated with the following t test 
# statistics, for a one-sample t-test, and state whether you would reject 
# or fail to reject the null hypothesis at alpha = 0.05:

# (a) t = 2.78, n = 45

# (b) t = -3.3, n = 51

# (c) t = 1.11, n = 100
##########################################################################

# use the cereal data to complete the last question
cereal <- read.delim("http://pastebin.com/raw/0G6DrHyC")


#################################################################################
# 4)  The 'sugars' column contains the sugar content (in grams), while
#     the 'shelf' column contains the shelf in which the cereal is 
#     shelved on, with 1 = lower shelf, 2 = middle shelf (which is at
#     eye level for children), and 3 = top shelf. The code below constructs
#     a boxplot comparing sugar content across only the lower and middle 
#     shelves
#################################################################################

# remove data from the top shelf (Note: make sure dplyr is loaded
#     before running the next statement)
cereal <- filter(cereal, shelf != 3)

# change shelf column to factor
cereal$shelf <- factor(cereal$shelf)
levels(cereal$shelf) <- c("lower", "middle")

# generate boxplot
ggplot(cereal) + geom_boxplot(aes(shelf, sugars, fill = shelf)) +
  theme_classic() + theme(legend.position = "none") +
  ggtitle("Sugar content in cereals by shelf level") + 
  labs(x = "shelf level", y = "sugar content (grams)")

# Now let's formally test whether mean sugar content differs
# between the lower shelf and the middle shelf.

# (a) State the null and alternative hypotheses, making sure
#     to define the 'mu' parameters

# (b) Use the t.test function to find the test statistic 
#     and corresponding degrees of freedom

# (c) Find the p-value

# (d) State the conclusion regarding the null and alternative hypotheses in 
#     the context of this problem.
#################################################################################

