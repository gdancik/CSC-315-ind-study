##########################################################################
# CSC 315, Exam II
# Name: 
##########################################################################

##########################################################################
# Add R code to the script below and create a Notebook to explicitly
# answer the following questions. Your Notebook should include output 
# showing the requested results, and written answers to 
# questions should be provided in comments. When you are finished, 
# submit an HTML Notebook through Blackboard as described previously.
#
# Note: DO NOT delete / modify any of the questions / comments below!
##########################################################################

library(dplyr)
library(ggplot2)
library(gtools)

# 1) The code below defines a function that randomly simulates drawing 2 cards 
#    from a 52 card deck. The function returns TRUE if the two cards have the 
#    same point value. Use the function to find the empirical probability of
#    drawing 2 cards with the same value, by calling the function 5000 times.
#    What is the probability of being dealt two cards with the same point value?
#    [8 points]

draw_2_same <- function() {
  deck <- rep(1:13,4)
  s <- sample(deck, 2)
  s[1] == s[2]
}


# 2) The code below generates all possible 2 card hands. Use R to show that the number of
# possible 2 card hands is 1,326. [4 points]

deck <- rep(1:13,4)
hands <- combinations(52, 2, deck, repeats.allowed = FALSE, set = FALSE)


# 2) Find the theoretical / classical probability of being dealt 2 cards with the 
#    same value, using the 'hands' matrix. [8 points].


# 3) Find the theoretical / classical probability of being dealt at least one Ace 
#    (which has a point value of 1). [8 points]


# 4) The function below takes a vector of card values, and finds the total blackjack score,
#    where face cards (11-13) are worth 10 points and aces (1) are worth 11 points. Use
#    the function to first find the score of each possible hand, and then to calculate the 
#    probability of being dealt a blackjack (a pair of cards worth 21 points). [8 points]

calc_score <- function(myhand) {
  myhand[myhand > 10] <- 10  # face cards are worth 10 points
  myhand[myhand == 1] <- 11  # aces are worth 11 points
  sum(myhand) # find the point total
}


# 5) Normal probability calculations. Write the correct R statement to calculate and display
#    the probability [3 points each = 12 points]  
#
#      (a) Suppose that X ~ N(60,5). Find P(X > 64)   
          
          
#      (b) What is the probability that an observation from the standard normal distribution
#          is greater than 1.24? 
          
          
#      (c) In a population that is normally distributed, calculate the probability 
#          that a randomly selected observation is more than 2 standard deviations
#          BELOW the mean. 
          
          
#      (d) Suppose that X ~ N(40,2) and that 15 individuals are sampled. 
#          Find the probability that the sample mean is at least 41. 
          
          
          
# 6) In 2013, the proportion of adults who smoke in the U.S. was 0.18. 
#    A 2019 study involving 1000 adults found that 141 of them smoked.
#    Is there evidence that the smoking rate has changed? [16 points]

# (a) State the null and alternative hypotheses, making sure to define 'p'

          
# (b) Use the prop.test function to conduct the hypothesis test WITHOUT
#     the continuity correction. Calculate the z test statistic from the 
#     prop.test object and extract the p-value 
          
          
# (c) Calculate the z test statistic using the appropriate formula, and find the p-value 
#     based on this test statistic. Note: these should match your answers from part (b).
          
          
# (d) The correct p-value is approximately 0.00133. Based on this p-value, state the 
#     conclusion regarding the null and alternative hypotheses in the context of this 
#     problem.

          
# 7) According to data from the CDC, for individuals 65 and over, 1968 out of 100,000 have been 
#    hospitalized with COVID-19; for individuals 18-49, 1050 out of 100,000 have been 
#    hospitalized (as of 10/28/2021, source: https://gis.cdc.gov/grasp/COVIDNet/COVID19_3.html).
#    Is there an association between age and COVID-19 hospitalization based on this data? 
#    [12 points]

# (a) State the null and alternative hypotheses, making sure
#     to define the 'p' parameters
          
# (b) Use the prop.test function to find the test statistic and p-value
          
# (c) Your p-value should be very close to 0 (1.758227e-63). Based on this p-value, 
#     state the conclusion regarding the null and alternative hypotheses in the context of 
#     this problem.
          
          
# 8) For this problem we will compare two different insecticides
#    (sprays) for controlling insects, using the built-in data set
#    'Insecticides'. The 'spray' column contains the spray used, and
#    the 'count' column contains the number of insects in an 
#    experimental area after the spray was used. The code below
#    filters the data to contain only results from spray 'C' and 'D'. [12 points]
          
      sprays <- filter(InsectSprays, spray %in% c("C", "D")) %>%
        mutate(spray = as.character(spray))
          
          
#     Our interest is in testing the following hypotheses:
          
#     H0: mu_C - mu_D = 0
#     HA: mu_C - mu_D != 0,
          
#     where mu_C is the mean number of insects in an area sprayed
#     with spray 'C', and mu_D is the mean number of insects in
#     an area sprayed with spray 'D'.
          
#   (a) Create side-by-side boxplots (using ggplot) showing the
#       number of insects for each spray. Make sure to label the 
#       axes and give the chart a title.
          
#   (b) Use the t-test function and display the test statistic, 
#       the degrees of freedom, and p-value. (Note: the sample
#       sample size is small, so we need to assume that the
#       the data is normally distributed for the t-test to be valid, 
#       which we will do here)
      
#   (c) The p-value should be 0.00573. Given this p-value, state
#        the conclusion regarding the null and alternative hypotheses in 
#        the context of this problem.
      
          
# 9) Consider the following Z test statistics or one sample t-test
#   statistics for the hypothesis tests discussed in class. Find the
#   p-value, and state whether you would REJECT the null hypothesis, 
#   or FAIL TO REJECT the null hypothesis based on the p-value. [12 points]

# (a) Z = 1.29

# (b) Z = -2.34 (additional question: why would Z be equal to 0)? 

# (c) t = 2.61, n = 45

# (d) t = -0.29, n = 90


# For questions (9) - (10), state the following:

# (a) What would it mean in the context of this problem if a Type I error occurred?

# (b) What would it mean in the context of this problem if a Type II error occurred?


# 10) A study is conducted to determine whether the proportion of females in
#    the United States differs from 50%. The null and alternative hypotheses are
#    as follows:  [4 points]

#    H0: p = 0.50
#    HA: p != 0.50, where p = the proportion of females in the United States

# 11) A study is conducted to determine whether or not the average age of an adult
#     male in the U.S. is different than the average age of an adult female. The
#     null and alternative hypotheses are as follows:  [4 points]

#    H0: mu_male - mu_female = 0
#    HA: mu_male - mu_female != 0, where mu_male and mu_female are the mean ages of
#                                  adult males and females in the United States.
      