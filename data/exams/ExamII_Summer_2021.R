##########################################################################
# CSC 315, Exam II
# Name: 
##########################################################################

##########################################################################
# Add R code to the script below and create a Notebook to explicitly
# answer the following questions. Your Notebook should include output 
# showing the requested results and graphs, and written answers to 
# questions should be provided in comments. When you are finished, 
# submit an HTML Notebook through Blackboard as described previously.
#
# Note: DO NOT delete / modify any of the questions / comments below!
##########################################################################

library(dplyr)
library(ggplot2)
library(gtools)


################################################################################
#  Blackjack time! A player has a blackjack if dealt two cards with a point
#  value of 21. The cards contain an Ace (worth 11 points),
#  and a 10 or a face card (J, Q, or K), worth 10 points. We will use
#  the number 1 to denote an Ace and 11,12, and 13 for the face cards.
################################################################################    


# 1) The function below draws a 2-card hand from a deck, and returns TRUE
#    if the hand is a blackjack (i.e., if the point value is 21), or FALSE
#    otherwise. Using this function, simulate 5000 games and find the 
#    probability that the player is dealt a blackjack. [8 points]

blackjack <- function() {

  # A deck of cards is given by the following code:
  deck <- rep(1:13,4)
  
  # The code below deals two cards and calculates the point value 
  hand <- sample(deck, 2) # draw 2 cards from the deck
  hand[hand > 10] <- 10  # face cards are worth 10 points
  hand[hand == 1] <- 11  # aces are worth 11 points
  return (sum(hand) == 21)
}



# 2) Find the theoretical / classical probability of being a dealt a blackjack, by
#    answering the questions below. The code below generates the sample space for 
#    all 2-card hands [8 points]

deck <- rep(1:13,4)
hands <- combinations(52, 2, deck, repeats.allowed = FALSE, set = FALSE)

#   (a) How many possible 2-card hands are there?


#   (b) Find the probability that a player is dealt a blackjack


# 3) A container contains 5 red marbles, 3 blue marbles, and 1 green marble.
#    The code below generates the sample space for randomly selecting 2 marbles, where
#    'R', 'B', and 'G' will denote 'red', 'blue', and 'green'. The code below
#    generates all combinations of selecting 2 marbles from the container.
#    Use R to analyze the 'marbles' sample space in order to answer the following 
#    questions: [8 points]

      colors <- c(  rep('R', 5), rep('B', 3), 'G')
      marbles <- combinations(9, 2, colors, repeats.allowed = FALSE, set = FALSE)

      
#     (a) What is the probability that both marbles are red? 
      
#     (b) What is the probability that the two marbles are different colors? Hint:
#         compare the 1st and 2nd columns of marbles, to get a vector of logical (TRUE/FALSE)
#         values.
                
# 4) Normal probability calculations. Write the correct R statement to calculate and display
#    the probability [3 points each = 12 points]  
#
#      (a) Suppose that X ~ N(42,2). Find P(X < 47)   
          
          
#      (b) What is the probability that an observation from the standard normal distribution
#          is greater than -0.65? 
          
          
#      (c) In a population that is normally distributed, calculate the probability 
#          that a randomly selected observation is more than 3 standard deviations
#          below the mean. 
          
          
#      (d) Suppose that X ~ N(78,5) and that 60 individuals are sampled. 
      
#           (i) What is the expected value of the sample mean? (simply type the answer)

#           (ii) What is the standard deviation of the sample mean? 
#                (type the calculation for the answer)
      
#           (iii) Find the probability that the sample mean is at least 79.
      

# 5) In 2013, the proportion of adults who smoke in the U.S. was 0.18. 
#    A 2015 study involving 1000 adults found that 163 of them smoked.
#    Is there evidence that the smoking rate has changed? [16 points]

# (a) State the null and alternative hypotheses, making sure to define 'p'
#     (this is done for you)

      # H0: p = 0.18
      # H1: p != 0.18,
      
      # where 'p' is the proportion of adults in the U.S. who smoke.
          
# (b) Use the prop.test function to conduct the hypothesis test WITHOUT
#     the continuity correction. Calculate the z test statistic from the 
#     prop.test object and extract the p-value 
          
          
# (c) Calculate the z test statistic using the appropriate formula, and find the p-value 
#     based on this test statistic. Note: these should match your answers from part (b).
          
          
# (d) The correct p-value is approximately 0.1617. Based on this p-value, state the 
#     conclusion regarding the null and alternative hypotheses in the context of this 
#     problem.

          

#  6) For this problem we will compare two different insecticides
#     (sprays) for controlling insects, using the built-in data set
#     'Insecticides'. The 'spray' column contains the spray used, and
#     the 'count' column contains the number of insects in an 
#     experimental area after the spray was used. The code below
#     filters the data to contain only results from spray 'C' and 'D'. [12 points]
          
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
      
          
# 7) Consider the following Z test statistics or one sample t-test
#   statistics for the hypothesis tests discussed in class. Find the
#   p-value, and state whether you would REJECT the null hypothesis, 
#   or FAIL TO REJECT the null hypothesis based on the p-value. [12 points]

# (a) Z = 1.22

# (b) Z = -2.9 

# (c) t = 2.11, n = 23

# (d) t = .29, n = 97


# For questions (9) - (10), state the following:

# (a) What would it mean in the context of this problem if a Type I error occurred?

# (b) What would it mean in the context of this problem if a Type II error occurred?


# 9) A study is conducted to determine whether the proportion of females in
#    the United States differs from 50%. The null and alternative hypotheses are
#    as follows:  [4 points]

#    H0: p = 0.50
#    HA: p != 0.50, where p = the proportion of females in the United States

# 10) A study is conducted to determine whether or not the average age of an adult
#     male in the U.S. is different than the average age of an adult female. The
#     null and alternative hypotheses are as follows:  [4 points]

#    H0: mu_male - mu_female = 0
#    HA: mu_male - mu_female != 0, where mu_male and mu_female are the mean ages of
#                                  adult males and females in the United States.
      
      