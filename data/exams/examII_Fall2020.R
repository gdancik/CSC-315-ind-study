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


# 1) Find the probability of being dealt a blackjack as follows:  
#    write a function, based on the code below, that randomly draws 
#    a 2-card hand from a deck, and returns TRUE if the hand is a 
#    blackjack (i.e., if the point value is 21). 
#    Then simulate 5000 blackjack games and find the empirical probability 
#    that the player is dealt a blackjack. [8 points]

# A deck of cards is given by the following code:
deck <- rep(1:13,4)

# The code below deals two cards and calculates the point value 
hand <- sample(deck, 2) # draw 2 cards from the deck
hand[hand > 10] <- 10  # face cards are worth 10 points
hand[hand == 1] <- 11  # aces are worth 11 points
sum(hand) # find the point total


# 2) Find the theoretical / classical probability of being a dealt a blackjack, by
#    answering the questions below. The code below generates the sample space for 
#    all 2-card hands [8 points]

deck <- rep(1:13,4)
hands <- combinations(52, 2, deck, repeats.allowed = FALSE, set = FALSE)

#   (a) How many possible 2-card hands are there?

#   (b) Find the probability that a player is dealt a blackjack


# 3) The code below generates the sample space for flipping 5 coins (or flipping the
#    same coin 5 times). [8 points]

      coins <- permutations(2,5, c("H", "T"), repeats.allowed = TRUE)

#     Use R to analyze the 'coins' sample space in order to answer the following questions:
      
#     (a) What is the probability of getting 3 heads? 

#     (b) What is the probability of getting 3 heads IN A ROW? Hint: I suggest to write
#         a function that returns TRUE if there are 3 heads in a row for a vector 'x', and 
#         then 'apply' this function to each row of the 'coins' matrix. To do this, you 
#         should use the 'which' function, where 'which(x)' returns a numeric vector of 
#         index values 'i' that correspond to x[i] being TRUE. An example is given below.

          # create a vector of coin flips
          flips <- c('H','H','T','H')
          
          # which flips resulted in heads -- in this case, flips 1, 2, and 4
          which(flips == 'H')

                
# 4) Normal probability calculations. Write the correct R statement to calculate and display
#    the probability [3 points each = 12 points]  
#
#      (a) Suppose that X ~ N(70,4). Find P(X < 75)   
          
          
#      (b) What is the probability that an observation from the standard normal distribution
#          is greater than 1.39? 
          
          
#      (c) In a population that is normally distributed, calculate the probability 
#          that a randomly selected observation is more than 2 standard deviations
#          below the mean. 
          
          
#      (d) Suppose that X ~ N(50,3) and that 17 individuals are sampled. 
#          Find the probability that the sample mean is at least 51. 
          
          
          
          
# 5) In 2013, the proportion of adults who smoke in the U.S. was 0.18. 
#    A 2015 study involving 1000 adults found that 163 of them smoked.
#    Is there evidence that the smoking rate has changed? [16 points]

# (a) State the null and alternative hypotheses, making sure to define 'p'

          
# (b) Use the prop.test function to conduct the hypothesis test WITHOUT
#     the continuity correction. Calculate the z test statistic from the 
#     prop.test object and extract the p-value 
          
          
# (c) Calculate the z test statistic using the appropriate formula, and find the p-value 
#     based on this test statistic. Note: these should match your answers from part (b).
          
          
# (d) The correct p-value is approximately 0.1617. Based on this p-value, state the 
#     conclusion regarding the null and alternative hypotheses in the context of this 
#     problem.

          
# 6) A study published in 2014 states that "A total of 31,989 participants were enrolled from 
#    126 research centers in the United States and Canada (15,991 were randomly assigned to 
#    receive IIV3-HD [a high dose flu vaccine], and 15,998 to receive IIV3-SD [a standard dose
#    flu vaccine]). In the intention-to-treat analysis, 228 participants in the IIV3-HD [high 
#    dose] group (1.4%) and 301 participants in the IIV3-SD [standard dose] group (1.9%) had 
#    laboratory-confirmed influenza." [16 points]

#    Source: https://www.nejm.org/doi/full/10.1056/nejmoa1315727
          
# (a) State the null and alternative hypotheses, making sure
#     to define the 'p' parameters
          
# (b) Use the prop.test function to find the test statistic and p-value
          
# (c) Your p-value should be 0.001624. Based on this p-value, state the conclusion 
#     regarding the null and alternative hypotheses in the context of this problem.
          
          
#  7) For this problem we will compare two different insecticides
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
      
          
# 8) Consider the following Z test statistics or one sample t-test
#   statistics for the hypothesis tests discussed in class. Find the
#   p-value, and state whether you would REJECT the null hypothesis, 
#   or FAIL TO REJECT the null hypothesis based on the p-value. [12 points]

# (a) Z = 2.49

# (b) Z = 0 (additional question: why would Z be equal to 0)? 

# (c) t = -3.11, n = 23

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
      
      
# 11) Extra Credit - The power of a test is the probability of 
#     correctly rejecting H0 (of rejecting H0 when H0 is false).
#     The power of a test depends on the sample size (tests
#     become more powerful as 'n' increases). This should be intuitive.
#     If we are testing whether a coin is biased, we are much more
#     likely to have sufficient evidence of bias if we flip the coin
#     100 times compared to 10 times. While formulas are available
#     for these calculations, let's calculate them empirically.
      
# (a) Complete the function below which randomly flips a biased
#     coin 'n' times. Have the function carry out a hypothesis test,
#     where H0: p = 0.50, where p = probability of Heads, and 
#     return TRUE if H0 is rejected, and FALSE otherwise.

reject <- function(n) {
  # flip coin 'n' times -- the coin is biased with p(Heads) = 0.60
  s <- sample(c("H", "T"), n, prob = c(0.6, 0.4), replace = TRUE)

  # TO DO: carry out hypothesis test for H0: p = 0.50, and
  # return TRUE if you reject H0.
  s
}
   
# (b) Calculate the empirical probability of rejecting H0 for
#     sample sizes of 10, 100, and 200 (you are calculating the
#     empirical power of the test for these sample sizes). 
#     Based on these results, how many times do you recommend 
#     flipping the coin to show that it is biased? 

# (c) Recall that a Type II error is when you fail to reject H0 when you
#     should (you fail to reject H0 when H0 is false). Based on your answer to (b), 
#     what is the probability of making a Type II error, when the true probability of 
#     heads is 0.60 and you flip the coin 200 times.
