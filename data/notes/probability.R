######################################
## Module 4: Probability
######################################

# Note: the 'gtools' library is needed to enumerate
# permutations and combinations

library(gtools)
library(ggplot2)
library(dplyr)

######################################################
# Simulate rolling a die 10,000 times, by sampling
# from the values 1:6, 10,000 times, with replacement
######################################################
roll.num <- sample(1:6, 10000, replace = TRUE)

## treat the results as a factor (qualitative variable) and construct a barplot
roll <- factor(roll.num)
ggplot() + geom_bar(aes(roll, fill = roll), show.legend = FALSE) + 
  ggtitle("Outcome from rolling die 10000 times") + 
  theme_linedraw()
  
# function to find the proportion of sixes occurring in first 
# 'i' elements of 'x' 
proportion.sixes <- function(i,x) {
  x <- x[1:i] # limit to first 'i' elements
  count <- sum(x==6)   # count the number of sixes
  count/i              # return the proportion
}

## apply this function to all integers from 1- 10000
props <- sapply(1:length(roll.num), proportion.sixes, x=roll.num)


## plot empirical and theoretical probabilities
six.plot <- ggplot() + geom_point(aes(x = 1:length(props), y=props), color = "blue") +
  ggtitle("Empirical probability of rolling a 6 with a fair die") +
  labs(x = "# rolls", y = "proportion of sixes rolled") +
  geom_hline(aes(yintercept=1/6, linetype = "theoretical\nprobability"),color = "red") +
  theme_linedraw() +
  scale_linetype_manual(name = '', values = 2) # this adds the legend (values = 2 for dotted line)


# plot only first 100 rolls
six.plot + xlim(0,100)

#####################################################
# The empirical probability of an event occurring is
# the proportion of times the event occurs over a
# large number of samples/trials
#####################################################

# look at complete plot
six.plot                

###################################
# Let's look at a coin example
###################################

## simulate flipping a fair coin 10,000 times
coins <- sample(c("H", "T"), 10000, replace=TRUE)

# generate bar graph of frequencies
ggplot() + geom_bar(aes(coins, fill = coins), show.legend = FALSE) + 
  ggtitle("Outcome of flipping a fair coin 10000 times") +
  labs(x = "outcome", y = "Frequency")

  
# calculate a relative frequency table and generate a bar graph of 
# relative frequencies 

# create data.frame containing relative frequency table
p <- table(coins) %>% prop.table() %>% data.frame()  

# plot relative frequencies
ggplot(p) + geom_col(aes(x = coins, y = Freq, fill = coins), show.legend = FALSE) + 
  ggtitle("Outcome of flipping a fair coin 1000 times") +
  labs(x = "outcome", y = "Relative Frequency") +
  geom_hline(aes(yintercept=1/2, linetype = 'theoretical probability'),
             color = "black") +
  scale_linetype_manual(name = "", values = 2) +
  ylim(0,1) + theme_classic()


# Let's repeat this simulation using a biased coin, flipped 10,000 times
# (the biased coin has a 90% probability of Heads, 10% probability of tails)
coins <- sample(c("H", "T"), 10000, prob = c(.9,.1), replace=TRUE)

# create data frame of relative frequency table
p <- table(coins) %>% prop.table() %>% data.frame()


ggplot(p) + geom_col(aes(x = coins, y = Freq, fill = coins), show.legend = FALSE) + 
  ggtitle("Outcome of flipping a biased coin 1000 times") +
  labs(x = "outcome", y = "Relative Frequency") +
  geom_hline(aes(yintercept=9/10, linetype = "theoretical probability (H)"),
             color = "black") +
  scale_linetype_manual(name = "", values = 2) + ylim(0,1)


############################################################
## general method for repeating a probability experiment --
## write a function to do the experiment once, and return
## the characteristic of interest. Then use the replicate 
## function to repeat the experiment
############################################################

## example function: flip a fair coin 2 times, 
#  we are interested in getting 2 heads
# returns TRUE if we get 2 heads, FALSE otherwise
flip.two.heads <- function() {
  
  # flip the coin twice (same as flipping 2 coins)
  f <- sample(c("H", "T"), 2, replace = TRUE)
  
  # count the number of Heads
  count <- sum(f=="H")
  
  # return TRUE if we got 2 heads
  count == 2
}

# flip a coin 2 times, repeat 1000 times
two.heads <- replicate(1000, flip.two.heads())

# find the empirical probability of getting 2 heads = 
# number of times we get 2 heads / number of experiments
prop.heads <- sum(two.heads) / length(two.heads)
prop.heads


#########################################################
# Exercise: Copy and modify the above code to find
# the empirical probability of flipping a coin 3 times
# and getting at least 2 heads.
#########################################################


#########################################################
## classical probability - when all outcomes
## are equally likely, 
## P(A) = (number of outcomes in A) / 
##          (number of outcomes in sample space)
#########################################################

##########################################################
# We can use the 'permutation' function from the gtools
# library for classical probability calculations.
# The arguments for the permutation function include 
# (1) number of outcomes per trial of the experiment, 
# (2) number of trials,
# (3) sample space for each trial,
# (4) repeats = TRUE if outcomes can repeat across trials
# Permutations gives all possible arrangements where
#    order matters
##########################################################

##########################################################
# Find the sample space S for flipping a coin 3 times  #
##########################################################
S <- permutations(2,3, c("H", "T"), repeats = TRUE)

# Define the event corresponding to getting exactly two heads
index <- rowSums(S == "H") == 2
S[index,]

# Define the event corresponding to getting at least two heads
index <- rowSums(S == "H") >= 2
S[index,]

#######################################################
# Finding classical probabilities
#######################################################

##########################################################
## aside: useful logical functions: 
#   any(x) returns TRUE if any element in x is TRUE
#   all(x) returns TRUE if all elements in x are TRUE
##########################################################

numbers <- 1:3
any(numbers == 4)
any(numbers == 1)
all(numbers == 1)

numbers <- c(1,1,1)
all(numbers == 1)

# Find the probability of getting all heads, P(H = 3), when
# flipping a fair coin 3 times
all.heads <- apply(S == "H", 1, all)
sum(all.heads) / length(all.heads)

# Find the probability of getting at least 1 Head, P(H >= 1)
any.heads <- apply(S == "H", 1, any)
p.any.heads <- sum(any.heads) / length(any.heads)
p.any.heads

# Note that the complement of at least one head (H >=1)
# is no heads (H=0) (or all tails). Therefore by the 
# rule of complements, P(no heads) = 1 - P(at least 1 heads)

# probability of no heads
1 - p.any.heads

# probability of no heads (direct calculation)
no.heads <- apply(S == "T", 1, all)
p.no.heads <- sum(no.heads) / length(no.heads)
p.no.heads

