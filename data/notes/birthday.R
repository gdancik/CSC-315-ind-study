############################################################
# The birthday problem - how many people need to be in 
# a room before the probability that at least 2 people
# share a birthday is more than 50%?
############################################################

library(ggplot2)

#################################################################
# anyDuplicated function returns 0 if no elements are duplicated
#################################################################

x <- 1:10
anyDuplicated(x) 

#################################################################
# anyDuplicated function returns the index of first duplicated 
# element if there are duplicates; otherwise it returns 0
#################################################################

x[1:2] <- 1
anyDuplicated(x) 

############################################################
## randomly select 2 birthdays, assuming 365 days / year
############################################################
s <- sample(1:365, 2, replace = TRUE)

############################################################
## function to randomly select 'n' birthdays, 
## assuming 365 days / year
############################################################
birthdays <- function(n) {
  s <- sample(1:365, n, replace = TRUE)  
  return(s)
}

#######################################################
## randomly generates 'n' birthdays
## returns TRUE if any are the same; FALSE otherwise
#######################################################
duplicate.birthdays <- function(n) {
  b <- birthdays(n)
  anyDuplicated(b) > 0
}

##################################################################
## returns the empirical probability (based on 5000 trials)
## that >1 person has the same birthday in a room with 'n' people
##################################################################
prob.duplicate <-function(n) {
    r <- replicate(5000, duplicate.birthdays(n))
    sum(r) / length(r)  
}
  

##################################################################
## plot empirical probabilities for n = 1,2, ... 100 people
##################################################################
n <- 1:100
probs <- sapply(n, prob.duplicate)

ggplot() + geom_line(aes(n, probs)) +
          ggtitle("The Birthday Problem") + 
          labs(x = "# of people in room", 
               y = "Probability at least 2 people have the same birthday") +
          theme_light() +
          geom_hline(aes(yintercept = 0.50), color = "red") +
          geom_vline(aes(xintercept = 23), color = "red")
          

