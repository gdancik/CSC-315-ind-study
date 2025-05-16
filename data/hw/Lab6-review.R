# 2. A person who claims to be a psychic says that she 
# can correctly predict the outcome of a roll of a 
# standard die  (with numbers 1-6) more times than 
# what would be expected by chance. When you roll 
# a die 50 times, she correctly predicts the outcome
# 12 times. 

#  a. State the null and alternative hypothesis 
#     corresponding to this claim.

#  b. Find the z test statistic

#  c. Find the p-value
                                                                                                                                                                                                                                                                 
#  d. State the conclusion regarding whether or not 
#     the person's ability to predict the outcome of 
#     the die is different than what would be expected 
#     by chance.

#  e. What would it mean in the context of this problem 
#     if a Type II error occurred?



# 5. A study was conducted to evaluate whether or not a 
# daily dose of aspirin could reduce the risk of dying 
# from cancer. The study used a randomized trial design, 
# where individuals were randomly assigned to receive 
# aspirin or a placebo. The results of the study are in
# the table below. Note that you can use the prop.test 
# function for (d) – (f).



deaths = matrix(c(347,327,11188,13708),ncol=2)
rownames(deaths) = c("Placebo", "Aspirin")
colnames(deaths) = c("Yes", "No")

x = deaths[,1]
n = rowSums(deaths)

# a. State the null and alternative hypothesis

# b. What is the estimate of the common population proportion
#    (i.e., the value of p on slide 18).
# c. What is the standard deviation of the difference between
#    the two proportions (i.e., the value of the denominator 
#    of Z on slide 18)?

# d. What are the sample proportions? (You can use prop.test to get this)

# e. What is the z test statistic?

# f. What is the p-value?

# g. State the conclusions regarding whether or not aspirin can prevent
#    the rate of deaths due to cancer, and justify your conclusions
#    based on the p-value.

# h. What would it mean in the context of this problem if a 
#    Type I error occurred?

# i. What would it mean in the context of this problem if a 
#    Type II error occurred?



