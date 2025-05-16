##########################################################################
# Example of two-sample t-test
##########################################################################

library(dplyr)
library(ggplot2)

# The ChickWeight dataset includes data from an experiment that looked 
# at the effect of 4 different protein diets on chicken weight, where
# chickens were weighed every other day for 20 days, and at day 21.

# Note: because the sample size for each group is < 30, we assume that
# the underlying populations (weights for chicken on diet 1 or diet 2)
# are normally distributed.

# Here we will compare diet 1 and diet 2, and weights at day 21, which
# was the end of the experiment

df <- ChickWeight %>% filter(Time == 21, Diet %in% 1:2) %>%
  mutate(Diet = paste0('Diet_', Diet))

# Question: Is there evidence that diet 1 and diet 2 result in different
# average (mean) weights at day 21?

# (a) State the null and alternative hypotheses (done for you):

#   H0: mu1 - mu2 = 0
#   H1: mu1 - m2 != 0,

#   where mu1 and mu_2 are the mean weights at day 21 for chickens on 
#   Diet #1 and Diet #2, respectively.


# (b) Create side-by-side boxplots (using ggplot) showing the 
#     distribution of weight for each diet. Make sure to label the 
#     y-axis and give the chart a title.

ggplot(df, aes(x = Diet, y = weight, fill = Diet)) + 
  geom_boxplot(show.legend = FALSE) + theme_linedraw() +
  ggtitle('Relationship between Diet and Weight at Day 21\nin Chicken Experiment')

# (c) Carry out the two-sample t-test and report the p-value that
#     tests against H0.

s <- split(df$weight, df$Diet)
res <- t.test(s$Diet_1, s$Diet_2)
res$p.value

# alternative approach using formula
res <- t.test(weight ~ Diet, data = df)
res

# (e) Find the p-value 'manually' based on the test statistic and
#     appropriate degrees of freedom from the t.test result (which 
#     is stored in the $parameter object)

2*pt(-abs(res$statistic[[1]]), df = res$parameter)

# (f) State the conclusion regarding the null and alternative hypotheses in 
#     the context of this problem.

# Because the p-value is NOT < 0.05, we fail to reject H0. There is not
# sufficient evidence that there is a difference in diets based on 
# mean weight loss at day 21.

