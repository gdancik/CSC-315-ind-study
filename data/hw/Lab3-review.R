library(ggplot2)
library(dplyr)

# 1) Construct the contingency table in R, along with the table of 
#    conditional proportions. Does there appear to be a relationship 
#    between Income and happiness? Why or why not?

happiness <- cbind(Not.Too.Happy = c(26,117,172), Pretty.Happy = c(233, 473,383), Very.Happy = c(164,293,132))
rownames(happiness) = c("Above Average", "Average", "Below Average")

# TO DO: need to construct table of conditional proportions and answer the 
# above question

#2) 

library(readr)
survey <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv")


# 3) Construct a stacked bar graph that shows whether there is an 
#    association between whether someone heard Yanny or Laurel and 
#    whether their preference was to fight 1 horse-sized duck or 100 
#   duck-sized horses. In our class, does there appear to be an 
#   association between these two variables? If so, describe the association.


# 7)

ggplot(survey, aes(hsGPA, collegeGPA, color = Gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  ggtitle('Highschool vs. College GPA by Gender')

#8 - 10)

ggplot(mtcars, aes(wt, mpg)) + geom_point() + 
  geom_smooth(method = 'lm') + 
  ggtitle('MPG vs Weight') + xlab('weight (1000lbs)')

#9. Find and interpret slope and y-intercept

#10. Predict mpg when weight is

#  (a) 3000 lbs

#  (b) 7000 lbs



