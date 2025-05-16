########################################################
# Name: REVIEW
# CSC-315
# Lab #2: Graphical and Numerical Summaries of Data
########################################################

library(ggplot2)
library(dplyr)
library(readr)
survey <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv")


# 8. Construct a relative frequency table for whether or not a student consumes alcohol
#    at least 1 day per week, on average (i.e., consumes alcohol > 0 days per week).
#    Do this by first creating a logical vector where TRUE corresponds to consuming
#    alcohol and FALSE corresponds to does not consume alcohol. Then create a relative
#    frequency of these TRUE and FALSE values. Tables and relative frequency tables
#    are stored as named vectors (e.g., x <- c(item1 = 1, item2 = 2)). Use the 'names' 
#    function to change the names from FALSE and TRUE to "Consumes alcohol" and
#    "Does not consume alcohol"



# 9. Out of the people who heard "Laurel" in this class, would they rather fight one 
#    horse-sized duck or one hundred duck-sized horses? Answer this question by using
#    dplyr's 'filter' function to create a new data.frame for those who heard "Laurel". 
#    Then generate a relative frequency table for the 'Fight' column results. 
#    Repeat the analysis to answer the same question for those who heard "Yanny"
#    What do you conclude about a person's choice regarding the "Fight" question?


# 10. Construct a histogram for Alcohol consumption, by using the hist() function with the argument
#     breaks = 14 to set the number of groupings. Describe the shape of its distribution. 
#     Is it unimodal, bimodal, or flat. Is it skewed right, skewed left, or symmetric?

hist(survey$Alcohol, breaks = 14, main = 'Histogram of alcohol consumption', 
     xlab = 'Alcohol consumption (days/week)', ylab = '# students')


# 11. Calculate the mean and median for Alcohol consumption. 
#     Which is a better measure of averages? (Note: although these numbers are similar,
#     one would still be considered better than the other -- why?)

mean(survey$Alcohol)
median(survey$Alcohol)



# 14. Create side-by-side boxplots showing the average hours of sleep 
#     based on a person's gender, and answer the questions below:

ggplot(survey, aes(x=Gender, y = Sleep, fill = Gender)) + 
  geom_boxplot() + theme_classic()

#     (a) Does there appear to be a difference in the 'median' amount of 
#         sleep between those who identify as 'Female' compared to 'Male'.
#     (b) What does the difference in the boxes indicate?
#         groups? 

#       Describe the shapes of the distributions for Male and Females?
#       (can you do this from the boxplot?)

# let's use facet_grid with geom_histogram to generate histograms
# for each gender
ggplot(survey, aes(Sleep, stat(count), fill = Gender)) +
  geom_histogram(breaks = 1:10, color = 'black') + 
  facet_grid(rows = vars(Gender)) +
  theme(legend.position = "none") +
  ggtitle('Histograms of Hours of sleep / night by Gender') +
  xlab("Sleep (hours/night)") + ylab("Frequency") 




