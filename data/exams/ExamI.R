# CSC 315, Exam I 
# Name: 

# Directions: Modify this script by adding R code to answer 
# the questions and/or complete the steps below. 
# When you are finished, create an HTML Notebook 
# and submit through Blackboard (https://easternct.blackboard.com/)
# as described previously. If you are unable to submit a Notebook, you may 
# submit the R code instead (but with a point deduction)
# [Notebook is worth 4 points]

# Note: All graphs must be created in ggplot unless
# stated otherwise, and all graphs must have appropriate 
# titles and axis labels.

library(dplyr)
library(ggplot2)
library(readr)

# 1. Create a vector that contains the numbers 1 - 10, 
#    and the number 50  [4 points]

# 2. Run the statement below, then add a single line of code to 
#    do the following: [4 points each]

x <- c(1, 3, -4, -11, 15:20)
   
#     (a) Extract the first number from the vector 'x'


#     (b) Extract the first 5 numbers from the vector 'x'


#     (c) Extract the positive numbers from the vector 'x'


# 3. Write a function called min.max which takes a vector x as an argument 
#    and returns a list containing two named elements, the minimum of x and the
#    maximum of x. Use this function to find the minimum and maximum of the 
#    vector 'x' from problem (2). [12 points]


# 4. The code below creates a data frame that includes the columns
#    'wt' and 'mpg'. [4 points each]
   
      mtcars2 <- select(mtcars, wt, mpg)

#   (a) Display the first 3 rows of this dataset
      
      
#   (b) Use the 'apply' function to find the 
#       median of each column (i.e., the median weight and mpg)

  
#    Run the command below to read in a data set that contains
#    information for trending You Tube videos on select dates.
#    We will make use of the following columns:
#         title: the title of the video
#         views: the number of views
#         category: the category of the video
#         date: the date the video was trending (in YYYY-MM-DD format)
  
videos <- read_delim("https://bioinformatics.easternct.edu/BCBET2/videos.csv", " ",
          escape_double = FALSE, col_types = cols(date = col_character()),
          trim_ws = TRUE)

# 5. Construct a frequency bar graph showing the number of videos
#    across each category. What category had the most trending videos?
#    [8 points]


# 6. The code below generates a contingency table for
#    the number of trending videos for each category for
#    each date. Answer the questions that follow: [4 points each]

t <- table(videos$date, videos$category)
t

#   (a) Use the 't' object to generate a table
#       of conditional proportions, conditional on the date.


#   (b) What was the proportion of "Howto/Style" videos on
#       2018-01-06?


#   (c) What was the proportion of "Howto/Style" videos on
#       2018-01-12?


# 7. Construct a stacked bar graph showing the conditional proportions
#    for each category, conditioned on date. Based on this graph and 
#    your answer to the previous question, does there appear to be an 
#    association between trending categories and date? Why or why not?
#    [8 points]


# 8. How many videos have more than 10 million views (10 million = 10000000), 
#    and what are the titles of these videos (display the titles only). 
#    Note: One of Bruno Mars' videos appears twice because it was trending 
#    on two different days. And because he's the man. [8 points]


# The next set of questions uses a dataset containing nutritional
# information for menu items from McDonald's. We will focus on the following
# columns:
#     Category - the food category
#     Sugars - the amount of sugar (in grams)
#     Calories - the total number of calories
#     Fat - the total amount of fat, in grams
#

# Read in the dataset
mcdonalds <- read.csv("http://bioinformatics.easternct.edu/BCBET2/mcdonalds.csv")

# 9. In the code below, set the x, y, and fill values of the 
#    aesthetics correctly to construct side-by-side boxplots 
#    for Sugar content across Categories. NOTE: You only need 
#    to change the aes() function for this problem.
#    What category or categories have the highest sugar content?
#    [8 points]

# modify aes() to construct boxplots for sugar content (Sugars) for each 
# Category
ggplot(mcdonalds, aes(x = 1, y=1, fill = 1)) +   
geom_boxplot() + 
ggtitle("Sugar Content by Category at McDonald's") +
  theme_classic() +
  theme(legend.position = "none") + 
  # display labels at an angle
  theme(axis.text.x = element_text(hjust = 1, angle = 45)) +  
  ylab("Sugar (grams)")


# 10. The code below constructs a scatterplot that predicts Calories 
#     from Fat. Add code to add the corresponding regression line.
#     [4 points]

ggplot(mcdonalds, aes(Fat, Calories)) + geom_point() +
  ggtitle("Relationship between Fat Content and Calories at McDonald's") +
  xlab('Fat (grams)')

# 11. Find the linear regression line that predicts Calories from Fat and 
#     complete the following: [4 points each]

#     (a) Find and interpret the y-intercept


#     (b) Find and interpret the slope


#     (c) How many calories do you predict for food that has 30 grams of fat?



# Extra Credit

# In a SINGLE statement, calculate the correlation between
# Fat and Calories, Cholesterol and Calories, Sodium
# and Calories, and Carbohydrates and Calories. Hint: how 
# can you 'apply' the correlation function to do this?

