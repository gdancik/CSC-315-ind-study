# CSC 315, Exam I Practice Problems

library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)

# Note: This is not a comprehensive review, but contains exercises
# covering some of the concepts that will be on the first exam. 

# In addition to these exercises, make sure you understand the concepts 
# covered in lecture and on the previous labs. 

# All graphs must be created in ggplot unless
# stated otherwise, and be given appropriate titles and axis labels.


# 1. Create a vector called ages that contains the numbers 21, 24, 32, and 19

# 2. Create a vector called evens that contains all even numbers 
#     between 1 and 100, and the number 200.

# 3. Write a function called min.positive which takes a vector 'x' as an
#    argument and returns the the smallest positive number from the vector.
#    For example, for v <- c(-3,10,2), min.positive(v) would return 2

# 4. Write a function called min.max which takes a vector x as an argument 
#    and returns a list containing two named elements, the minimum of x and the
#    maximum of x. Use this function to find the minimum and maximum of the 
#    vector ages from problem (1). 

# 5. The code below creates a matrix filled with 5 columns and 20 rows, 
#    that is filled with random numbers between 0 and 1.

      random_matrix = matrix(runif(100), ncol = 5, nrow = 20)

#   (a) Find the median of each row 
#   (b) Find the median of each column.

# 6. Run the command below to read in an old class survey:
      
      survey <- read_delim("http://pastebin.com/raw/1csmBawE")
      
# 7. The code below generates a scatterplot of College GPA against Alcohol
#    consumption that includes the regression line, and colors the points by
#    Gender.
      
      ggplot(survey, aes(Alcohol, College.GPA)) + 
        geom_point(aes(color = Gender)) + 
        geom_smooth(method = 'lm', color = 'black', se = FALSE) +
        theme_classic() + ggtitle('Alcohol Consumption vs. College GPA') +
        xlab('Alcohol consumption (days / week)')

    
    # (a) Fit a linear model that predicts college GPA from Alcohol consumption
    #     (i)  Find and interpret the y-intercept
    #     (ii) Find and interpret the slope
      
        
# 9. For those who agree with same sex marriage legalization, find the mean 
#    and standard deviation of College GPA.
      
# 10. Construct side-by-side boxplots for FB usage based on whether or not
#    a person agrees or disagrees with same sex marriage 
#    Was there an association between FB usage and views on same sex marriage
#    in this class?
      
  
# 11.  Construct a stacked bar graph with one bar for males and one for 
#      females, where each bar showing the proportion who agree and disagree
#      with marijuana legalization. Was there a relationship between gender
#      and views on marijuana legalization in this class?

    