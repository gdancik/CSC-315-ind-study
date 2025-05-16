########################################################
# Name:
# CSC-315
# Lab #2: Graphical and Numerical Summaries of Data
########################################################

##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps and explicitly answer the following questions.
# Your Notebook must include output showing the requested tables and
# graphs, and answers to questions should be provided in comments.
# Also, all graphs must be given an appropriate title, x-axis label, and
# y-axis label. The ggplot2 library must be used to generate all
# graphs unless stated otherwise. When you are finished, submit an
# HTML Notebook through Blackboard as described previously.
#
# Note: DO NOT delete / modify any of the questions / comments below!
##########################################################################

# 1) Load our classes survey data (available at:
#  https://gdancik.github.io/CSC-315/data/datasets/csc315_survey_fall2023.csv)
#   and add the code for this to the script. Note: I suggest to 
# change the name of the survey data frame (such as to 'survey') so
# that it is easier to type!


# 2) How many students completed the survey (i.e., how many rows are there)?

# 3) How many questions were asked (i.e., how many columns are there)?

# 4) Display the 'CatOrDogPerson' value for the 4th person (and only this person)

# 5) (a) Construct a frequency table for whether someone is a Cat or Dog person.

#    (b) Construct a relative frequency table for whether someone is a Cat or Dog person. 

#    (c) Based on this data, does this class prefer cats or dogs?

# 6) Construct a frequency bar graph for the "LightOrDark" data, which contains the 
#    response to the question "When using digital media, what is your preferred mode?"
#    The bars should be colored according to preference, using the default 
#    colors, and the graph should not have a legend. What do you conclude
#    about digital media preference in this class?


# 7) Construct a Pareto Chart for a person's favorite movie genre
#    (you may display either the frequency or relative frequency). What
#    do you conclude about movie preference in this class?


# 8) Using the alcohol consumption data, first create a factor corresponding to whether
# or not a student drinks (the levels for this factor should be 'does drink' and
# 'does not drink'. Note: do this by first creating a logical vector, which will contain
# TRUE or FALSE values

# Then, generate a relative frequency bar graph describing alcohol consumption in 
# this class. If a student is selected at random are you more or less likely to 
# select a student who drinks?


# 9) Construct a histogram for alcohol consumption using the 'hist' 
#     function with default parameters. Note: you should be looking
#     at the original Alcohol data, not the two values from 
#     question (8). Describe the shape of its distribution. 
#     In particular, describe whether it is unimodal, bimodal, or flat, and 
#     whether it is skewed right, skewed left, or symmetric?


# 10) Calculate the mean and median for Alcohol consumption. 
#     Which is a better measure of average, based on the shape of the 
#     distribution? (Note: in this case, the mean and the median are similar, but
#     in general one of these is preferred for this distribution shape) 

# 11) Create a data frame from the survey data that contains only 
#    the columns `CatOrDogPerson` and `College GPA`, and then use
#    the function remove_missing to remove all rows with missing
#    values. Then using this new data frame, create side-by-side 
#    boxplots of College GPA based on whether a person is a "cat" 
#    or "dog" person, and answer the questions below:
#     (a) Does there appear to be a different in College GPA
#         between "cat" and "dog" people in this class?
#     (b) Are there any outliers? If so, how many, and for which group?
#     (c) What happens if you don't remove the missing values?


# 12) (a) Find the variance for College GPA using the 'var' function.
#
#     (b) Use the 'sqrt' function with your answer to (a) to find the 
#         standard deviation; show that this calculation for the standard
#         deviation is the same as what you get when using the 'sd' function.
