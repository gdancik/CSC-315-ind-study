##########################################################################
# CSC-315, Fall 2023
# Lab 1: R programming
# Name: 
##########################################################################

##########################################################################
# Add R code to the script below in order to explicitly output the 
# answers to the following questions. Turn in this assignment
# by creating an HTML Notebook and submitting the Notebook through 
# Blackboard.
# Note: DO NOT delete or modify any of the questions / comments below!
##########################################################################

# 1) What is 100 * 3 + 12

# 2) What is the sum of all integers between (and including) -100 and +100.

# 3)	Create a vector named 'v1' that contains the numbers 1-5.

# 4) Change the 3rd value of the above vector to 99

# 5)	Create a vector named 'nums' that contains the numbers 1-20, 37, and 0.

# 6)	Create a vector named 'me' that contains two elements, your first name and last name

# 7) The code below creates a matrix that contains the grades for 2 students
#    for 3 exams. Set or change the column names to "ExamI", "ExamII", 
#    and "ExamIII", and set or change the row names to "Holly Parker" and 
#    "William Jones"

    grades <- matrix(c(91, 89, 85, 74, 89, 82),byrow = TRUE, nrow = 2)

# 8) Calculate the average grade for William Jones, using the 'mean' function.
#    In other words, find the average of the values in the second row. Note
#    that your code should work on any matrix named 'grades' that has at
#    least 2 rows.

# 9) "William" prefers to be called "Bill". Change the name of the 2nd row of the matrix 
#   to "Bill Jones". You should do this in a statement that only operates on 
#   the second row. Note that for a matrix 'm', R allows you to directly assign a value to any element 
#   (or elements) of rownames(m).

# 10)	Create a list that includes the following objects: 
#       (1) a vector that contains two elements, your first name and last name; 
#       (2) your major

# 11) Read in the survey.txt file as was done in class (and put the code for this in your script!)
# The survey results are available from https://gdancik.github.io/CSC-315/data/datasets/survey.txt
    
    
# 12)	How many individuals surveyed did not use FB (i.e., spent 0 hours / week on FB)

# 13) What are the GPAs of the three students with the lowest College GPAs (you 
#    should display only these GPAs)? Hint: use the 'sort' function. 

# 14) What are the GPAs of the three students with the highest college GPAs? 
#    Hint: use the sort function with decreasing = TRUE

# 15) Use the 'filter' function from 'dplyr' to create a data frame (tibble) 
#    called surveyA that includes the students with a 3.5 college GPA or higher

# 16) Display the 'Gender' column from surveyA and comment on the number of 
#    individuals who are male, and the number who are female.

# 17) Create a list that contains the following named elements, based on
#     the survey data:
#     min_cgpa - the lowest college gpa
#     max_cgpa - the highest college gpa
#     mean_cpga - the mean college gpa
#
    
# 18) Create an HTML noteobok 
