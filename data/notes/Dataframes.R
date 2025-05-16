#################################################################
# A data.frame is a table (like a matrix) but columns can be 
# different types. Elements, rows, and columns can be 
# accessed using matrix notation, e.g., df[1,] or df[,3:4]. A 
# data.frame is also a list, where df[[i]] is the ith column,
# and columns can also be accessed by name, e.g., df$age or
# df[["age"]]
#################################################################

#################################################################
# We will read in data from the URL
# https://gdancik.github.io/CSC-315/data/datasets/survey.txt
# using Import Dataset --> From Text (readr) from the environment tab. 
# The table should be named 'survey'. After importing,
# make sure to add the R code to this script

# Note: technically, the table returned will be stored as a
# 'tibble', which is a type of data frame with special properties
# that we will not discuss further, but for more
# information see: https://r4ds.had.co.nz/tibbles.html
##############################################################

##############################################################
# Add R code to import survey 
##############################################################


#####################################################
# Question: How many rows (individuals) and 
# columns(variables) are in this data?
#####################################################

colnames(survey) # get names of columns
summary(survey) # summarizes each column; results depend on column type
max(survey$FB)   # max hours / week spent on FB


# get data for the males only, the traditional way
males1 <- survey[survey$Gender == "Male",]
print(males1)

###########################################################
# The dplyr package is useful for manipulating data frames
###########################################################

# Load the dplyr package 
library(dplyr)

# get data for the males only, using the dplyr 'filter' function
males2 <- filter(survey, Gender == "Male")
print(males2)

# create a new data frame containing only the College and HS GPAs
# using the dplyr 'select' function
# Note that backticks are used for column names that would not be
# valid variable names (such as when they contain spaces)
survey.GPA <- select(survey, `College GPA`, `HS GPA`)
print(survey.GPA)

# Use dplyr 'mutate' to create a new variable #
alcohol <- mutate(survey, AlcoholPerYear = Alcohol*52)

# create new table of females showing only GPAs -- approach 1
survey.females <- filter(survey, Gender == "Female")
females1 <- select(survey.females, `HS GPA`, `College GPA`)
print(females1)

# create new table of females showing only GPAs -- approach 2
# (but this is very confusing)
females2 <- select(   
    filter (survey, Gender == "Female"),
    `HS GPA`, `College GPA`
  )
print(females2)


##################################################################
# Let's look at a better approach using 'pipes' 
# The pipe ('%>%') operator is used to pass an object to
# a function as its first parameter, so that
# x %>% f(y, ...) becomes f(x, y, ...)
# Note: the pipe operator comes from the 'magrittr' package, 
# which is loaded with 'dplyr'
##################################################################

# same as filter(survey, Gender == "Female")
females <- survey %>% filter(Gender == "Female")
print(females)

# alternative way to create table with females, showing only GPAs
females3 <- survey %>% filter(Gender == "Female") %>%
                      select(`HS GPA`, `College GPA`)
print(females3)

############################################################
## Were college GPAs higher for males or females?
## split(x,y) returns a list of x values split by y values;
## each element of the list are the x-values corresponding
## to a particular y value
############################################################

s <- split(survey$`College GPA`, survey$Gender)

mean(s$Female)
mean(s$Male)

boxplot(s, ylab = "College GPA", col = c("pink", "blue"), 
        main = "College GPA by Gender")

#######################################################################
## Question: What is the highest college GPA for males? For females?
#######################################################################

