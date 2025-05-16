##########################################################################
# CSC 315, Exam I 
# Name: 
##########################################################################

##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps and explicitly answer the following questions.
# Your Notebook should include output showing the requested tables and
# graphs, and answers to questions should be provided in comments.
# Also, all graphs must be given an appropriate title, x-axis label, and
# y-axis label. The ggplot2 library must be used to generate all
# graphs unless stated otherwise. When you are finished, submit an
# HTML Notebook through Blackboard as described previously.
#
# Note: DO NOT delete / modify any of the questions / comments below!
##########################################################################

library(dplyr)
library(ggplot2)
library(readr)

# 1) Run the statement below, then add 1-2 lines of code to 
#    do each of the following. Note that your code below should work for
#    any numeric vector 'x' [4 points each]

x <- c(1, 3, -4, -11, 15:20)
   
#     (a) Extract the positive numbers from the vector 'x'


#     (b) Replace the negative values in 'x' with 0. After running this 
#         code, all negative values in 'x' should be set to 0.


#     (c) Create a vector named 'z' that contains the 1st and last value 
#         of 'x'



# 2) Write a function called min.max which takes a vector x as an argument 
#    and returns a list containing two named elements, the minimum of x and the
#    maximum of x. Use this function to find the minimum and maximum of the 
#    vector 'x' from problem (1). [6 points]


# The remaining questions are based on the IMDB 5000 dataset, which contains
# information from around 5,000 movies scraped from IMDB. The dataset is
# available at https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset,
# and can be read in using the command below:

movies5000 <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/movie_metadata.csv")

# 3) (a) How many rows are in the table? [4 points]


#    (b) Display the first 5 directors    [4 points]


# Let's limit our analysis to movies directed by either James Cameron,
# Steven Spielberg, Christopher Nolan, or Robert Zemeckis. Run the code
# below to filter the movies based on these directors, and use this
# data frame for the remaining problems

movies <- movies5000 %>% filter(director_name %in% 
                              c("James Cameron", "Steven Spielberg", 
                                "Christopher Nolan", "Robert Zemeckis" ))



# 4) Construct a relative frequency table for the number of movies directed by 
#    each person [6 points]


# 5) Construct a Pareto chart for the number of movies from each director 
#    (you may use either frequencies or relative frequencies). 
#    [8 points]


# 6) Construct a contingency table and the corresponding conditional 
#    proportions for the director and content rating, where director_name 
#    is the explanatory variable. Does there appear to be an association 
#    between director and the content_rating of their movies? 
#    Why or why not? [8 points]


# 7) Construct side-by-side boxplots showing the distribution of IMDB scores
#    for each director. Does there appear to be an association between
#    director and imdb rating? Would you say that one director is more 
#    successful than the others (based on imdb score)? After answering the 
#    question, add the following layer to your plot: 
#    
#    facet_grid(vars(content_rating)))
#
#    This will construct separate side-by-side boxplots for each content 
#    rating. Describe the results. Is one director more successful than 
#    the others? Is a director's success associated with content rating?
#    [8 points]

# The code below adds 2 columns to movies, containing the budget and
# gross earnings in millions of dollars. Note that 'e' denotes scientific
# notation in R, so 1e6 = 1 x 10^6 = 1,000,000. Run this code to update
# the movies data frame and then answer the questions below.

movies <- movies %>% mutate(gross_millions = gross / 1e6, 
                            budget_millions = budget / 1e6)

# 8) Construct a histogram (using the 'hist' function) of the gross earnings 
#    for these movies, in millions of dollars. Describe the shape of the 
#    histogram. In addition, what is the mean gross and what is the 
#    median? Which is a better measure of center, and why? [8 points]


# 9) The code below generates a scatter plot that predicts gross earnings
#    (in millions) from budget (in millions), and adds the regression line. 
#    Fit a linear model that predicts gross earnings (in millions) from 
#    the budget (in millions). Note that the fitted linear model is 
#    stored as a list, and the element named 'coefficients' includes the 
#    y-intercept and slope. Extract these values and store them in the 
#    variables 'y.int' and 'slope', and use the 'round' function to round
#    these to 2 decimal places. These values will be added to the plot.
#    Interpret the slope and y-intercept in the context of
#    this problem. Note that the correct values are 73.22 for the y-intercept
#    and 1.26 for the slope. [8 points]

# TO DO: fit the linear model and extract the slope and y-intercept
slope <- 'a'
y.int <- 'b0'

# create a string containing the equation of the regression line
equation <- paste0('gross = ', y.int, ' + ', slope, '(budget)') 

ggplot(movies, aes(budget_millions, gross_millions)) + 
  geom_point() +
  theme_linedraw() + xlab('Budget, millions of dollars') + 
  ylab ('Gross, millions of dollars') + 
  ggtitle('Budget vs gross earnings') +
  annotate(geom="text", x = 0, xmin=50, y=650, 
           label= equation, color="blue", size = 7, hjust = 0) + 
  geom_smooth(method = 'lm')


# 10) What gross earnings would you predict for movies that have budgets 
#     of $100 million, $400 million, and $50 million? [8 points]


# 11) As seen in (7), sometimes the association between two variables
#     is influenced by a third. Create a scatterplot as in the previous
#     question, but set the color aesthetic of ggplot to content_rating. 
#     Now add the regression line, and a regression line will be added for 
#     each rating. When adding the regression line, set se = FALSE and
#     fullrange = TRUE, which removes the error bars and extends each
#     line across the length of the plot. Interpret the results. What 
#     gross do you expect for low budget movies (< 50 million).
#     Would you recommend, based on this data, to increase the budget of 
#     a PG-13 movie? What about a PG movie?  [6 points]


# 12) The code below uses 'facet_grid' to generate a scatterplot of
#     budget (in millions) vs gross (in millions) for each rating.
#     This makes clear that the relationship within each content_rating is linear 
#     (except for G movies, since there is only 1 movie).

ggplot(movies, aes(budget_millions, gross_millions, color = content_rating)) + 
  geom_point()+ geom_smooth(method = 'lm', fullrange = TRUE) + 
  facet_grid(vars(content_rating)) + ggtitle('Budget vs gross earnings')

#     (a) Use 'filter' and 'select' to display only the director_name,
#         movie_title, and title_year of the movie which is rated 'G'
#         [4 points]

#     (b) The code below splits the data by content_rating, which 
#         returns a list. Each element of the list is a data frame
#         containing movie information for movies with a particular 
#         rating. Use the 'sapply' function to find the number of movies 
#         for each rating. Hint: what function do you want to apply to 
#         each data frame in 's'? [4 points]

          s <- split(movies, movies$content_rating)  

#     (c) Write a function called 'correlation' that has a single input,
#         which is a data frame (like movies) for columns with 
#         budget_millions and gross_millions. The function finds the
#         correlation between budget_millions and gross_millions and 
#         returns it. Note that when you call 'cor', you should set
#         use = "complete.obs" to ignore missing values; otherwise
#         the "cor" function will return NA. Use this function to 
#         find the correlation of budget and gross (millions) for all
#         movies in the 'movies' data frame. Then use 'sapply' to find
#         the correlation for each content_rating. Interpret the results.
#         [6 points]          
          

# 13) EXTRA CREDIT

# On Lab 1, you were asked to filter the students to keep only those 
# with a college GPA >= 3.5.

library(readr)
library(dplyr)

survey <- read_delim("https://gdancik.github.io/CSC-315/data/datasets/survey.txt",
                     "\t", escape_double = FALSE, trim_ws = TRUE)

surveyA <- filter(survey, "College GPA" >= 3.5)

# The code above runs but does not give the correct answer. What does 
# this code do? And why?
  
  