##########################################################################
# CSC 315, Exam I Review 
##########################################################################

library(dplyr)
library(ggplot2)
library(readr)

# The remaining questions are based on the IMDB 5000 dataset, which contains
# information from around 5,000 movies scraped from IMDB. The dataset is
# available at https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset,
# and can be read in using the command below:

movies5000 <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/movie_metadata.csv")

# Let's limit our analysis to movies directed by either James Cameron,
# Steven Spielberg, Christopher Nolan, or Robert Zemeckis. Run the code
# below to filter the movies based on these directors, and use this
# data frame for the remaining problems

movies <- movies5000 %>% filter(director_name %in% 
                              c("James Cameron", "Steven Spielberg", 
                                "Christopher Nolan", "Robert Zemeckis" ))


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
  
  