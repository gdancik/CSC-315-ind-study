library(dplyr)
library(ggplot2)

####################################################################
## Graphical summaries of data
####################################################################
status <- c("first-year", "first-year", "sophomore", "sophomore", "junior", "sophomore")

#################################################
# frequency table and relative frequency tables #
#################################################
t <- table(status) # frequency table
t

# use prop.table to convert a frequency table to a relative frequency table
prop.table(t)


##################################################################
# Look at ggplot2.R before proceeding
##################################################################

##################################################################
# bar graphs construct bars whose heights correspond to 
# frequencies or relative frequencies
##################################################################

# To have 'ggplot' count the data for you, use
# the 'geom_bar' layer. 
d.status <- data.frame(status = status)
ggplot(d.status, aes(x=status, fill = status)) + geom_bar() +
           ggtitle("Class status of students") +
            labs(x = "Class status", y = "Frequency") + theme_classic()


# To generate a frequency bar graph from the counts, use
# the 'geom_col' layer.
t <- table(status)
counts <- data.frame(t)
ggplot(counts, aes(x=status, y=Freq, fill = status)) + geom_col() +
  ggtitle("Class status of students") +
  labs(x = "Class status", y = "Frequency") + theme_bw()

# To generate a relative frequency bar graph from the raw data,
# set y = ..count../sum(..count..)
ggplot(d.status, aes(x=status, y=..count.. / sum(..count..), fill = status)) + 
  geom_bar() +
  ggtitle("Class status of students") +
  labs(x = "Class status", y = "Relative frequency") + theme_classic()


##################################################################
# Construct a Pareto chart to show bars from tallest to shortest
##################################################################

# bars are ordered in alphabetical (factor-level) order by default
levels(counts$status)

# reorder based on counts (the default is to re-order in increasing order,
# so we need to set decreasing to TRUE)
counts$status <- reorder(counts$status, counts$Freq, decreasing = TRUE)

ggplot(counts,aes(x=status, y=Freq, fill = status)) + 
  geom_col() +
  ggtitle("Class status of students") +
  labs(x = "Class status", y = "Frequency") + theme_classic()

##################################################################
# Let's look at the survey data (from a prior class)
# that we analyzed previously
##################################################################

library(readr)
survey <- read_delim("https://gdancik.github.io/CSC-315/data/datasets/survey.txt", "\t")

###################################################################         
# Question: Construct a relative frequency table for the number of males
#     and females
###################################################################




# Note, below is an alternative way of generating a frequency and
# relative frequency table using 'dplyr'

# frequency table
survey %>% group_by(Gender) %>% summarize(Frequency = n())
              
# add relative frequencies
survey %>% group_by(Gender) %>% summarize(Frequency = n()) %>% 
  mutate(Proportion = prop.table(Frequency))


###################################################################
# Histograms are like bar graphs but for quantitative variables, 
# where the height of the bar corresponds to the number or 
# proportion of observations that fall within a range of values
###################################################################

## we will use the 'base' hist function, rather than the ggplot one
hist(survey$FB, main = "histogram of FB usage", xlab = "Hours/week on FB")

###########################################################
# histogram shapes
# unimodal = 1 mound or mode
# bimodal = 2 mounds or modes
# skewed left: tail is longer (or fatter) on the left
# symmetric: tails are approximately the same length
# skewed right: tail is longer (or fatter) on the right
###########################################################

###########################################################
# The R code below is used to demonstrate various 
# histogram shapes. You do not need to understand the R 
# code at this point, but you do need to be able to 
# distinguish between histogram shapes
###########################################################

x.norm <- rnorm(1000)
x.right <- c(x.norm, runif(30,3,10))
x.left <- c(x.norm, runif(30,-10,-3))
par(mfrow = c(1,3))
hist(x.left, main = "left-skewed")
hist(x.norm, main = "symmetric")
hist(x.right, main = "right-skewed")

dev.off()  # reset display to default (single figure)
bimodal <- c(rnorm(500, mean = 70), rnorm(500, mean = 65))
hist(bimodal, main = "bimodal distribution")

