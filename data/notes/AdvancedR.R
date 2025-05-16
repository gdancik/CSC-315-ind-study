###################################
## writing functions in R
###################################

## example with two arguments ##
divide <- function(x,y) {
    return(x/y)
}
divide(3,4)

## example with a default argument ##
divide <-function(x,y = 1) {
  return(x/y)
}

### arguments are matched in order unless they are named
divide(3)         # 3 / 1 (since 1 is default y-value)
divide(3,2)       # 3/2 = 1.5
divide(2,3)       # 2/3 = .666
divide(y = 2, 3)  # 3/2 = 1.5

###################################################
# return is implicit if the function ends with
# an expression (or an assignment)
##################################################

# same divide function as above, with implicit return
divide <-function(x,y = 1) {
  x/y
}

divide(3,2) # 3/2 = 1.5

#################################################
## for loops exist in R, but there is almost
## never a good reason to use them, and they
## should not ever be used in this class!
#################################################

xvalues <- c(1,3,2,9)

# Suppose I want to increase each element of 'xvalues' by 1

# You can use a for loop, but this would be considered
# bad practice since it is less readable and less efficient
xvalues <- 1:10
for (i in 1:length(xvalues)) {
  xvalues[i] <- xvalues[i] + 1
}
xvalues

# Because R lets you do this:
xvalues <- 1:10
xvalues <- xvalues + 1
xvalues


######################################################################
# In R, a 'functional' is a function that takes another function
# as an input
#####################################################################

# here we return a vector containing f1(x) and f2(x)
calc_statistics <- function(x, f1, f2) {
  c(f1(x), f2(x))
}

mydata <- c(1:10,13)

# calculate mean and median
calc_statistics(mydata, mean, median)

# calculate mean and standard deviation
calc_statistics(mydata, mean, sd)

#####################################################################
# Instead of loops, the functionals apply, lapply, and sapply
# should be used to apply a function to each element of a vector or 
# list. Functional approaches are simpler and more readable, once 
# understood. 

# apply - applies a function to each row (MARGIN = 1) or column (
#         MARGIN = 2) of a matrix (or data frame)
#
# lapply - applies a function each element of a vector or list, and
#          returns a list

# sapply - like lapply, but returns a vector if the applied function
#          returns a single value
######################################################################

######################################################################
## apply - applies a function to a row (MARGIN = 1) 
## or column (MARGIN = 2) of a matrix or data frame. 
######################################################################

###########################################
# example: find max of each row of 'm'
###########################################
m <- matrix(1:20, ncol=5, byrow=TRUE)
m
rowMaxes <- apply(m, 1, max)
rowMaxes

###############################################################
## add the 'n' smallest numbers in a vector
## Note: could also return sum(sort(x)[1:n])
###############################################################
add.smallest <- function(x, n=2) {
  x.sorted <- sort(x)
  sum(x.sorted[1:n])
}

ans.row <- apply(m, 1, add.smallest) ## for each row
ans.row

ans.col <- apply(m, 2, add.smallest) ## for each col
ans.col

# additional arguments can be included after the function name
# (Note: additional arguments should not be called 'x' or 'X')
ans.row.4 <- apply(m, 1, add.smallest, n = 4)
ans.row.4

# function to add the 2 smallest values of a vector
add2Smallest <- function(x) {
  sum(sort(x)[1:2])
}

# use apply with named function, applied to each row
apply(m, 1, add2Smallest)

## alternative format using an inline (anonymous) function
apply(m, 1, function(x) {
    sum(sort(x)[1:2])
})

## another alternative format using an inline (anonymous) function
apply(m, 1, function(x) sum(sort(x)[1:2]))


######################################################################
## lapply applies a function to each object in a list or vector, 
## and returns a list
######################################################################
person <- list(name = "Bob", sibling.ages = c(43,21), pet.ages = c(8,3))
lapply(person, length)

## sapply does the same but returns a vector
sapply(person, length)

#############################################
# if statements - follows C/C++/Java format
#############################################
compare <-function(x, ref = 0) {
  if (x < ref) {
    cat("the number", x, "is less than", ref, "\n")
  } else if (x > ref) {
    cat("the number", x, "is greater than", ref, "\n")
  } else {
    cat("the number", x, "is equal to", ref, "\n")
  }
}

compare(5, 3)


#############################################
# Facet grid - with ggplot, facet_grid can
# be used to generate separate plots based
# on the value of one or more variables
#############################################

library(ggplot2)
ggplot(iris, aes(x=Petal.Width, y=Petal.Length)) +
  geom_point(aes(color = Species)) +
  ggtitle("Petal Length vs. Petal Width from Iris dataset") +
  labs(x = "Petal Width", y = "Petal Length") + theme_linedraw()

# use facet_grid to generate separate plots based on Species

ggplot(iris, aes(x=Petal.Width, y=Petal.Length, color = Species)) +
  geom_point() +
  ggtitle("Petal Length vs. Petal Width from Iris dataset") +
  labs(x = "Petal Width", y = "Petal Length") + theme_linedraw() +
  facet_grid(rows = vars(Species))


#######################################################################
# Question set (use the grades matrix below):
#######################################################################

grades <- matrix(c(71,86,82,93,87,92,85,85,98,99,100,92),ncol=3,byrow=T)
rownames(grades) <- c("Steve", "Joe", "Jane", "Andrea")
grades

# 1. Using the apply function, find the following:

# (a) mean grade for each student (rows)

# (b) mean grade for each assignment (columns)

# (c) median grade for each assignment


################################################
# Additional Practice Problems
################################################

# 1. Write a function called min.positive which takes a vector 'x' as an
#    argument and returns the smallest positive number from the vector.
#    For example, for v <- c(-3,10,2), min.positive(v) would return 2

# 2. Write a function called min.max which takes a vector x as an argument 
#    and returns a list containing two named elements, 
#    the minimum of x and the maximum of x. Use this function to find
#    the minimum and maximum of the vector below:

v <- c(1:10, -13, 90, 2:20)

# 3. Include the following code in your script to create a matrix 
#    Use the 'apply' function to find the standard deviation of each row
#    What is the standard deviation of the last row? Why?

m = matrix(c(1:30, rep(3,5)), ncol = 5, byrow = TRUE)

