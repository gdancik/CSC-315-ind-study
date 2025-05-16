##################################################################
# sampling distributions - in a random selection of observations,
#    the sample mean is a random variable with a probability
#    distribution
##################################################################

library(ggplot2)
library(gtools)

####################################################
# for a given population of student ages, draw a 
# histogram and add a line for the population mean
####################################################
x <- c(21,19,21,21,18)

## calculate population mean, which is 20 ##
mean(x)   

## draw histogram ##
xlim <- c(min(x), max(x))
breaks <- seq(17.5, 21.5, by= 1)
par.orig <- par(mfrow = c(3,1), mar = c(3,3,2,1))
hist(x, prob = TRUE, xlim = xlim, 
     main = "Population distribution of x", breaks = breaks)
abline(v = mean(x), col = "red", lwd = 3)

###################################################################
## What are the possible mean values if we randomly select 3
## students? Generate the sample space, find the possible
## sample means, and construct a histogram. Also label the
## average (expected value) for the sample mean
###################################################################

## Find the sample space for selecting n = 3 students. 
##    We use combinations, since order does not matter, and
##      use the following arguments:
##        - set = FALSE because x contains duplicate values
##            that we don't want removed
##        - repeats.allowed = FALSE because once one student
##            is selected, we do not select them again

S <- combinations(length(x), 3, x, set = FALSE, repeats.allowed = FALSE)
S

## get possible sample means and draw histogram
sample.means <- rowMeans(S)
hist(sample.means, xlim = xlim, prob = TRUE, breaks = breaks,
     main = "Probability distribution of xbar when n = 3")
expected.value <- mean(sample.means) ## 20, the same as the population mean!
abline(v = expected.value, col = "red", lwd = 3)

### repeat above analysis for n = 4 ###
S <- combinations(length(x), 4, x, set = FALSE, repeats.allowed = FALSE)
sample.means <- rowMeans(S)
hist(sample.means, xlim = xlim, prob = TRUE, breaks = breaks,
     main = "Probability distribution of xbar when n = 4")
expected.value <- mean(sample.means) ## 20, the same as the population mean!
abline(v = expected.value, col = "red", lwd = 3)

par(par.orig)


####################################################################
# Key observations/facts: 

# Let X be a random variable with mean mu and standard deviation 
# sigma. 

# Let Xbar be the sample mean calculated from 'n' independent
# samples of X. Then Xbar has mean mu and standard deviation
# sigma / sqrt(n)

####################################################################


###################################################################
## Central Limit Theorem when X ~ normally distributed
###################################################################


# helper function that constructs histogram and adds vertical 
# red line for mean 
plot.hist <-function(x, ...) {
  hist(x, ...)
  abline(v = mean(x), lwd = 3, col = "red")
}


###################################################################
## returns the sample mean from 'n' randomly generated observations 
## from the standard normal distribution
###################################################################
get.sample.mean <- function(n) {
  r <- rnorm(n)
  mean(r)
}

x.population <- rnorm(1000)
x.10 <- replicate(5000, get.sample.mean(10))
x.30 <- replicate(5000, get.sample.mean(30))
x.100 <- replicate(5000, get.sample.mean(100))

xlim <- c(min(x.population), max(x.population))
par.orig <- par(mfrow = c(2,2), mar = c(3,3,2,1))
plot.hist(x.population, main = "Distribution of X", xlim = xlim)
plot.hist(x.10, main = "Distribution of sample mean (n = 10)", xlim = xlim)
plot.hist(x.30, main = "Distribution of sample mean (n = 30)", xlim = xlim)
plot.hist(x.100, main = "Distribution of sample mean (n = 100)", xlim = xlim)
par(par.orig)

# use default x-range
plot.hist(x.100, main = "Distribution of sample mean (n = 100)")


###################################################################
## Central Limit Theorem when X ~ NOT normally distributed!
## we will look at the exponential distribution, which is skewed
###################################################################

###################################################################
## returns the sample mean from 'n' randomly generated observations 
## from the exponential distribution
###################################################################
get.sample.mean <-function(n) {
  r <- rexp(n)
  mean(r)
}

x.population <- rexp(1000)
x.10 <- replicate(5000, get.sample.mean(10))
x.30 <- replicate(5000, get.sample.mean(30))
x.100 <- replicate(5000, get.sample.mean(100))


xlim <- c(min(x.population), max(x.population))
par.orig <- par(mfrow = c(2,2), mar = c(3,3,2,1))
plot.hist(x.population, main = "Distribution of X", xlim = xlim)
plot.hist(x.10, main = "Distribution of sample mean (n = 10)", xlim = xlim)
plot.hist(x.30, main = "Distribution of sample mean (n = 30)", xlim = xlim)
plot.hist(x.100, main = "Distribution of sample mean (n = 100)", xlim = xlim)
par(par.orig)

# use default x-range to show sampling distribution is
# approximately normal
plot.hist(x.100, main = "Distribution of sample mean (n = 100)")

###################################################################
## Central Limit Theorem in practice:
## Suppose that heights have the distribution X ~ N(68, 1.7).
## We will calculate probabilities involving the sample mean 
## from a sample of 20 individuals
###################################################################

###################################################################
## Visualization
###################################################################

x <- seq(62, 74, by = .2)
y1 <- dnorm(x, mean = 68, sd = 1.7) ## density of x
y2 <- dnorm(x, mean = 68, sd = 1.7 / sqrt(20)) ## density of sample mean when n = 20
df <- data.frame(x = x, y1 = y1, y2 = y2)

ggplot(df) + geom_line(aes(x,y1, color = "y1")) + 
  geom_line(aes(x,y2, color = "y2")) +
  ggtitle("Distribution of heights") + ylab("density") +
  scale_colour_manual(breaks = c("y1", "y2"),
                    values = c("black", "red"),
                    labels = c("X ~ N(68, 1.7)", "Xbar, n = 20 ~ N(68, 1.7/sqrt(20))")) +
  theme_classic() + theme(legend.position = c(.22,.9), 
                          legend.box.background = element_rect(),
                          legend.title = element_blank())

###############################################################
# Questions:
###############################################################

# Find P(X < 69)


# Find P(X > 67)


# In a random sample of 20 individuals, find the probability
# that the sample mean is less than 69:



# In a random sample of 20 individuals, find the probability
# that the sample mean is greater than 67


# In a random sample of 20 individuals, find the probability
# that the sample mean is between 67 and 67.5.

