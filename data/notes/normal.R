##################################################################
# The normal distribution
##################################################################

library(ggplot2)
library(dplyr)
library(cowplot)

##################################################################
# create a vector of heights
##################################################################
heights <- c(68.54, 69.85, 69.8, 67.56, 67.36, 64.63, 72.24, 67.34, 64.24, 
    67.22, 69.73, 70.52, 66.47, 67.64, 66.78, 66.89, 66.75, 65.95, 
    64.11, 64.88, 68.91, 68.52, 68.23, 67.3, 67.62, 67.97, 66.11, 
    63.65, 66.85, 67.13, 71.15, 65.08, 67.86, 68.23, 72.65, 68.87, 
    65.69, 66.54, 68.88, 69.79, 70.04, 65.87, 65.21, 69.82, 71.25, 
    68.09, 65.69, 70.82, 66.96, 68.16, 65.53, 70.05, 67.75, 69.1, 
    64.05, 66.11, 68.35, 64.81, 68.85, 69.54, 67.7, 69.68, 68.58, 
    67.29, 69.82, 66.73, 70.81, 66.95, 67.88, 66, 68.43, 65.33, 66.94, 
    66.27, 66.8, 69.81, 67.86, 70.15, 66.94, 68.07, 69.41, 67.83, 
    69.06, 69.38, 65.97, 69.41, 68.87, 71.95, 67.16, 68.47, 65.86, 
    67.48, 66.04, 62.36, 67.06, 69.63, 73.08, 67.33, 68.25, 67.35)

##################################################################
# This function constructs a histogram of X while optionally shading
# all values corresponding to P(X <= obs), and optionally adding
# a density curve if density = TRUE
# ... are additional arguments to hist
##################################################################
shaded.hist <- function(X, hist = TRUE, density = FALSE, obs = NULL, ...) {
  h <- hist(X, plot = FALSE)
  col <- rep("white", length(h$breaks))
  ylim <- NULL
  if (density) {
    m <- max( max(h$density), dnorm(mean(X), mean = mean(X), sd = sd(X)))
    m <- m +0.1*m  
    ylim <- c(0,m)
  }
  
  if (hist & !is.null(obs)) {
     col[h$breaks<obs] = "red"
  }
  lty <- 1
  if (!hist) lty <- 0
  hist(X, prob = TRUE, ylim = ylim, col = col, lty = lty, ...)
  
  if (density) {
    curve(dnorm(x, mean = mean(X), sd = sd(X)), col = "blue", lty = 2, lwd = 2, add = TRUE)
  }
}


#################################################
## If an individual is randomly selected, what 
## is the probability that their height is less 
## than or equal to 67 inches?
#################################################

hist(heights, prob = TRUE, main = "Histogram of Heights", xlab = "height")

p <- sum(heights <= 67) / length(heights)
p


#####################################################
## We can visualize this on a histogram. Let
## X be the height of a randomly selected individual
## P (X <= 67) is the cumulative density of
## the red bars, while P (X > 67) is the 
## cumulative density of the white bars, or
## 1 - P(X <=67)
#####################################################
shaded.hist(heights, obs = 67, main = "Histogram of Heights", xlab = "height")

#####################################################
## What is the probability P (67 < X < 70)?
## We can take:  P(X <=70) - P(X <= 67)
#####################################################
par.orig <- par(mfrow = c(2,1), mar = c(3,4,2,0))
shaded.hist(heights, obs=70, main = "Histogram of Heights", xlab = "height")
shaded.hist(heights, obs=67, main = "Histogram of Heights", xlab = "height")
par(par.orig)

###################################################################
# In practice, probability distributions for continuous random 
# variables are visualized by curves which (approximately) 
# reflect the distribution (shape of the data) and probability 
# density across the sample space. The cumulative density is 
# equivalent to the area under the curve
###################################################################

shaded.hist(heights, density = TRUE, obs = 70, main = "Histogram of Heights With Normal Approximation", xlab = "height")

###################################################################
# Several distributions are given below. However we will focus
# on the most widely used distribution, the normal distribution
# (or bell curve)
###################################################################

# set up data frames with (x,y) values for various distributions
# for given 'x' values, use 'mutate' to add the corresponding 'y' value (density)
data.unif <- data.frame(x = seq(0,1,by=.1)) %>% mutate(y = dunif(x))
data.exp <- data.frame(x = seq(0,10,by=.1)) %>% mutate(y = dexp(x))
data.chisq <- data.frame(x = seq(0,10,by=.1)) %>% mutate(y = dchisq(x, df = 3))
data.norm <- data.frame(x = seq(0,10,by=.1)) %>% mutate(y = dnorm(x, mean = 5, sd = 1))

# generate each plot
plot.unif <- ggplot(data.unif, aes(x, y)) + geom_line(color = "blue", lty = 2) +
  ggtitle("Uniform Distribution") + labs(x = "x", y = "density") + theme_classic()
plot.exp <- ggplot(data.exp, aes(x, y)) + geom_line(color = "blue", lty = 2) +
  ggtitle("Exponential Distribution") + labs(x = "x", y = "density") + theme_classic()
plot.chisq <- ggplot(data.chisq, aes(x, y)) + geom_line(color = "blue", lty = 2) +
  ggtitle("Chi-Square Distribution") + labs(x = "x", y = "density") + theme_classic()
plot.norm <- ggplot(data.norm, aes(x, y)) + geom_line(color = "blue", lty = 2) +
  ggtitle("Normal Distribution") + labs(x = "x", y = "density") + theme_classic()

# display multiple plots on a grid (from cowplot)
plot_grid(plot.unif, plot.exp, plot.chisq, plot.norm)


###################################################################
# The normal distribution is completely specified by its mean
# and standard deviation
###################################################################

# normal distribution with mean = 60 and sd = 72
d1 <- data.frame(x = seq(60,72,by=.1)) %>% mutate(y=dnorm(x, mean = 66, sd = 2))
p1 <- ggplot(data = d1, aes(x, y)) + geom_line() +
  ggtitle("Normal Distribution\n(mu = 66, sigma = 2)")  + ylab("density") +
  theme_classic()

# normal distribution with mean = 0 and sd = 1 (i.e., standard normal distribution)
d2 <- data.frame(x = seq(-3,3,by=.1)) %>% mutate(y=dnorm(x, mean = 0, sd = 1))
p2 <- ggplot(data = d2, aes(x, y)) + geom_line() +
  ggtitle("Standard Normal Distribution\n(mu = 0, sigma = 1)")  + 
  ylab("density") + xlab("z") + theme_classic()

# plot both distributions
plot_grid(p1, p2, nrow = 2)

########################################################################
# Suppose heights are normally distributed with mean = 68 and 
# sd = 1.7 inches. Find the probability that a randomly selected 
# person is less than (or equal to) 70 inches tall 
#
# If X ~ N(mu, sd), then 
#     pnorm(a, mean = mu, sd = sd) returns P(X <= a) 
#
########################################################################

pnorm(70, mean = 68, sd = 1.7)


########################################################################
# Shades the area under the normal curve between a and b 
# This area corresponds to P (a <= x <= b) when X ~ N (mean, sd)
# For P (x <= b) set a = -Inf
# For P (x >= a) set b = Inf

# Note: This function is provided for demonstration purposes only.
# You should NOT use this function to answer probability
# questions on assignments/exams. Instead, you should just use 
# 'pnorm'.
########################################################################
shade.norm <- function(a,b, mean = 0, sd = 1,  ...) {
  m1 <- mean-4*sd
  if (!is.infinite(a)) m1 = min(a,m1)
  m2 <- mean+4*sd
  if (!is.infinite(b)) m2 = max(b,m2)
  
  x <- seq(m1,m2, length.out = 100)
  plot(x, dnorm(x, mean = mean, sd = sd), type = "l", ylab = "normal density", ...)
  if (is.infinite(a)) a <- m1
  if (is.infinite(b)) b <- m2
  r <- seq(a,b,length.out=100)
  cord.x <- c(a,r,b)
  cord.y <- c(0,dnorm(r, mean = mean, sd = sd),0)
  polygon(cord.x,cord.y,col='skyblue')
  abline(h = 0)

  p <- pnorm(b, mean = mean, sd = sd) - pnorm(a, mean = mean, sd = sd)
  legend("topleft", legend = paste("p = ", round(p,3), sep = ""))
  
}


########################################################################
# Assume that X ~ N(68, 1.7).
# 1. Find P(X <= 68)
# 2. Find P(X <= 65)
# 3. Find P(X > 65)
# 4. Find P(64.6 <= X <= 71.4)
########################################################################

#1. P(X <= 68)

# visualization of the area under the curve we want to find
shade.norm(-Inf, 68, mean = 68, sd = 1.7, 
           main = "X ~ N(68,1.7)\nP(X <= 68)")  

# calculation of the probability
pnorm(68, mean = 68, sd = 1.7)

#2. P(X <= 65)

# visualization of the area under the curve we want to find
shade.norm(-Inf, 65, mean = 68, sd = 1.7,
           main = "X ~ N(68, 1.7)\nP(X<=65)") 

# calculation of the probability
pnorm(65, mean = 68, sd = 1.7)

#3. P(X > 65)

# visualization of the area under the curve we want to find
shade.norm(65, Inf, mean = 68, sd = 1.7,
           main = "X ~ N(68, 1.7)\nP(X>65)")  # or

# calculation of the probability
1 - pnorm(65, mean = 68, sd = 1.7)

# for probabilies of the form P(X > a), you can
# also use pnorm with lower.tail = FALSE
pnorm(65, mean = 68, sd = 1.7, lower.tail = FALSE)

#4. P(64.6 <= X <= 71.4)

# visualization of the area under the curve we want to find
shade.norm(64.6, 71.4, mean = 68, sd = 1.7,
           main = "X ~ N(68, 1.7)\nP(64.6 <= X <= 71.4)") 

# calculation of the probability
pnorm(71.4, mean = 68, sd = 1.7) - pnorm(64.6, mean = 68, sd = 1.7)


########################################################################
# Percentiles (quantiles) from the normal distribution
# Assume still that X ~ N(68, 1.7)
# What is the 75th percentile?
# What is the 90th percentile?
########################################################################

qnorm(.75, mean = 68, sd = 1.7)
qnorm(.90, mean = 68, sd = 1.7)

########################################################################
# Verify 90th percentile
########################################################################
q <- qnorm(.90, mean = 68, sd =1.7)
shade.norm(-Inf, q, mean = 68, sd = 1.7, 
           main = "90th percentile of X ~ N(68, 1.7)")


########################################################################
# Suppose that X is normally distributed. Find the probability
# that a randomly selected value from X is more than 2 standard
# deviations above the mean.
########################################################################

# Because Z = (X- mu) / sigma  is the number of standard 
# deviations from the mean, we can calculate this probability
# using Z ~ N(0,1). Importantly, we do not need to know 'mu'
# or 'sigma' for this calculation!

# Mathematically, X > 2 standard deviations above the mean is 
# equivalent to P(Z > 2) which can be calculated as:
#   1 - P(Z < 2)

1 - pnorm(2, mean = 0, sd = 1)

# by default, pnorm uses mean = 0 and sd = 1
1 - pnorm(2)
