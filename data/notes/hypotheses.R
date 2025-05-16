####################################################
# Intro to hypothesis testing:
####################################################

###################################################################
# Suppose I want to determine if a coin is biased. 
# I'll flip the coin 100 times, if the proportion of heads I get
# is much more or less than I'd expect by chance, then
# I conclude the coin is biased.
###################################################################

# What proportion of heads do I expect to get by chance? 
# We will use simulations to get the empirical distribution 
# of this proportion
prop.heads <- function(n) {
  tosses <- sample(c("H", "T"), n, replace= TRUE)
  proportion <- sum(tosses == "H") / length(tosses)
  return(proportion)
}


p.hat = replicate(5000, prop.heads(100))
hist(p.hat, prob = TRUE, xlab = "proportion of heads (p.hat)", 
     main = "Proportion of Heads in 100 Tosses")

###############################################################
# Fact: if p.hat is the distribution of a sample proportion 
# calculated from a population with mean mu = p, then its 
# standard deviation is sigma / sqrt(n), where sigma = 
# sqrt(p(1-p)) and n is the sample size. Furthermore, 
# the distribution is approximately normal!
###############################################################

####################################################
# histogram with normal distribution
####################################################
hist.norm <- function(X, mu, sigma, ylim = NULL, ...) { 
  h = hist(X, plot = FALSE)
  col = rep("white", length(h$breaks))
  
  m = max( max(h$density), dnorm(mu, mean = mu, sd = sigma))
  m = m +0.1*m  
  if (is.null(ylim)) {
    ylim = c(0,m)
  }
  
  hist(X, prob = TRUE, ylim = ylim, col = col, ...)
  
  curve(dnorm(x, mean = mu, sd = sigma), col = "red", lty = 2, lwd = 2, 
        add = TRUE)
}

n <- 100 
p <- 0.5
mu <- p
sigma <- sqrt(p*(1-p))

hist.norm(p.hat, mu, sigma / sqrt(n), main = "Proportion of Heads in 100 Tosses",
          xlab = "proportion of heads (p.hat)", ylim = c(0,10))
legend("topleft", legend = "p.hat ~ N(p, sqrt((p(1-p))/n)\nand p=0.5",
       lwd = 2, lty = 2, col = "red", cex = .7)

#################################################
# The variable p.hat can be transformed
# Let Z = (p.hat - mu) / (sigma / sqrt(n)),
# and Z ~ N(0,1)
#################################################

#################################################
# Let's look at the distribution of Z and 
# compare this to the distribution of p.hat
# Let's also assume that when we flipped a coin
# 100 times, we got heads 62% of the time
#################################################

###############################################################
# observed.Z is our test statistic which follows
# a known distribution under the null hypothesis
# The p-value is the probability that under the null 
# hypothesis, the test statistic exceeds its value in 
# either direction

# You only need to understand the following from the code 
# below: The distribution of p.hat is (approximately) 
# normal with mean of p, and standard deviation of 
# sqrt(p(1-p)/n); we can calculate the z-value of p.hat by
# subtracting the mean and dividing by its standard deviation
###############################################################

Z <- (p.hat - mu) / (sigma / sqrt(n))
observed <- 0.62
observed.Z <- (observed - mu) / (sigma / sqrt(n))

## graphing parameters
m1 <- min(p.hat) - .05
m2 <- max(p.hat) + .05
breaks <- seq(m1, m2,by=.05)
breaksZ <- (breaks - mu) / (sigma / sqrt(n))
xlim <- c(0.3, 0.7)
xlimZ <- (xlim - mu) / (sigma / sqrt(n))
par.orig <- par(mfrow = c(2,1), mar = c(4,4,2,1))
hist.norm(p.hat, mu, sigma / sqrt(n), xlim = xlim, 
          main = "Distribution of p.hat ~ N(p, sqrt((p(1-p))/n))",
          xlab = "p.hat", breaks = breaks)
abline(v = observed)
text(0.62, 6, "p.hat = 0.62")
hist.norm(Z, 0, 1 , xlim = xlimZ, 
          main = "Distribution of Z ~ N(0,1)",
          xlab = "Z", breaks = breaksZ)
abline(v = observed.Z)
text(observed.Z, 0.4, "Z = 2.4")
par(par.orig)

# Note the relationship between the distributions! 
# P(p.hat > 0.62) is the same as P(Z > 2.4)

########################################################################
# Shades the area under the normal curve between a and b 
# This area corresponds to P (a <= x <= b) when X ~ N (mean, sd)
# For P (x <= b) set a = -Inf
# For P (x >= a) set b = Inf
########################################################################
shade.norm <- function(a,b, mean = 0, sd = 1,  ...) {
  m1 = mean-4*sd
  if (!is.infinite(a)) m1 = min(a,m1)
  m2 = mean+4*sd
  if (!is.infinite(b)) m2 = max(b,m2)
  
  x = seq(m1,m2, length.out = 100)
  plot(x, dnorm(x, mean = mean, sd = sd), type = "l", ylab = "normal density", ...)
  if (is.infinite(a)) a = m1
  if (is.infinite(b)) b = m2
  r = seq(a,b,length.out=100)
  cord.x <- c(a,r,b)
  cord.y <- c(0,dnorm(r, mean = mean, sd = sd),0)
  polygon(cord.x,cord.y,col='skyblue')
  abline(h = 0)
  
  p = pnorm(b, mean = mean, sd = sd) - pnorm(a, mean = mean, sd = sd)
  legend("topleft", legend = paste("p = ", round(p,3), sep = ""))
  
}

#####################################################
# p-value is area in unshaded region, corresponding
# to the probability of observing a test statistic
# more extreme (in either direction) than what is
# expected by chance (assuming H0 is true)
#####################################################
pos.observed.Z <- abs(observed.Z)
neg.observed.Z <- -pos.observed.Z
shade.norm(neg.observed.Z, pos.observed.Z)
abline(v = neg.observed.Z)
abline(v = pos.observed.Z)

p.value <- 2*(pnorm(neg.observed.Z))
p.value

#####################################################
# If p-value is small (generally < 0.05), we
# reject the null hypothesis and accept the
# alternative. This is because the observed test
# statistic is not likely under the null hypothesis
#####################################################

##################################################
# hypothesis test for single proportion in R
#################################################
# compare this to p-value above 
p1 <- prop.test(62, 100, correct = FALSE)
p1$p.value

# Note: we use double brackets to select the first
# element and drop the name, which is no longer relevant
sqrt(p1$statistic[[1]]) # the Z test statistic

# conclusion:
#  Because p-value < 0.05, we reject H0 and accept H1
#  We have sufficient evidence to conclude that the
#  coin is biased

###################################################
# Need for continuity correction:
# There is a limitation with the normal approximation,
# because we are using a continuous distribution
# to approximate a discrete random variable

# Consider the uniform probability distribution 
# for selecting a number X between 1 and 4

# What is P(X <= 2) 
###################################################

library(ggplot2)
df <- data.frame(x = 1:4, y = 1/4)
myplot <- ggplot(df, aes(x,y)) + 
  geom_col(color = 'black', fill = 'lightblue', width = 1) + 
  theme_classic() + ggtitle('X ~ discrete uniform [1,4]') 
myplot

# Note that looking at the area under the curve to the left of 
# 2 will not give us the correct answer:
myplot + geom_vline(xintercept = 2, color = 'red')

# This is because the 'bar' at 2 actually spans from
# 1.5 - 2.5. To find the area under the curve, we
# make a 'correction' by adding/subtracting 0.5

myplot + geom_vline(xintercept = 2.5, color = 'red')

###################################################

###################################################
## It is more accurate to use the 
## 'continuity correction' for hypothesis testing
## for a proportion, which makes an adjustment
## similar to the one described above
###################################################
p2 <- prop.test(62, 100, correct = TRUE)
p2$p.value
