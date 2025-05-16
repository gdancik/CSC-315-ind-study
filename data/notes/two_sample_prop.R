#############################################################
# Note: the code before the example is provided to
# illustrate the theorem:
#   If X1 ~ N(mu1, sigma1) and X2 ~ N(mu2, sigma2), then
#     X1+X2 ~ N(mu1+mu2,   sqrt(sigma1**2 + sigma2**2))
# You do not need to worry about understanding the code,
# but you should understand the theorem
#############################################################

####################################################
# histogram with normal distribution
####################################################
hist.norm <- function(X, mu, sigma, ...) { 
  h <- hist(X, plot = FALSE)
  col <- rep("white", length(h$breaks))
  ylim <- NULL
  m <- max( max(h$density), dnorm(mu, mean = mu, sd = sigma))
  m <- m +0.1*m  
  ylim <- c(0,m)
  
  hist(X, prob = TRUE, ylim = ylim, col = col, ...)
  
  curve(dnorm(x, mean = mu, sd = sigma), col = "red", lty = 2, lwd = 2, 
        add = TRUE)
}

####################################################
# normal density: graphs normal density with 
# x range of (mu - r*sigma) to (mu + r* sigma)
####################################################
norm.density <- function(mu, sigma, r=4, main = NULL, ylab = "density", ...) {
  x <- seq(mu-r*sigma, mu+r*sigma, length.out = 100)
  if (is.null(main)) main <- paste("N(", mu, ",", sigma, ")", sep = "")
  plot(x, dnorm(x, mean = mu, sd = sigma), type = "l",
       ylab = ylab, main = main, ...)

  
}

  
#############################################################
# We will visualize the distribution of X1 + X2 where
# X1 ~ N(20,4) and X2 ~ N(6,1)
# Theorem:
#   If X1 ~ N(mu1, sigma1) and X2 ~ N(mu2, sigma2), then
#   X1+X2 ~ N(mu1+mu2,   sqrt(sigma1**2 + sigma2**2))
#############################################################

mu1 <- 20
sigma1 <- 4

mu2 <- 6
sigma2 <- 1

# finds the sum of X1 and X2 where 
# X1 ~ N(mu1, sigma1) and X2 ~ N(mu2, sigma2)
add.norm <- function(mu1,sigma1, mu2,sigma2) {
  x1 <- rnorm(1, mean = mu1, sd = sigma1)
  x2 <- rnorm(1, mean = mu2, sd = sigma2)
  x1+x2
}


x <- replicate(1000, add.norm(mu1,sigma1,mu2,sigma2))

mu12 <- mu1+mu2
sigma12 <- sqrt(sigma1**2+sigma2**2)

split.screen(c(2,1))           # splits screen into 2 rows
split.screen(c(1,2))           # splits 1st row into 2 columns
screen(3)                      # select top left
norm.density(mu1, sigma1, xlab = "X1")
screen(4)                      # select top right
norm.density(mu2, sigma2, xlab = "X2")
screen(2)                      # select bottom 
hist.norm(x, mu = mu12, sigma = sigma12,
          main = "N(26, sqrt(17))", xlab = "X1+X2")
close.screen(all=TRUE)


##################################################################
# Example: In a survey of 2600 individuals, 110 out of 1120 males 
# were red-green color-blind and 10 out of 1480 females were
# red-green colorblind. Is there a significant relationship 
# (at p < 0.05) between sex and red-green color-blindness?
# Answer this question by carrying out the steps below.
##################################################################

#1. State the null and alternative hypotheses (done for you)
# H0: pm - pf = 0, where pm is the true proportion of males who are
#      red-green colorblind; pf is the true proportion of females
# HA: pm - pf != 0

#2. Find the test statistic

#3. Find the p-value


#4. State the conclusion


#5. What would it mean in the context of this problem if a Type I
#   error occured? 

# 6. What would it mean in the context of this problem if a Type II
#   error occured? 

