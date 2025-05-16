################################################
## The t-distribution and t test statistic
################################################

library(ggplot2)
library(dplyr)

x <- seq(-3.5, 3.5, by=.1)
pz <- dnorm(x)
pt1 <- dt(x, df = 1)
pt5 <- dt(x, df = 5)
pt30 <- dt(x, df = 30)

df <- data.frame(x = x, pz = pz, pt1 = pt1, pt5 = pt5, pt30 = pt30)
colors <- c("red", "skyblue", "darkblue", "darkgreen")
labels <- c("normal", "t, df = 1", "t, df = 5", "t, df = 30")

ggplot(df) + geom_line(aes(x, pz, color = "pz")) +
             geom_line(aes(x, pt1, color = "pt1")) +
             geom_line(aes(x, pt5, color = "pt5")) +
             geom_line(aes(x, pt30, color = "pt30")) +
             ggtitle("Normal and Student's t Distribution") +
             ylab("density") +
            scale_colour_manual(breaks = c("pz", "pt1", "pt5", "pt30"),
                      values = colors,
                      labels = labels) +
            theme_classic() + labs(color = "Distribution") +
            theme(legend.background = element_rect(color = "black"))
            
            
###################################################################
# plots histogram and either the standard normal distribution 
# when df is NULL or both the standard normal and t distribution 
# with the specified degrees of freedom (df)
###################################################################
hist.norm <- function(X, df, ...) { 
  h <- hist(X, plot = FALSE)
  col <- rep("white", length(h$breaks))
  ylim <- NULL

  m = max(h$density, dnorm(0))
  if (!is.null(df)) m = max( max(h$density), dt(0, df = df))
  m <- m +0.1*m  
  ylim <- c(0,m)
  
  hist(X, prob = TRUE, ylim = ylim, col = col, ...)
  
  curve(dnorm(x), col = "red", lty = 2, lwd = 2, 
        add = TRUE)
  if (!is.null(df)) {
    curve(dt(x, df = df), col = "darkred", lty = 2, lwd = 2, 
          add = TRUE)
  }

  if (is.null(df)) {
    legend("topleft", legend = "N(0,1)", col = "red", lty = 2, lwd = 2)  
  } else {
    
    t.txt <- paste("t, df = ", df, sep = "")
    legend("topleft", legend = c("N(0,1)", t.txt), 
           col = c("red","darkred"), lty = 2, lwd = 2)
  }
}

####################################################
## generates t-distributed random variables where
## t = (mean(x) - mu)  / (sd(x) / sqrt(n))
## This t-statistic follows the t distribution
## with n-1 df
####################################################
t.statistic <- function(n, mu = 0, sigma = 1) {
  x <- rnorm(n, mean = mu, sd = sigma) 
  t <- (mean(x) - mu) /  ( sd(x) / sqrt(n))
  return(t)
}


## look at the t distribution empirically 
n <- 5
t <- replicate(5000, t.statistic(n, 10, 5))

# plot histogram with normal vs. t distribution  (df = n-1)#
par.orig <- par(mfrow = c(2,1), mar = c(2,4,2,1))
hist.norm(t, df = NULL, breaks = 40, main = "Histogram of Z ~ N(0,1)")
hist.norm(t, df = n-1, breaks = 40, main = "Histogram of t distribution")
par(par.orig)

#######################################################################
# t distribution functions in R follow the format of 
#    pnorm, dnorm, etc, but all require an argument 
#    for df = degrees of freedom
# pt - probability function (similar to pnorm)
# dt - density function (similar to dnorm)
# rt - random generation function (similar to rnorm)
#######################################################################

###########################################################################
# Example (one sample t-test): We will first analyze whether CS majors 
# have, on average, a GPA that differs from 3.3 (a B+). So that we can
# generalize the results of our hypothesis test, we will assume that 
# the class data is representative of all CS majors at eastern
##########################################################################

survey <- read.delim("http://pastebin.com/raw/QDSga7qF")

#1. State the null and alternative hypotheses.

# H0: mu = 3.3   vs. HA:  mu != 3.3, 
#     where mu is the mean college GPA of all CS students

#2. Calculate the t test statistic under the null hypothesis.
n <- length(survey$College.GPA) #note: would not work if there were missing values
t <- (mean(survey$College.GPA) - 3.3)   /   
        ( sd(survey$College.GPA) / sqrt(n) )
t

#3. Graph the distribution of the t test statistic under the null 
# hypotheses, using the 'dt' function to plot the density. 
# Give the graph an apropriate title and draw a vertical line at 
# the value of the test statistic. 

df <- data.frame(x = seq(-4,4, length.out = 100)) %>% 
      mutate(y = dt(x, df = n - 1))

main <- paste0("Distribution of t test statistic when mu = 3.3, n = ", n)
ggplot(df) + geom_line(aes(x,y)) +
    ggtitle(main) + labs(x = "t", y = "density") +
    theme_classic() +
    geom_vline(xintercept = t, color = "red") + 
    geom_text(aes(t+1.3, .4), label = "observed t test statistic")



#4. Find the p-value of this test statistic using the 'pt' function.
2*pt(-abs(t), df = n-1)

##########################################################################
# Find the test statistic and the p-value using the t.test function 
# below. Note that these answers should match your answers to the previous
# questions
##########################################################################

result <- t.test(survey$College.GPA, mu = 3.3)
result$statistic
result$p.value
result$parameter # degrees of freedom

##########################################################################
# State your conclusion regarding the null hypothesis in the context of
# this problem.
##########################################################################

# Because p = 0.362 is NOT less than 0.05, there is not sufficient 
# evidence to conclude that CS majors have a GPA that differs from
# a 3.3 (B+ average)

