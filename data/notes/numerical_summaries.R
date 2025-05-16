####################################################################
## Numerical summaries of data
####################################################################

library(ggplot2)

###########################
## calculate the mean in R
###########################
x <- c(5,10,11,1)
mean(x)

#####################################################
## calculate the mean when there are missing values
## Note that many functions have an na.rm argument
#####################################################
x <- c(5,10,11,1, NA)
mean(x)  ## this will return NA
mean(x, na.rm=TRUE)

# compare mean and median
income <- c(29000, 35000, 35000, 40000, 33000)
mean(income)
median(income)

#######################################################
# plots a histogram of x with vertical lines for
# the mean and median (Note: you should understand
# how we use this function, but you do not need to 
# understand the underlying code)
#######################################################
plot.hist <- function(x, breaks = "Sturges", ...) {
  hist(x, breaks = breaks, ...)
  abline(v = mean(x), col = "blue")
  abline(v = median(x), col = "red")
  text(mean(x), 1, "mean", srt = 90)
  text(median(x), 1, "median", srt = 90)  
}

plot.hist(income, xlab = "income", main = "Histogram of incomes")

####################################################################
## What happens to the mean and median when income contains a very 
## large (or small) observation, such as 250,000?
## Is the mean or median more resistant to this large observation?
####################################################################
income.with.extreme.value <- c(income, 250000)
plot.hist(income.with.extreme.value, 
          breaks <- seq(0,260000,10000),
          xlab = "income", main = "Histogram of incomes with extreme value")

#########################################################
# The standard deviation is a measure of variability, or
# how spread out the data is relative to the mean. The
# code below provides a graphical demonstration of 
# how the standard deviation is calculated
#########################################################

# plot deviations to illustrate standard deviation (you do not
# need to understand the underlying code for this function)
plot.deviations <- function(x) {

  df <- data.frame(value = x, index = 1:length(x)) 
  
  ggplot(df) + geom_point(aes(x = value, y = index), size = 2.5) + 
    geom_segment(aes(x = value, y = index, 
                     xend = mean(x), yend = index), 
                 col = 'blue') + 
    geom_vline(xintercept = mean(x),col = 'red') +
    theme_classic() + ggtitle('Deviation demonstration')
  
}

################################################
## recreate example from slide 12 of the notes
################################################
x <- c(0,0,0,2,4,4,4)

# the blue lines are the deviations from the mean (red line)
plot.deviations(x)

##################################################
# manual method for calculating variance and 
# standard deviation
##################################################
dev <- x - mean(x)
sd.table = data.frame(x = x, dev = dev, "squared deviation" = dev**2)
sd.table
n = nrow(sd.table)

sum.squared.dev <- sum(sd.table$squared.deviation)
x.var.manual <- (sum.squared.dev) / (n-1)
x.sd.manual <- sqrt(x.var.manual)
x.sd.manual

##################################################
# easy way to calculate variance and standard 
# deviation in R
##################################################
x.var <- var(x)
x.var

x.sd <- sd(x)  
x.sd
