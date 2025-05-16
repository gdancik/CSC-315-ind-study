#########################################
# Correlation and regression
#########################################

library(ggplot2)

#######################################
## load internet data
#######################################
library(readr)
internet <- read_delim("http://pastebin.com/raw/enxWu6R6", delim = '\t')

# to make typing easier, let's rename some columns
library(dplyr)
internet <- internet %>% rename(internet_penetration = `Internet Penetration (Percent)`,
                   fb_penetration = `Facebook Penetration (Percent)`)

##############################################
# what is the mean value of internet and
# fb penetration?
##############################################

##############################################
# describe the shape of the distributions for
# internet penetration.
##############################################

##################################################
# scatterplots: plots a point for each pair of 
#   (x,y) values
##################################################
ggplot(internet,aes(internet_penetration, fb_penetration))  + 
         geom_point() + theme_classic() + 
              labs(x = "Internet Penetration (%)", y = "FB Penetration (%)",
                title = "Internet and FB penetration rates")


## where is the United States?
colors <- rep("black", length(internet$internet_penetration))
colors[internet$Country == "USA"] = "red"

## Re-plot with colors to highlight the United States
## Note that in this case color is not an aesthetic, because
## the aesthetic would map each observation to a category that
## then gets color-coded. Instead, we are specifying the colors directly
ggplot(internet, aes(internet_penetration, fb_penetration)) + 
  geom_point(color = colors) + theme_classic() + 
  labs(x = "Internet Penetration (%)", y = "FB Penetration (%)",
       title = "Internet and FB penetration rates")

########################################################################
## Is there a trend in the data?
## what countries are potential outliers (from the trend in the data)
########################################################################

## Let's highlight the outlier
## Note: for element-by-element comparison across vectors, use 
#  single & for AND and single | for OR 
#
# In R, && and || will return a single logical value
# a && b returns TRUE if all element-by-element comparisons of 'a' and'b'
# are TRUE

index = internet$fb_penetration < 10 & internet$internet_penetration>60
internet$Country[index]

#######################################################################
# Note: the code below is used to generate datasets with various
# correlations; you do not need to understand the code, but you do 
# need to understand how correlation is interpreted
#######################################################################

#######################################################################
# example correlations
#######################################################################
plot.cor <-function(x,y, location = "topleft", ...) {
  plot(x,y, ...)
  abline(h = mean(y), col = "red")
  abline(v = mean(x), col = "red")
  l = lm(y~x)
  abline(l, col = "blue")
  r = cor(x,y)
  r = paste("r = ", round(r,2))
  legend(x = location,r)
}


#######################################################################
# generate data
#######################################################################
x <- rnorm(100)
y1 <- rnorm(100, mean = x, sd = .4)
y2 <- rnorm(100, mean = -x, sd = .4)
y3 <- rnorm(100, mean = x, sd = 2)
y4 <- rnorm(100, mean = -x, sd = 2)
y5 <- rnorm(100)

#######################################################################
# plot data along with correlations
#######################################################################
par(mfrow = c(3,2))
par(mar = c(2,2,2,2)+.1)
plot.cor(x,y1)
plot.cor(x,y2, location = "topright")
plot.cor(x,y3)
plot.cor(x,y4, location = "topright")
plot.cor(x,y5)
plot.cor(x, x**2)


### reset to 1 panel for plotting 
par(mfrow = c(1,1))

#############################################
# find correlation between internet and FB
# penetration
#############################################
ggplot(internet, aes(internet_penetration, fb_penetration)) + 
  geom_point(color = colors) + theme_classic() + 
  labs(x = "Internet Penetration (%)", y = "FB Penetration (%)",
       title = "Internet and FB penetration rates")

## to get correlation ##
cor(internet$internet_penetration, internet$fb_penetration)

#############################################
# linear regression
#############################################

###################################################################
## fit a linear regression line with internet penetration as the
## explanatory or independent variable (x) and FB penetration as
## the response variable (y)
###################################################################

# Fit the linear model using lm(y~x, data = df), 
# where the linear model corresponds to y = f(x), and
# 'x' and 'y' are columns from the data frame 'df'

fit <- lm(fb_penetration ~ internet_penetration, data = internet)

## display results ##
fit

## display summary of results (more information) ##
summary(fit)

###################################################################################
## Questions: (1) Find and interpret the y-intercept of the linear regression line
##            (2) Find and interpret the slope
################################################################################### 



## add regression line to current plot ##
ggplot(internet, aes(internet_penetration, fb_penetration)) + 
  geom_point() + theme_classic() + 
  labs(x = "Internet Penetration (%)", y = "FB Penetration (%)",
       title = "Internet and FB penetration rates") +
  geom_smooth(method = "lm", color = "darkred")
  
########################################################################
# Use predict(fit, df) to make predictions with the fitted model
#      fit - a fitted linear model, obtained from 'lm'
#      df - a data frame, but column names MUST be the same as those 
#            used to fit the original model
########################################################################

# if df is not specified, predictions are based on the original data
predict(fit)

## what is the predicted FB penetration rate for a country that
## has 50% internet penetration
predict(fit, data.frame(internet_penetration=50))

## what is the predicted FB penetration rate for countries that
## have 50% and 80% internet penetrations
predict(fit, data.frame(internet_penetration=c(50,80)))


#################################################################
# A linear model gives the equation of the line that minimizes
# the sum of the squared residuals. The code below illustrates
# this minimization (the regression line minimizes the lengths
# of the blue lines)
#################################################################

## generate a scatterplot and visualize residuals for lm(y~x) ##
plot.resids <- function(x,y, ...) {
  plot(x,y, pch = 19,...)
  l = lm(y~x)
  abline(l, col = "red")
  points(x, predict(l), pch = 19, col = "red")
  segments(x,predict(l), x, y, col = "blue")
  legend("topleft", c("obs", "prediction", "residual"), 
         lty = c(0,0,1), pch = c(19,19,-1),col = c("black", "red", "blue"))
}


plot.resids(internet$internet_penetration, internet$fb_penetration)

######################################################
# Find the regression for the ANNUAL temp against YEAR 
# and complete the questions below
######################################################
temps <- read.delim("http://pastebin.com/raw/KZgkViBK")
tempPlot <- ggplot(temps, aes(YEAR, ANNUAL)) +
            geom_point() + 
            geom_smooth(method = "lm", color = "darkgreen", 
                        fullrange = TRUE) +
            ggtitle('Average annual temperature over time') +
            theme_linedraw()

tempPlot

fit <- lm(ANNUAL ~ YEAR, data = temps)

# 1. Find and interpret the slope

# 2. Find and interpret the y-int

# 3. Predict average temp in the year 1999

# 4. Predict average temp in the year 3000
