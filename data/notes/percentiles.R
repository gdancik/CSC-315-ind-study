library(ggplot2)

####################################################################
## Percentiles and Boxplots
####################################################################

##################################################
# Visualization of the 6.5 as the 60th percentile
# in a dataset consisting of the mumbers 1 - 10
##################################################

# we will plot x-values (the data) vs. y = 1 for visualization
x <- 1:10
y <- rep(1,10)

# set the colors
v.red <- rep("red", 6)
v.blue <- rep("blue", 4)
color <- c(v.red, v.blue)

# use ggplot to plot points; note that we do not have a data.frame, so 
# we do not pass any values to ggplot(); the x- and y-vectors can
# be specified directly for the aesthetics
ggplot() + geom_point(aes(x,y), color = color) +
           theme_classic() +
           geom_vline(xintercept = 6.5, color = 'darkgreen') +
           ylim(0,1.6) +
           annotate("text", x = 3, y = 1.3, label = "60% of the data", color = "red") +
           annotate("text", x = 9, y = 1.3, label = "40% of the data", color = "blue") +
           ggtitle("Visualization of the 60th percentile")
           

# manually calculate this percentile
sum(x < 6.5) / length(x)

#############################################
# find the 20th and 30th percentiles
#############################################
quantile(x, .2)   ## finds the 20th percentile
quantile(x, .3)   ## finds the 30th percentile
quantile(x, c(.2,.3)) ## finds both the 20th and 30th percentile

#############################################
# Quartiles and 5-number summary 
# (min, Q1, Q2, Q3, max)
# Note that summary/quantile and fivenum 
# give different values of Q1 and Q3 when 
# there are an even number of observations
# However both are correct
#############################################

summary(x) ## note that this also contains the mean
quantile(x) ## just the quantiles
fivenum(x)  ## five number summary

#############################################
# Quartiles are the same for an odd number of
# observations
#############################################
z = 1:11
summary(z)
quantile(z)
fivenum(z)

##################################################
# boxplot - 
# 1) draw box around Q1 and Q3
# 2) set IQR = Q3 - Q1, and define a 'fence' 
#    that encloses points between Q1 - 1.5*IQR
#    and Q3 + 1.5*IQR.
# 3) draw 'whiskers' from the Q3 to the largest
#    observation inside the fence, and from Q1 to
#    the smallest observation inside the fence
# 4) add points for any observations outside
#    the fence (these are considered outliers)
##################################################

# create a new data.frame containing some numbers
numbers <- data.frame(values = c(1:10, 20:21))

#################################################################
# Generate boxplot using ggplot
# In the aesthetic we include the following:
#    x corresponds to the column containing the groups to plot; 
#      here we use a dot (".") to indicate that there are no groups
#      (there are no groups in this example, in general there will be
#      groups, as shown in the next example)
#    y corresponds to the column containing the numeric data
# Also note the 'fill' argument is outside the aesthetic, so it 
#     is applied to all groups
###################################################################
g <- ggplot(numbers, aes(".", values)) + geom_boxplot(fill = "lightblue") + 
           xlab("")  + ggtitle("Example boxplot") + theme_classic()

# Let's annotate the boxplot to understand the 'box'
med <- median(numbers$values)
q1 <- quantile(numbers$values, .25)
q3 <- quantile(numbers$values, .75)
upper.fence <- q3 + 1.5*(q3-q1)
lower.fence <- q1 - 1.5*(q3-q1)
g + annotate('label', x=.5, y=med, label = 'median') +
    annotate('label', x=.5, y=q1, label = 'Q1') +
    annotate('label', x=.5, y=q3, label = 'Q3') +
    annotate('label', x = 1, y = upper.fence, 
             label = 'upper fence = Q3+1.5*IQR') +
    annotate('label', x = 1, y = lower.fence, 
           label = 'lower fence = Q1-1.5*IQR')


#####################################################################
# Side-by-side boxplots can be used to compare quantitative data 
# across two (or more) groups 
#####################################################################

# Let's compare the heights of males and females using side-by-side boxplots #
# males = 0, females = 1
heights <- read.csv("http://pastebin.com/raw/g7UdTFKG")

# since gender is categorical, let's change it to a factor 
# (otherwise, ggplot will treat gender as numeric)
# we also set the levels (otherwise the levels are 0 and 1)
heights$GENDER <- factor(heights$GENDER, labels = c("Male", "Female"))

# Here the 'fill' argument is part of the aesthetics, since it maps those
# values to a color
ggplot(heights, aes(GENDER, HEIGHT, fill = GENDER)) + 
  geom_boxplot()

# Change formatting (remove legend, change labels)
# Note: other options for the legend.position are "left", "right", "bottom", "top"
ggplot(heights, aes(GENDER, HEIGHT, fill = GENDER)) + 
        geom_boxplot(show.legend = FALSE) + 
        ggtitle("Comparison of heights between males and females") +
        labs(x = "gender", y = "height (inches)") +
        theme_classic() 


#############################################################
# Note about ggplot
#############################################################

# if the aesthetics are specified in the ggplot function, they apply
# to all layers. In this case, the x-, y-, and fill values apply
# to all layers, as shown in the next example which adds a 
# geom_jitter layer, which plots points with random noise added 
# (to limit overlap between points)

# the 'color' aesthetic applies to the outline of the boxplot
# and the color of the points

ggplot(heights, aes(GENDER, HEIGHT, color = GENDER)) + 
  geom_boxplot(show.legend = FALSE) + 
  geom_jitter(width = .2, show.legend = FALSE, size = 1) + 
  ggtitle("Comparison of heights between males and females") +
  labs(x = "gender", y = "height (inches)") +
  theme_classic() 

# if you wanted different colors for the boxplot and jitter layers, 
# you can change the aesthetic for the desired layer only
ggplot(heights, aes(GENDER, HEIGHT)) + 
  geom_boxplot(aes(color = GENDER), show.legend = FALSE) + 
  geom_jitter(width = .2, show.legend = FALSE, size = .8) + 
  ggtitle("Comparison of heights between males and females") +
  labs(x = "gender", y = "height (inches)") +
  theme_classic() 
