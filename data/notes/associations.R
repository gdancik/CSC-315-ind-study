##############################################
# associations.R: 
##############################################

library(ggplot2)
library(reshape2)

#########################################################################
# We first consider the situation where we start with the contingency
# table and not the underlying raw data
#########################################################################

# Manually construct contingency table from pg 7 of the notes, using
# cbind (column bind)
pesticides <- cbind(Present = c(29,19485), "Not Present" = c(98, 7086))
rownames(pesticides) <- c("Organic", "Conventional")
pesticides

# confirm row and column sums ##
colSums(pesticides)
rowSums(pesticides)

# To calculate conditional proportions, use prop.table with
#      margin = 1 to condition on rows
#      margin = 2 to condition on columns
#      margin = NULL (default) to condition on total number of observations

# We use margin = 1 to condition on row (pesticide status -- organic vs. conventional)
pesticides.conditional <- prop.table(pesticides, margin = 1)
pesticides.conditional

##################################################################
# We visualize associations between qualitative variables through
# side-by-side or stacked bar graphs
##################################################################

# To use ggplot, we need a data.frame with 1 column for the 
# explanatory variable (pesticide type), another column for 
# the response variable (presence), and another column for 
# the conditional proportion (value) 
# this is accomplished in the two steps below

#convert the table into a data.frame ...
d <- data.frame(type = rownames(pesticides.conditional), pesticides.conditional)

# ... but to use ggplot, we need each column of our data frame to represent a
# variable, but this is not the case. 'Present' and 'Not.Present' are 
# different values for the same variable ('presence'). We correct this by
# using the 'melt' function (from reshape2) to combine (melt) the
# values from the other columns into a single column (value) and to add
# a column with the appropriate label ('presence') for each value of that variable

# In 'melt', the second argument ("type") specifies the 'id' variables that are left
# unchanged. All other columns are combined as follows:
#     the column names become the values of a new 'presence' column 
#         (determined by 'variable.name')
#     the values are combined into a single column, which will be named 'value' by default
m <- melt(d, id = "type", variable.name = "presence")


# display a stacked bar chart, with x = the explanatory variable 
# and 'fill' used to indicate the response variable; note that
# we use 'geom_col' because our data contains the y-values 
ggplot(m, aes(type, value, fill = presence)) + geom_col() +
            labs(y = "Proportion", fill = "Pesticide status", 
                 title = "Distribution of pesticide status by food type") +
            theme_classic()

## display a side-by-side barchart, by changing the position argument 
## to 'dodge' in geom_bar
ggplot(m,aes(type, value, fill = presence)) + geom_col(position = "dodge") +
  labs(y = "Proportion", fill = "Pesticide status", 
       title = "Distribution of pesticide status by food type") +
  theme_classic()

#################################################################
# example of no relationship; remember we must compare 
# conditional proportions between each EXPLANATORY variable and 
# not each response variable
#################################################################

# p2 is contingency table with conditional proportions
p2 <- cbind(Present = c(0.6,0.62), "Not Present" = c(0.4, 0.38))
rownames(p2) <- c("Organic", "Conventional")
p2

# create the 'melted' data.frame
d <- data.frame(type = rownames(p2), p2)
m <- melt(d, "type", variable.name = "presence")

## display a stacked bar chart, based on 'fill' argument
ggplot(m,aes(type, value, fill = presence)) + geom_col() +
  labs(y = "Proportion", fill = "Pesticide status", 
       title = "Distribution of pesticide status by food type\n(no association)") +
  theme_classic()


#########################################################################
# We next consider the situation where we are working with raw data
#########################################################################

## import sample survey data from previous class ##
library(readr)
survey <- read_delim("http://pastebin.com/raw/QDSga7qF",
"\t", escape_double = FALSE, trim_ws = TRUE)


# Is there a relationship between cat/dog person and coffee preference?
# create a contingency table, where first vector gives rows, second gives columns
t <- table(survey$CatOrDog, survey$StarbucksOrDunkins)
t.conditional <- prop.table(t, margin = 1)
t.conditional

## To create a stacked bar graph from raw data, use "geom_bar" with the following
## aesthetics: aes(explanatory, fill = response) 
## We also need to set position to "fill" in geom_bar in order to plot conditional proportions
## (rather than counts)
ggplot(survey,aes(CatOrDog, fill=StarbucksOrDunkins)) + geom_bar(position = "fill") +
                labs(x = "", y = "Relative frequency", 
                     title = "Coffee preference by person")
