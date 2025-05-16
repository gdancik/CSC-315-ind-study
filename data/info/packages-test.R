########################################################################
# Script to test whether tidyverse (dplyr and ggplot2), and 
# knitr related packages are installed
########################################################################

# TO DO: From RStudio, select File->Knit Document and select HTML and click
# Compile. If prompted to update or install additional packages, do so. 
# If you receive any error messages, most likely the required packages 
# have not been installed. 

library(dplyr)
library(ggplot2)

iris2 <- filter(iris, Species!= "setosa")

ggplot(iris2, aes(Species, Sepal.Length, fill = Species)) + geom_boxplot() +
  theme_classic() + theme(legend.position = "none") +
  ggtitle("Speal Length of Versicolor and Virginica Iris Flowers")


