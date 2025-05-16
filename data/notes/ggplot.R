###########################################################
# ggplot provides a powerful framework for generating
# complex graphics, based on a philosophy/language of 
# visualization that separates the data, the aesthetics 
# (mapping of variables to visual properties), and the 
# plotting layer(s)
###########################################################

# First, we will look at a simple scatterplot using the 
# 'base' plot function in R
plot(iris$Petal.Width, iris$Petal.Length, pch = 18, 
     xlab = "Petal Width", ylab = "Petal Length",
     main = "Petal Length vs. Petal Width from Iris dataset")

# Now generate similar scatterplots using ggplot
library(ggplot2)

####################################################################
# ggplot examples -- we will generally use this format
# ggplot(data, aes(x,y)) + layer + labels + ...
#    data - the data.frame containing data to plot
#    aes - an aesthetic mapping, specifying the x- and y-values
#             to plot, and optionally the coloring to use; when 
#             specified in ggplot(), the mapping applies to 
#             all layers
#    layer - the type of plot, e.g., geom_point() for a scatterplot, 
#             geom_bar() for a bar plot, etc
#    labels - ggtitle(), xlabs(), ylabs(), and labs() 
####################################################################

ggplot(iris, aes(x=Petal.Width, y = Petal.Length)) +
      geom_point() + 
      ggtitle("Petal Length vs. Petal Width from Iris dataset") +
      labs(x = "Petal Width", y = "Petal Length") 
      

# we can color the points by specifying the color (or colour) aesthetic
# we can also store the plot in an object and plot later using the
# print function
g <- ggplot(iris, aes(x=Petal.Width, y=Petal.Length)) +
  geom_point(aes(color = Species)) +
  ggtitle("Petal Length vs. Petal Width from Iris dataset") +
  labs(x = "Petal Width", y = "Petal Length")
print(g)

# alternative plots with added components
g + geom_smooth(color = "blue")
g + geom_smooth(color = "blue") + theme_linedraw()

