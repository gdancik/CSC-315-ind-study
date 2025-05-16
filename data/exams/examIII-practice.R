# CSC 315, Exam III Practice Problems

# Note: This is not a comprehensive review, but contains exercises covering some
# of the concepts that will appear on Exam III. In addition to these exercises,
# make sure you understand concepts covered in lecture and on the previous labs 
# and project.

# Directions: Modify this script to add R code in order to answer the questions 
# and/or complete the steps below. 

library(ggplot2)
library(dplyr)


# 1) Fit a linear model that predicts Petal Width based on the type of flower from
# the iris dataset (see below). Code your explanatory variable 
# using x = 0 if the sample is of type "versicolor" and x = +1 if the sample is of type 
# "virginica" (type "setosa" will be ignored). 
#    
# (a) Find and interpret the y-intercept of the regression line in the
#      context of this problem.
# (b) Find and interpret the slope in the context of this problem
# (c) Based on your linear model, what is the p-value for the hypothesis test that there is a
#     significant difference in Petal Width between the two flower types? 
#     Note that you should extract the exact p-value from the coefficients object
#     (which is a matrix) of the fitted linear model. 
# (d) Verify the p-value from (c) is identical to the p-value obtained from
#     the two-sample t-test with equal variance

#  The code below constructs a scatterplot of this data (the iris data is available 
#  by default). Note that you will need to remove or ignore "setosa" in your analysis.

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + 
  geom_point() + theme_classic() + 
  theme(legend.background = element_rect(color = "black",
                                         linetype = "solid")) 



# 2) The code below reads in the TMM processed logCPM expression data
#   and pheno data from a dataset comparing patients with 
#   Alzheimer's Disease to patients who are healthy. Executing the R
#   code will load two objects into your workspace:
#
#  logCPM - the logCPM expression data
#  pheno  - the phenotype data
#  probeMap - the probe map information

# (Note: This dataset was downloaded from: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE184942)

load(url('https://github.com/gdancik/CSC-315/raw/gh-pages/data/exams/GSE184942.RData'))

#   Answer the following questions based on the expression data:
#    (a) How many samples are there? 
#    (b) How many probes are profiled?
#    (c) What is the mean expression value of the 5th probe?
#    (d) Which probe has the highest mean expression?
#        Hint: which.max can be used to find the index of the maximum element of a vector

# 3) In the phenotype data table, characteristics_ch1 indicates whether 
#    the sample is from a healthy patient or a patient with Alzheimer's Disease (AD).
#    Generate a relative frequency table for the proportion of patients in each group.


# 4)  Using the limma package, find the top 10 differentially expressed probes
#     (sorted by p-value). What is the probe name, the fold-change, 
#     and the adjusted p-value of the top probe. 


# 5) Based on the probes below, generate a heatmap and color the columns with "lightgreen" for
#    healthy samples and "darkred" for AD samples.

probes <- c("ENSG00000171189", "ENSG00000171502", "ENSG00000154080", "ENSG00000240342", 
    "ENSG00000183044", "ENSG00000226608", "ENSG00000167491", "ENSG00000104490", 
    "ENSG00000250305", "ENSG00000089169")


# 6) Using k-nearest neighbor classification with k = 3, calculate the recall for each group, using
#    the probes from the previous question. Don't forget to scale each probe!

# 7) Show that the probes above correspond to the following genes:

genes <- c("GRIK1", "COL24A1", "CHST9", "RPS2P5", "ABAT", "FTLP3", "GATAD2A", 
    "NCALD", "KIAA1456", "RPH3A")


# 8) Using DAVID, find the top GO (GOTERM_BP_DIRECT) terms associated with the genes 
#    in the vector below.

genes <- c("GRIK1", "RTBDN", "ABAT", "KCNK18", "TMC4")
