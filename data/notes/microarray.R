#############################################
# Microarray Basics
#############################################

library(affy)
library(affydata)
library(ggplot2)
library(dplyr)

###############################################################
# Look at the Dilution dataset, which contains cRNA from
# human liver tissue (labeled 'A') and from a central nervous
# system cell line (labeled 'B'). The amount of of cRNA is
# either 10 or 20 micrograms. Note that Dilution is an
# AffyBatch object (which is an extension of an 
# ExpressionSet (eSet) object)
###############################################################

# load the data
data(Dilution)

# display the data
Dilution

####################################################
# Methods (functions) for AffyBatch objects
####################################################
sampleNames(Dilution)     # the names of the samples
experimentData(Dilution)  # experiment information
annotation(Dilution)      # annotation (the microarray used)
phenoData(Dilution)       # get summary of phenotypic (clinical) data and metadata
pData(Dilution) # phenotypic data in table form
varMetadata(Dilution) # description of phenotypic data

####################################################
# Let's look at each microarray
####################################################

# You can run the code below, but will need to hit enter to 
# generate each microarray, which takes time
#image(Dilution, col = topo.colors(500))
# clear the current plot
#dev.off()

# let's just look at the first one
image(Dilution[,1], col = topo.colors(500))


###########################################################
# We can use the base 'boxplot' function to 
# produces a boxplot of the probe intensities for
# each microarray (for 'AffyBatch' objects, 'boxplot' 
# plots the intensities on the log2 scale and takes the
# title from experimentData). Does it seem fair to compare 
# gene expression across concentrations or scanners?
###########################################################

boxplot(Dilution, col = 1:4, ylab = "log2 expression")

####################################################
# Normalization of microarray data involves
# 1. Background correction to remove noise
# 2. Normalization of samples
# 3. Estimation of "average" probe intensity values
####################################################

######################################################################
# We will use Robust Multi-array average (RMA) which involves 
# 1. Background correction of probe level intensity values
# 2. Quantile Normalization (all probe level quantiles are the same)
# 3. Estimation of average probe set values on log2 scale
######################################################################

Dilution.rma <- rma(Dilution)        # perform RMA
Dilution.expr <- exprs(Dilution.rma) # extract the expression values

################################################################
# confirm that samples are quantile normalized
################################################################
boxplot(Dilution.expr, col = 2:5, ylab = "log2 expression",
        main = "Small part of dilution study (after RMA normalization)",
        xlab = "Sample")


################################################################
# Let's look at a leukemia dataset, and compare 
# acute lymphoblastic leukemia (ALL) samples with healthy, 
# non-leukemia (noL) bone marrow samples
################################################################
library(leukemiasEset)
data(leukemiasEset)

################################################################
# Extract phenotype data. How many samples of each leukemia 
# type are there?
################################################################
leukemia.p <- pData(leukemiasEset)
table(leukemia.p$LeukemiaType)

################################################################
# This data is already processed using RMA, so we can extract
# the expression data
################################################################
leukemia.expr <- exprs(leukemiasEset) 

# confirm that data has been normalized #
boxplot(leukemia.expr, main = "Leukemia samples", ylab = "log2 expression")

######################################################################
# Let's compare expression between leukemia (ALL) and healthy (noL)
# bone marrow samples for the following probes (these are probe IDs):

# ENSG00000171960 - corresponds to gene PPIH 
#       (https://www.ncbi.nlm.nih.gov/gene/10465)
# ENSG00000135679 - corresponds to gene MDM2
#       (https://www.ncbi.nlm.nih.gov/gene/4193)

# We want to calculuate the fold change (see below) and the p-value
# evaluating whether or not any difference in expression between
# leukemia (ALL) and healthy samples (noL) are statistically significant
# (in other words, are the genes differentially expressed?)
######################################################################

# find expression for desired probe -- we need to determine which
# row of the expression matrix contains the expression for this probe
m <- match("ENSG00000171960", rownames(leukemia.expr))
m

df <- data.frame(expr = leukemia.expr[m,], type = leukemia.p$LeukemiaType)

ggplot(df,aes(type, expr, fill = type)) + geom_boxplot() +
  theme_classic() + theme(legend.position = "none") + 
  labs(x = "Leukemia Type", y = "Log2 Expression") +
  ggtitle("PPIH (ENSG00000171960) expression")


# let's only look at ALL and NoL
df <- filter(df, type == "ALL" | type == "NoL")
ggplot(df,aes(type, expr, fill = type)) + geom_boxplot() +
  theme_classic() + theme(legend.position = "none") + 
  labs(x = "Leukemia Type", y = "Log2 Expression") +
  ggtitle("PPIH (ENSG00000171960) expression")


########################################################################
# Calculate fold change (FC) which is the average expression in the 
# first group divided by the average expression in the second group,
# Since data is on the log2 scale, we must convert back to normal scale
########################################################################

# in split function, if the second argument is a factor and drop 
# is TRUE, then we drop levels that do not occur
s <- split(df$expr, df$type, drop = TRUE)  

l <- lapply(s, mean)

logFC <- l$ALL - l$NoL   # difference on log2 scale
FC <- 2**logFC  ## difference in terms of fold change

main = paste("PPIH (ENSG00000171960) expression\nFC = ", round(FC,2))
ggplot(df,aes(type, expr, fill = type)) + geom_boxplot() +
  theme_classic() + theme(legend.position = "none") + 
  labs(x = "Leukemia Type", y = "Log2 Expression") +
  ggtitle(main)


########################################################################
# Is the difference in fold change statistically significant??
# H0: mu_ALL - mu_normal = 0
# HA: mu_ALL - mu_normal != 0
# where mu_ALL is the mean expression of ALL leukemia samples for the 
# probe of interest and mu_normal is the mean expression of
# normal samples

# Note: the null hypothesis is equivalent to H0: FC = 1

# How do we carry out a hypothesis test comparing two population means?
########################################################################





########################################################################
# Repeat analysis for probe ENSG00000135679 (MDM2)
########################################################################

# get row index of probe, then create data frame, including only
# ALL and NoL samples
m <- match("ENSG00000135679", rownames(leukemia.expr))
df <- data.frame(expr = leukemia.expr[m,], type = leukemia.p$LeukemiaType)
df <- filter(df, type == "ALL" | type == "NoL")
ggplot(df,aes(type, expr, fill = type)) + geom_boxplot() +
  theme_classic() + theme(legend.position = "none") + 
  labs(x = "Leukemia Type", y = "Log2 Expression") +
  ggtitle("MDM2 (ENSG00000135679) expression")


# Is there evidence that MDM2 is differentially expressed between 
# ALL and normal samples? (we will complete this in class)

s <- split(df$expr, df$type, drop = TRUE)  

l <- lapply(s, mean)
logFC <- l$ALL - l$NoL
FC <- 2**logFC  ## data is on log2 scale

res <- t.test(s$ALL, s$NoL)
res$p.value

# because P < 0.05, we reject H0 and conclude that
# MDM2 is differentially expressed between ALL (leukemia)
# and NoL (normal) samples. Specifically, MDM2 is up-regulated 
# in ALL (leukemia) samples

# Useful terminology:
#     upregulated = higher expression in
#     downregulated = lower expression in
