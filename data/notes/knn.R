###############################################################
# k-nearest neighbor (KNN) example
###############################################################

library(dplyr)
library(ggplot2)
library(UCSCXenaTools) # needed to retreive data
library(edgeR) # needed for processing, such as TMM
library(limma) # needed to find DE probes


################################################################
# We first find the top 10 probes differentially expressed
# between males and females (see limma.R for details)
################################################################

###################
# Get the data
###################

data(XenaData)

# limit to desired cohort
blca <- XenaData %>% filter(XenaCohorts == 'GDC TCGA Bladder Cancer (BLCA)')

# Get the phenotype / clinical data
cli_query = blca %>%
  filter(Label == "Phenotype") %>%  # select clinical dataset
  XenaGenerate() %>%  # generate a XenaHub object
  XenaQuery() %>%     # generate the query
  XenaDownload()      # download the data

# prepare (load) the data into R
blca_pheno <- XenaPrepare(cli_query)

# Get the RNA-seq data, including the "probe map"
cli_query <- blca %>% filter(Label == 'HTSeq - Counts') %>%
  XenaGenerate() %>%  # generate a XenaHub object
  XenaQuery() %>%
  XenaDownload(download_probeMap = TRUE)

# prepare (load) the data into R
blca_counts <- XenaPrepare(cli_query)

########################################################
# Pre-process the data to get X (expression matrix),
# Y (clinical info), and probeMap
########################################################

# for X, we need to set the rownames and remove the probe column
# from the data matrix
X <- data.frame(blca_counts$TCGA.BLCA.htseq_counts.tsv.gz)
rownames(X) <- X$Ensembl_ID
X <- X[,-1]  # remove the probe name column

# probeMap = probe names
probeMap <- blca_counts$gencode.v22.annotation.gene.probeMap

# Y = pheno data
Y <- blca_pheno

# 'change '.' to '-' so sample ID format is consistent
colnames(X) <- gsub('\\.', '-', colnames(X))


# keep tumor samples ending in 01A
g <- grep('01A$', colnames(X))
X <- X[,g]


# match expression to clinical data
common_samples <- intersect(colnames(X), Y$submitter_id.samples)
mx <- match(common_samples, colnames(X))
my <- match(common_samples, Y$submitter_id.samples)

X <- X[,mx]
Y <- Y[my,]

# Make sure that the samples match -- if they don't, this will produce an error
stopifnot(all(colnames(X) == Y$submitter_id.samples))


#################################
# Process the expression data
#################################

# convert data from log2 (counts + 1) to counts
X <- round(2**X - 1)

# first create a Digital Gene Expression (DGE) list object,
# which contains counts and library size information
dge <- DGEList(counts=X)

# remove genes with low counts
keep <- filterByExpr(dge,min.prop = .10 )
dge <- dge[keep,,keep.lib.sizes=FALSE]


# apply TMM normalization, and calculate log CPM
dge <- calcNormFactors(dge, method = "TMM")
logCPM <- cpm(dge, log = TRUE, prior.count = 3)


################################################################
# Find differentially expressed (DE) probes between males
# and females
###############################################################

# construct design matrix
gender <- Y$gender.demographic
design <- model.matrix(~-1+gender)

# let's change the column names -- we need to reference them below
colnames(design) <- c("Female", "Male")

# 'lmFit' fits a linear model to each row of the expression matrix ##
fit <- lmFit(logCPM, design)

# Specify the contrasts -- the names here must match column names of 
# design matrix 
contrast.matrix <- makeContrasts(Male - Female,levels=design)

## fit model based on contrasts (e.g., Male - Female)
fit <- contrasts.fit(fit, contrast.matrix)

# calculate moderated t-statistics by moderating standard errors
# toward the expected value, using limma trend.
fit.de <- eBayes(fit, trend = TRUE)

# get the top differentially expressed probes, 
# sorted by p-value ('topTable' gives top 10 by default)
tt <- topTable(fit.de,sort.by = "p")
tt


##############################################################################
# In classification problems, it is often desirable to scale each probe,
# so that no single probe has a dominating effect. Below is an example of
# why we do this
##############################################################################

##############################################################################
# function to print the Euclidian distance matrix of x and to 
# plot the cluster
#############################################################################
plot.clust <-function(x) {
  d <- dist(t(x))
  print(d)
  h <- hclust(d)
  plot(h)
}

## generate data
r <- c(1.5, 2, 3) #each row
M <- rbind(p1=r,p2=r,p3=r,p4=r,p5=r,p6=r,p7=r) # 7 rows
colnames(M) <- c("A","B","C")
M

# View clusters: in this case samples A and B are the closest
# (expected based on M)
plot.clust(M)


# Another example: in this case a probe with large expression
# is added, and samples A and C are the 'closest'.
# But this is not desirable since 'closeness' is now completely 
# determined by probe p8 
M <- rbind(M, p8=c(10,15,10))
M
plot.clust(M)

# Solution is to scale each probe (each row). If each probe is
# on the same scale, each probe is given the same weight in 
# the classification (no single probe dominates)

# We use the following functions:
#   scale - scales each column to have mean 0 and sd of 1
#   t - the transpose (switches rows and columns)
row.scale <-function(x) {
  x.scale <- t(scale(t(x)))
  return(x.scale)
}

# With row scaling, A and B are the most similar
M.scale <- row.scale(M) # scale each row
plot.clust(M.scale)

###############################################################
## Back to RNA-seq data
###############################################################

# With gene expression data, it is usually desirable to scale 
# each probe. Otherwise, probes with relatively high 
# or low expression will drive the classification

## get probes that are differentially expressed
m <- match(rownames(tt), rownames(logCPM))
X <- logCPM[m,]

# Probe visualization: with knn, an unknown observation (?)
# is classified based on it's k-nearest neighbors. Note: only
# 2 dimensions are shown, though 10 probes (dimensions) are 
# used for classification
col.gender <- as.integer(as.factor(gender))
col.gender <- c("pink", "blue")[col.gender]
X.scale <- row.scale(X)


df <- data.frame(probe1 = X.scale[1,], probe2 = X.scale[2,], gender = gender)

ggplot(df, aes(probe1,probe2,color=gender)) + geom_point() +
  theme_classic() + ggtitle("Scaled expression of top 2 probes") +
  scale_color_manual(values = c("hotpink", "blue")) +
  annotate("text", x = 0, y = -1.0, label = "? (M or F)") 

# we will use the plotly package for a 3D plot 
# (this package will need to be installed)
library(plotly)
labels <- list(xaxis = list(title = "probe 1", range = c(-2, 2)),
               yaxis = list(title = "probe 2", range = c(-2, 2)),
               zaxis = list(title = "probe 3",range = c(-2, 2)))
plot_ly(x=X.scale[1,], y=X.scale[2,], z=X.scale[3,], 
        type="scatter3d", mode="markers", color = gender, 
        colors=c("pink", "blue")) %>%
        layout(title = '\nTop 3 probes', scene = labels)


###################################################################
# Let's predict the gender of each individual, using the top 10
# probes. Note that knn requires samples in rows and 
# features (probes) in columns
###################################################################

library(class) # required for knn

###################################################################
# Leave one out cross-validation (loocv):
# for each sample, predict its class after removing it from the 
# training set. 
# Use knn.cv(data,classes, k), where 
#   data - the data (with probes in COLUMNS and samples in ROWS)
#   classes - the known classes corresponding to the data
#   k - value of k for the 'k' nearest neighbors
###################################################################

preds = knn.cv(t(X.scale), gender, k = 3) 
table(true = gender, predicted = preds)

# overall accuracy (% correct) #
sum(preds == gender) / length(gender)

#######################################################################
# The overall accuracy is not a good measure of performance because 
# it is misleading if the data is unbalanced
# The 'sensitivity' (or 'recall') of class 'A' is the probability of 
# correctly classifying samples from group A. We will calculate the
# recall for each group.
#######################################################################

t <- table(true = gender, predicted = preds)
t
prop.table(t, margin = 1)  # relative frequency by row

# Recall for females is 99%; recall for males is 100%

#################################################
# Let's now make a prediction for 3 new samples
#################################################
X.test <- cbind(s1 = c(-4.61, -4.61, 7.47, -4.2, -4.61, -4.61, -4.61, -4.61, -4.61, -4.61),
                s2 = c(7.58, 5.48, -4.61, 5.71, 3.65, 3.63, 5.06, 4.31, 5.71, 5.29),
                s3 = c(-4.61, -4.61, 4.67, -4.61, -4.61, -4.61, -4.61, -4.61, -4.61, -4.61)
          )
  
  
# Testing data should have same scaling as training data
# the function below takes a previously scaled X matrix and
# applies its scaling to rows of the matrix X
scale.transform <- function(scaledX, X) {
  X <- t(X)
  center <- attr(scaledX, "scaled:center")
  sds <- attr(scaledX, "scaled:scale")
  t(scale(X, center, sds))
}

X.test.scale <- scale.transform(X.scale,X.test)

####################################################################
# Making predictions in a test set:

# Use knn(train, test, classes, k), where 
#   train - training data (probes in columns and samples in rows)
#   test - testing data (probes in columns and samples in rows)
#   classes - the known classes corresponding to the training data
#   k - value of k for the 'k' nearest neighbors

# Note: the test data should be scaled the same as the training
# data
####################################################################

preds <- knn(t(X.scale), t(X.test.scale), gender, k = 3)
preds


########################################################################
# General classification procedure:
#   Choose a classifier, and use leave-one-out cross-validation to 
#   estimate the number of probes/genes to use (based on number, or FDR
#   cutoff), and classification parameters such as 'k' in knn. Then 
#   evaluate the classfier on an independent dataset
########################################################################
