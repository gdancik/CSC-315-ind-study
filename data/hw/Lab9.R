####################################################################
# Name:
# Lab 9: Identification of differentially expressed genes

# Turn in a Notebook that answers the questions below
####################################################################

library(dplyr)
library(ggplot2)
library(UCSCXenaTools) # needed to retreive data
library(edgeR) # needed for processing, such as TMM
library(limma) # needed to find DE probes

########################################################
# We start by retreiving and processing the data
# This code generates the following objects:
#  logCPM - the TMM normalized expression data on the 
#              log CPM scale
#  Y - the clinical information
#  probeMap - the probeMap data

# logCPM and Y are aligned so that column 'i' of logCPM 
# contains the expression data for the individual with
# clinical data in row 'i' of Y
########################################################


##########################################
# Setup step 1: Retrieve the data
##########################################

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



##########################################
# Setup step 2: Data pre-processing
##########################################

X <- data.frame(blca_counts$TCGA.BLCA.htseq_counts.tsv.gz)
rownames(X) <- X$Ensembl_ID
X <- X[,-1]  # remove the probe name column

# probeMap = probe names
probeMap <- blca_counts$gencode.v22.annotation.gene.probeMap

# Y = pheno data
Y <- blca_pheno


# 'change '.' to '-' so sample ID format is consistent
colnames(X) <- gsub('\\.', '-', colnames(X))

# Keep only the '01A' tumor samples
g <- grep('01A$', colnames(X))
X <- X[,g]


# match expression data to clinical data
common_samples <- intersect(colnames(X), Y$submitter_id.samples)
mx <- match(common_samples, colnames(X))
my <- match(common_samples, Y$submitter_id.samples)

X <- X[,mx]
Y <- Y[my,]

# Make sure that the samples match -- if they don't, this will produce an error
stopifnot(all(colnames(X) == Y$submitter_id.samples))


#############################################
# Setup step 3: Process the expression data
#############################################

# convert from log2(count + 1) to count data
X <- round(2**X - 1)

# remove genes with low counts
dge <- DGEList(counts=X)
keep <- filterByExpr(dge,min.prop = .10 )
dge <- dge[keep,,keep.lib.sizes=FALSE]

# apply TMM normalization, which computes the normalization 
# factors. The actual normalization is done in a later step
dge <- calcNormFactors(dge, method = "TMM")

# Calculate the log CPM values, using the normalization factors;
# 3 counts are added to each observation to prevent log 0 values
logCPM <- cpm(dge, log = TRUE, prior.count = 3)


########################################################################
# Questions: Use the logCPM, probeMap, and Y objects to answer the
# questions
########################################################################

# 1) Find the mean expression of the probe "ENSG00000215704.8"

# 2) Use ggplot to construct side-by-side boxplots for the expression
#    of "ENSG00000215704.8" between males and females (do not worry about
#    the FC and p-value for this comparison). The title of the graph should
#    be the probe name and the y-axis should be labeled "log CPM". Based 
#    on the graph, does it appear that this probe is differentially
#    expressed?


##########################################################################
# Beginning with question 3, we will identify probes that are 
# differentially expressed between "High Grade" and "Low Grade" tumors. 
# This data has some missing values, which we replace with "unknown" 
# because  missing values are not allowed in the design matrix. 
# High grade tumors grow more quickly than low grade tumors, and are 
# associated with poorer outcomes
##########################################################################

grade <- Y$neoplasm_histologic_grade
grade[is.na(grade)] <- "unknown"


# 3) Use limma to find the top 10 differentially expressed probes between
# low and high grade tumors, and confirm that the top probe is 
# "ENSG00000198670.10"

# (a) construct design matrix

# (b) fit the linear model to each row of the expression matrix

# (c) specify and fit the contrasts

# (d) apply the 'eBayes' step to calculate moderated t statistics

# (e) find the top 10 differentially expressed probes

# (f) confirm that top probe is "ENSG00000198670.10"


# 4) Construct side-by-side boxplots (using ggplot) comparing the 
#    expression of the top probe across high grade and low grade samples.
#    Filter out the "unknown" samples so they do not appear in the
#    boxplot. Your graph should include the probe name and the FC
#    You may specifically look at the probe "ENSG00000198670.10"
#    if your previous answer is not correct.


# 5) Using the probes below (which are the top 10 probes), 
#    generate a heatmap that color codes the columns as follows:
#    low grade = "lightgreen"; high grade = "magenta";
#    unknown = "black". It is typical to use yellow/blue for the
#    expression data, but you may change these colors if you prefer
#    Note that factor levels are in alphabetical order. Your heatmap
#    should also display the gene names instead of the probe names.

probes <-
  c("ENSG00000198670.10", "ENSG00000065911.10", "ENSG00000261713.5", 
    "ENSG00000136052.8", "ENSG00000108960.6", "ENSG00000269986.1", 
    "ENSG00000276972.1", "ENSG00000169504.13", "ENSG00000086300.14", 
    "ENSG00000271579.1")


# 6) Use the top table function and find the number of probes that
#    are differentially expressed with an FDR of < 5%. 

# 7) Find the FC and p-value of the probe "ENSG00000163564.13".
#    Hint: you will need to re-run the topTable function to get
#    results for all probes

# 8) The gene TREX1 (three prime repair exonuclease 1) plays a role
#    in DNA repair. What probe or probes correspond to TREX1?


# Note: Your results from question (6) provide a candidate set of genes
# that could be used to better predict risk for patients with bladder
# cancer, and can also provide therapeutic targets (genes that could be
# targeted for better bladder cancer treatment)
