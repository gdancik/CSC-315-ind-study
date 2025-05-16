################################################################
# Limma.R. This script demonstrates how to use the limma 
# package to identify differentially expressed probes from
# RNA seq data. We will generally follow the process
# described in Chapter 15 of the limma user manual, using
# limma trend. To access this, run the command: limmaUsersGuide()
################################################################

library(dplyr)
library(ggplot2)
library(UCSCXenaTools) # needed to retreive data
library(edgeR) # needed for processing, such as TMM
library(limma) # needed to find DE probes

# Retrieve the 'GDC TCGA Bladder Cancer (BLCA)' data from Xena
# https://xenabrowser.net/datapages/?cohort=GDC TCGA Bladder Cancer (BLCA)

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
# Data pre-processing: we need to do a fair amount of 
# filtering and re-arranging to work with the data
########################################################

# First, let's use more manageable names
#   - X: expression data, with probes as row names
#   - probeMap: the probeMap
#   - Y: the pheno/clinical data

# for X, we need to set the rownames and remove the probe column
# from the data matrix
X <- data.frame(blca_counts$TCGA.BLCA.htseq_counts.tsv.gz)
rownames(X) <- X$Ensembl_ID
X <- X[,-1]  # remove the probe name column

# probeMap = probe names
probeMap <- blca_counts$gencode.v22.annotation.gene.probeMap

# Y = pheno data
Y <- blca_pheno

# The expression and clinical data need to match; currently, e.g.,
# The first column of the expression data does not correspond
# to the first row of the pheno data; the data is also not
# in a consistent format (one has '.' and the other has '-')

# compare sample names between X and Y; they do not match and are not in the same format
colnames(X)[1]
Y$submitter_id.samples[1]

# 'change '.' to '-' so sample ID format is consistent
colnames(X) <- gsub('\\.', '-', colnames(X))

# Note that the sample ID is a barcode that has a special meaning:
#   https://docs.gdc.cancer.gov/Encyclopedia/pages/TCGA_Barcode/
# In particular, the 4th section describes the 'Sample' which is 
#   either tumor (01 - 09) or normal (10-19). For details see:
#   https://gdc.cancer.gov/resources-tcga-users/tcga-code-tables/sample-type-codes

# Summarize these samples, using a handy R trick that takes 
# advantage of the fact that `[]` is a function
s <- strsplit(colnames(X), '-')   # split each string in a vector by a '-'
sapply(s, `[`, 4) %>% table()

# Our expression data contains 411 tumor samples (405 from 1 vial and
# 6 from a different one), and 19 normal samples. We will limit our 
# analysis to tumor samples ending with '01A'. The code below 
# uses a regular expression to find these samples, using the
# special character '$' that denotes the end of a string 
g <- grep('01A$', colnames(X))
X <- X[,g]


# We still need to match the expression data with the clinical data
# Let's do that by first finding the samples that are common
# between the expression and clinical data. We can use 
# intersect(a,b) to return a vector containing the elements common
# to vectors 'a' and 'b'

common_samples <- intersect(colnames(X), Y$submitter_id.samples)

# we then use match(x, t) to get a vector of indices. The value
# x[i] is the index of 't' containing the i^th value of 'x'

mx <- match(common_samples, colnames(X))
my <- match(common_samples, Y$submitter_id.samples)

X <- X[,mx]
Y <- Y[my,]

# Make sure that the samples match -- if they don't, this will produce an error
stopifnot(all(colnames(X) == Y$submitter_id.samples))

# We now have count data for 405 bladder cancer tumor
# samples and the corresponding clinical information
dim(X)

#######################################################
# Process the expression data
#######################################################

###################################################
# Data is log2(counts + 1), so we need to get
# back to the scale of counts
# Note: This was left off of the original script
#   and added after class on 11/10
####################################################
X <- round(2**X - 1)

# first create a Digital Gene Expression (DGE) list object,
# which contains counts and library size information
dge <- DGEList(counts=X)
head(dge$counts)   # dge is a list containing the counts
head(dge$samples)  #      as well as sample/library size information

# remove genes with low counts, since these should not
# be considered in our downstream analysis. The default
# min.count = 10 will be used and we require this min
# count in at least min.prop = 10% of samples
keep <- filterByExpr(dge,min.prop = .10 )
dge <- dge[keep,,keep.lib.sizes=FALSE]

nrow(dge) # how many probes are left?

# apply TMM normalization, which computes the normalization 
# factors. The actual normalization is done in a later step
dge <- calcNormFactors(dge, method = "TMM")

head(dge$counts)    # we still have the counts
head(dge$samples)   #   but now we have the normalization factors


# Calculate the log CPM values, using the normalization factors;
# 3 counts are added to each observation to prevent log 0 values
logCPM <- cpm(dge, log = TRUE, prior.count = 3)


# This approach, which uses limma trend (below), works well if the
# ratio of the largest library size to the smallest is not more than 
# about 3-fold. If this is not the case for your analysis, let me know
# and an alternative approach can be used. Here we verify that the 
# library sizes (normalization factors) are similar
max(dge$samples$norm.factors) / min(dge$samples$norm.factors)

# Added after class on 11/10: In general, using an 
# alternative method called limma voom may be a little 
# better for differences above 3-fold; but the results in this 
# case for the top probes are very similar. For simplicity, 
# we will use limma trend in this class; limma trend and 
# limma voom are described in: 
#   https://genomebiology.biomedcentral.com/articles/10.1186/gb-2014-15-2-r29

################################################################
# To find differentially expressed (DE) probes between males
# and females, we need to design a model.matrix that uses 
# indicator variables for the groups and to specify the 
# contrasts we are interested in (e.g., Females - Males)
###############################################################

# construct design matrix
gender <- Y$gender.demographic
design <- model.matrix(~-1+gender)
head(design) # note that indicator variables are used

# let's change the column names -- we need to reference them below
colnames(design) <- c("Female", "Male")

# 'lmFit' fits a linear model to each row of the expression matrix ##
fit <- lmFit(logCPM, design)

# for each probe, limma calculates the mean for each group 
#  as well as the standard deviation
head(fit$coefficients)
head(fit$sigma)

# Specify the contrasts -- the names here must match column names of 
# design matrix 
contrast.matrix <- makeContrasts(Male - Female,levels=design)

# the null hypothesis is that the contrast (Male - Female in this case) is 0
contrast.matrix

## fit model based on contrasts (e.g., Male - Female)
fit <- contrasts.fit(fit, contrast.matrix)
head(fit$coefficients)
head(fit$sigma)

# calculate moderated t-statistics by moderating standard errors
# toward the expected value, using limma trend. Per the user's manual:
# "This approach will usually work well if the ratio of the largest
# library size to the smallest is not more than about 3-fold, which
# we confirm below. If this is not the case, 'voom' should be used
# (see me if this occurs for you)

fit.de <- eBayes(fit, trend = TRUE)

# get the top differentially expressed probes, 
# sorted by p-value ('topTable' gives top 10 by default)
tt <- topTable(fit.de,sort.by = "p")
tt

###############################################################
# let's confirm the top probes (with the lowest p-value)
###############################################################

# create data frame with expression values and gender
probe <- rownames(tt)[1]
m <- match(probe, rownames(logCPM))
df <- data.frame(expr = logCPM[m,], gender = gender)

# convert from logFC to FC #
logFC <- tt$logFC[1]
2**logFC

## visualize ##
FC <- paste0("FC = ", round(2**logFC, 2))
main <- paste0("Expression of ", probe, ", ", FC)

ggplot(df, aes(x = gender, y = expr, fill = gender)) + geom_boxplot() +
  ylab("log2 expression") + ggtitle(main) +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_classic() + theme(legend.position = "none")

###############################################################
# How many genes have FDR < 5%?
###############################################################

# We need to set the following arguments to topTable:
# p.value - this is the adjusted p-value (FDR) cutoff 
#           (it is NOT the p-value)
# number - the maximum number of probes to return. To
#          find all DE probes, we can set it to Inf. 
#          Note that the default value is 10, so this must 
#          be set to get more than 10 probes
tt.05 <- topTable(fit.de,sort.by = "p", p.value = 0.05, number = Inf)
nrow(tt.05)


# what genes do these probes correspond to?
# Note that all are the X or Y chromosomes; 
# Females are XX, and males are XY, so this makes sense
m <- match(rownames(tt.05), probeMap$id)
probeMap[m,]

# When identifying differentially expressed probes/genes, you should 
# report the gene (probe), FC (or log2 FC), and FDR. You should not 
# report the p-value since it is not meaningful.

###############################################################
# Create a heatmap using the top 10 probes (FDR < 0.05)
###############################################################

probes <- rownames(tt.05)[1:10]

# extract expression values for DE probes
m <- match(probes, rownames(logCPM))
expr <- logCPM[m,]

m <- match(probes, probeMap$id)
genes <- paste0(probeMap$gene[m], ' (', probeMap$chrom[m], ')')
rownames(expr) <- genes

# create a color range consisting of 200 values between yellow and blue
col.heat <- colorRampPalette(c("yellow", "blue"))(200)

# set colors for gender #
col.gender <- as.integer(as.factor(gender))
col.gender <- c("pink", "blue")[col.gender]

# Generate the heatmap; note that clustering is done on the original data, 
# but the rows are scaled (converted to z-scores) by default for 
# visualization
heatmap(expr, ColSideColors = col.gender, col = col.heat, scale = "none")


###############################################################
# What if we are just interested in a specific gene? 
###############################################################

##################################################
# Look for SRY -- but gene is not found; 
#   it was likely removed because of low counts
#################################################
keep <- probeMap$gene %in% "SRY"
probes <- probeMap[keep,]
m <- match(probes$id, rownames(logCPM))
m

##################################################
# Look for XIST
#################################################
keep <- probeMap$gene %in% "XIST"
probes <- probeMap[keep,]
m <- match(probes$id, rownames(logCPM))
m

df <- data.frame(x = logCPM[m,], gender)

tt_all <- topTable(fit.de, number = Inf)


# since we are only looking at a single gene, we report the FC and 
# (unadjusted) p-value
m <- match(probes$id, rownames(tt_all))
tt_all[m,]

# The FC is 2^-7.912 = 0.004 and the p-value is 1.009 x 10^-157 (we would say P < 0.01)
# This is the FC for males / females; equivalently, the FC
# is 2^+7.912 = 241 for females / males. There are two ways
# of describing this relationship (either is correct)
# This gene is up-regulated in females
# This gene is down-regulated in males

# generate a graph
FC <- paste0("FC = ", round(2**tt$logFC[m], 5))
main <- paste0("Expression of XIST, ", FC)
ggplot(df, aes(x = gender, y = x, fill = gender)) + geom_boxplot() +
  ylab("log2 expression") + ggtitle(main) +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_classic() + theme(legend.position = "none")


##############################################################
# How can I find information about specific genes?
# A good reference for genes is the website Gene Cards
#  (http://www.genecards.org/)
##############################################################

###############################################################
# For more about genetic (and other) determinants of sex, see
# https://blog.23andme.com/23andme-research/how-was-your-sex-determined-it-might-be-a-lot-more-complicated-than-you-think/
###############################################################

