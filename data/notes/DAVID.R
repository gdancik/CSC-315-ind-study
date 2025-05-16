###############################################################
# DAVID: Functional analysis of gene sets (gene lists)
# The majority of this script is taken from knn/limma.R,
# to identify differentially expressed genes between 
# males and females. We then provide directions for the
# functional analysis of these genes
################################################################

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

# get differentially expressed probes using an FDR of 1% 
tt <- topTable(fit.de,sort.by = "p", p.value = .01, number = Inf)
tt

##############################################################
# Get genes for DAVID
##############################################################

m <- match(rownames(tt), probeMap$id)
genes <- probeMap$gene[m]
genes <- unique(genes) # get unique set of genes (removes duplicates)

## view/save genes for input into DAVID (https://david.ncifcrf.gov/)
## you may save the genes to a file by setting the file argument,
## if a path is not specified, the file will be saved in your
## current working directory - see getwd() - by default
write.table(genes, row.names = FALSE, quote = FALSE)

########################################################
# We will use DAVID (https://david.ncifcrf.gov/) for
# functional analysis of gene lists.

# Select Start Analysis, and from the Upload tab on the left,
# upload or paste your gene list;

# Set the identifier to OFFICIAL_GENE_SYMBOL, 
# Enter your species (human = Homo sapiens), 
# Under List Type, select Gene List.

# Click Submit List

# If < 80% of your genes map, make sure you have selected the right species.
# If so, continue the analysis with the genes that do map

# Make sure that annotations are limited to the correct species

# If the background species is not correct, click on Background and select the species

# Select Functional Annotation Tool

# Then select the following to view the biological processes
#   and pathways

# 1) Gene_Ontology --> GOTERM_BP_DIRECT (click on Chart)
# 2) Pathways --> KEGG_PATHWAY (click on Chart)


#######################################################################################
# Additional example: the genes below are part of a Cell Cycle 
# Progression (CCP) signature that predicts outcome in bladder and lung 
# cancer (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0085249).
# What biological (GO) terms and KEGG Pathways areassociated with these genes, based on DAVID?
#######################################################################################

genes <- c("ASF1B", "ASPM", "AURKA", "BIRC5", "BUB1B", "C18orf24", 
           "CDC2", "CDC20", "CDCA3", "CDCA8", "CDKN3", "CENPF", "CENPM",
           "CEP55", "DLGAP5", "DTL", "FOXM1", "KIAA0101", "KIF20A", 
           "MCM10", "NUSAP1", "ORC6L", "PBK", "PRC1", "PTTG1", "RAD51",
           "RAD54L", "RRM2", "TK1", "TOP2A")


write.table(genes, row.names = FALSE, quote = FALSE)
