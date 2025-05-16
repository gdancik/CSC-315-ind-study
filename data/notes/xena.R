##############################################################################
# Retrieving RNA-seq data from Xena
#   Xena is "an online exploration tool for public and private, 
#   multi-omic and clinical/phenotype data". Xena hosts a large amount
#   of genomic data sets and also provides tools for analysis. Xena 
#   contains many types of genomic data, although we will use it for its 
#   RNA-seq data.

# List of datasets: https://xenabrowser.net/datapages/
# List of data hubs: https://xenabrowser.net/hub/

# The most commonly used data hub is The Cancer Genome Atlas (TCGA), which
# includes genomic data for 33 cancer types*. For more information about
# TCGA, see: https://www.cancer.gov/about-nci/organization/ccg/research/structural-genomics/tcga

# *TCGA has generated over 2.5 petabytes (PB) of data. 1 PB = 1 million GB,
# which is enough to store 2000 years of MP3 encoded music or over 4000 photos / day
# over the course of a human life.

# TCGA data is also housed under the Genomic Data Commons (GDC),
# which uses a consistent set of bioinformatics pipelines to create
# derived datasets: https://portal.gdc.cancer.gov/
##############################################################################

##############################################################################
# We will use the R package UCSCXenaTools to retrieve genomic data. 
# Paper describing R package: 
#   https://joss.theoj.org/papers/10.21105/joss.01627
# Vignette (demo) of R package:
#   https://cran.r-project.org/web/packages/UCSCXenaTools/vignettes/USCSXenaTools.html
##############################################################################

library(UCSCXenaTools)
library(dplyr)

# load table containing all cohorts/datasets
data(XenaData)
View(XenaData)

# Question: How many of each DataSubtype are there?

# Let's look at how many gene expression RNAseq datasets there are
View(XenaData %>% filter(DataSubtype == 'gene expression RNAseq'))

##############################################################################
# Retrieving data
##############################################################################

# We need to select the row from XenaData corresponding to the 
# dataset/label of interest

#############################################################################
# Let's get GDC TCGA Bladder Cancer data
# https://xenabrowser.net/datapages/?cohort=GDC TCGA Bladder Cancer (BLCA)
#############################################################################

# Let's look at the GDC TCGA data 
blca <- XenaData %>% filter(XenaCohorts == 'GDC TCGA Bladder Cancer (BLCA)')



#################################################
# We can only get one dataset/label at a time
# We will first get the phenotype / clinical data 
#################################################

# get sample data 
#   1) We need to filter the data to a single row, then
#       call XenaGenerate, XenaQuery, and XenaDownload.
#      cli_query will be a table containing information about the data to 
#       download,such as the file names
cli_query = blca %>%
  filter(Label == "Phenotype") %>%  # select clinical dataset
  XenaGenerate() %>%  # generate a XenaHub object
  XenaQuery() %>%     # generate the query
  XenaDownload()      # download the data

#   2) This "prepares" the dataset by loading data into R 
#      using the query table
blca_pheno <- XenaPrepare(cli_query)

# Questions -- 
#     1. How many patients (rows) do we have data for?

#     2. How many males/females are there

#     3. What is the distribution of ages?


##########################################
# Next get the RNA-seq data
##########################################

# We want the counts, which has label of 'HTSeq - Counts'
# Note: these values are log2(count + 1)
rnaseq <- blca %>% filter(DataSubtype == "gene expression RNAseq")

# download the count data, along with the "probe map"
cli_query <- blca %>% filter(Label == 'HTSeq - Counts') %>%
  XenaGenerate() %>%  # generate a XenaHub object
  XenaQuery() %>%     # generate the query
  XenaDownload(download_probeMap = TRUE) # download the data

# load the data into R
blca_counts <- XenaPrepare(cli_query)

# The probemap links the Enseml_ID to the gene name.
# Ensemble (https://useast.ensembl.org/index.html) is a genome
# browser and maintains a genome reference sequence that was used for
# genome assembly. More information about a gene can be obtained from
# https://www.ncbi.nlm.nih.gov/gene/.

head(blca_counts$gencode.v22.annotation.gene.probeMap)

# The 'count' data includes the ensemble ID and the count for each sample,
# with value log2(count + 1). 

head(blca_counts$TCGA.BLCA.htseq_counts.tsv.gz)

##############################################################
# Next steps:
#     - Match the expression data to the clinical data
#     - Normalize the data using Trimmed Mean of M values
##############################################################
