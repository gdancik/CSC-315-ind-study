####################################################################
# Name:
# Lab 8: Intro to RNA-Seq and Xena
# Turn in a Notebook that answers the questions below
####################################################################

library(edgeR)
library(UCSCXenaTools)
library(dplyr)
library(ggplot2)


##########################################################################
# 1) Consider a reference genome with genes as follows:
#    AGAGAGAAG|AACAAGGGCAACAAC|AAATAACAA
#     gene 1 |   gene 2   | gene 3

# Suppose that mRNA is extracted from a sample, and during sequencing is
# broken into the following fragments: AAT, GCA, ATA, GAA, AGG, GGC, GGG
# 
# What are the read counts for each of the 3 genes?
# Gene 1: 
# Gene 2: 
# Gene 3:

# Include your answers in the comment above. Note that you should NOT use
# R to answer this question.


##########################################################################
# 2) Consider the matrix 'm' below, and complete the following steps so 
#    that each column has a mean of 0.

m <- matrix(c(1:10, c(1:3, 10, 4), rep(17,5)), ncol=4)

# (a) Use the 'colMeans' function to create a vector containing the mean of
#     each column

# (b) Use the 'sweep' function to subtract the mean of each column, and
#     store the result.

# (c) Use the 'colMeans' function to verify that the mean of each column is 0

##########################################################################
# 3)  Consider the matrix of read counts below, where the relative 
#     expression between samples 1 and 2 are the same (but the total read 
#     counts are twice as high for sample 2), and gene 3 has higher 
#     expression in sample 3 than in sample 1.

read_counts <- data.frame(s1 = c(10, 20, 15)) %>% 
               mutate(s2 = 2*s1, s3 = s1)
rownames(read_counts) <- paste0("gene",  1:3)
read_counts$s3[3] <- read_counts$s3[3] * 2
read_counts$s3[1:2]  <- read_counts$s3[1:2] - 15/2

# (a) Calculate the vector containing total reads per sample, per million mapped reads.  
#     Note: this is 'N' from the class example.

# (b) The length of the three genes are 100, 200, and 100 bases. Create a vector containing
#     the gene lengths in kilobases. Note: this is 'L' from the class example.

# (c) Use the 'sweep' function as needed to calculate the RPKM values, which gives
#     the Reads Per Kilobase of transcript, per Million mapped reads.

# (d) Use the 'rpkm' function from the edgeR package to calculate the RPKM values (these
#     results should be the same as your calculation in (c))

# 4) Consider the rpkm values in the matrix below

    rpkm_values <- data.frame(s1 = c(5,10,5), s2 = c(5,10,30))
    rownames(rpkm_values) <- paste0('gene', 1:3)

# (a) Calculate the TPM values from the rpkm values matrix
    
# (b) Use cpm(rpkm_values) to calculate TPM values, using
#     the 'cpm' function from 'edgeR'. For every 1 million
#     mapped reads, how many reads would you expect to map
#     to gene2 in sample 1?

    
  
# 5) Show that the number of cohorts available with read count data from the 'gdcHub' 
#    is 41. Note that the column 'XenaHostNames' contains the hub information while the
#    read count data is labeled as 'HTSeq - Counts' in the 'Label' column.
    
    data(XenaData) # load the XenaData data frame
    
    
# The code below loads the phenotype (clinical) data from the GDC / Bladder Cancer
# patient cohort, which is stored in 'blca_pheno'
    
    blca <- XenaData %>% filter(XenaCohorts == 'GDC TCGA Bladder Cancer (BLCA)')
    cli_query = blca %>%
      filter(Label == "Phenotype") %>%  # select clinical dataset
      XenaGenerate() %>%  # generate a XenaHub object
      XenaQuery() %>%     # generate the query
      XenaDownload()      # download the data
    
    # This loads the dataset into R
    blca_pheno <- XenaPrepare(cli_query)
    
# 6) Find the mean and median age of a patient from this cohort. The age information
#   can be found in the 'age_at_index.demographic' column.
    
# 7) Construct a frequency bar graph showing the race of each patient.
#   Race information is contained in the 'race.demographic' column.
    
    
# 8) (a) Load the phenotype data for the cohort: 'GDC TCGA Thyroid Cancer (THCA)'

#   (b) How many patients (rows) are in this cohort?
    
#   (c) Construct a relative frequency table showing the pathological primary
#       tumor (T) stage. Note: this information is in the 
#       column: 'pathologic_T'
#       Note: the tumor stage is assigned based on the size of the tumor and 
#       whether or not the tumor extends beyond the thyroid.
#       More information: https://emedicine.medscape.com/article/2006643-overview    
