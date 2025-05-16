################################################################
# GEO-and-limma.R. This script demonstrates how to download 
# data from GEO and how to use the limma package to 
# identify differentially expressed probes
################################################################

library(dplyr)
library(ggplot2)
library(GEOquery) # needed to obtain data from GEO
library(limma) # needed to find DE probes

###########################################################
# Get the processed data for GSE1297. The object
# returned by getGEO is a LIST of AffyBatch objects
# getGEO returns a list because each GEO Series
# may contain multiple platforms
###########################################################

GSE1297 <- getGEO("GSE1297")

# GSE1297 is a list of length 1
typeof(GSE1297)
length(GSE1297)

# summary of GSE1297
GSE1297


###########################################################
# Pull out gene expression data and pheno type data
# (same functions as before, but GSE1297 is a list,
# and we want the first element)
###########################################################

GSE1297.expr <- exprs(GSE1297[[1]])
GSE1297.p <- pData(GSE1297[[1]])

#  We are looking at processed data, but it might not
#  be on the log2 scale. We need to check that the boxes 
#  are visible and that the distributions look similar across samples. 
#  Typically, boxes for log2 data will be between 5 and 10
boxplot(GSE1297.expr, main = "processed data")

# In this case, data is not on the log2 scale. Therefore
# we need to take log2. This is very important -- if data 
# is not on the log2 scale, then downstream analysis will 
# be WRONG. Processed data from GEO may or may not be on 
# the log scale, so make sure to check)
GSE1297.expr <- log2(GSE1297.expr)
boxplot(GSE1297.expr, main = "log2 processed data")

#################################################################
# How many males and how many females are there?
#################################################################
gender <- as.character(GSE1297.p$characteristics_ch1.6)
table(gender)

#################################################################
# For summer:
# Let's check if the probe 214218_s_at is differentially 
# expressed between males and females.

# H0: mu_m - mu_f = 0
# H1: mu_m - mu_f != 0,

# where mu_m is the mean expression in males and mu_f is 
# the mean expression in females

# Note: This probe is for gene XIST, which is found on the
# X chromosome (females have 2 X chromosomes, males have 1)
#################################################################

m <- match('214218_s_at', rownames(GSE1297.expr))
df <- data.frame(x= GSE1297.expr[m,], gender = gender)

ggplot(df, aes(gender, x,fill = gender)) + 
  geom_boxplot() + theme_classic() +
  labs(y = 'log2 expression') + 
  ggtitle('XIST (214218_s_at) expression for females and males')

# carry out the t-test
s <- split(df$x, df$gender)
res <- t.test(s$`Sex: F`, s$`Sex: M`)
res$p.value

# Because p-value is < 0.05, we reject H0 and accept H1.
# There is sufficient evidence that the probe is
# differentially expressed. In this case, expression is 
# upregulated (higher) in females

################################################################
# Find differentially expressed (DE) probes between males
# and females. We will use limma, which requires us to design 
# a model.matrix using indicator variables and to specify the 
# contrasts we are interested in (e.g., Females - Males)
###############################################################

# construct design matrix
design <- model.matrix(~-1+gender)
head(design) # note that indicator variables are used

# let's change the column names
colnames(design) <- c("Female", "Male")

# 'lmFit' fits a linear model to each row of the expression matrix ##
fit <- lmFit(GSE1297.expr, design)

# for each probe, limma calculates the mean for each group 
#  as well as the standard deviation
head(fit$coefficients)
head(fit$sigma)

# Specify the contrasts -- the names here must match column names of 
# design matrix 
contrast.matrix <- makeContrasts(Male - Female,levels=design)

# the null hypothesis is that the contrast (Male - Female in this case) is 0
contrast.matrix

## fit model based on contrasts (e.g., Male - Feale)
fit2 <- contrasts.fit(fit, contrast.matrix)
head(fit2$coefficients)
head(fit2$sigma)

# calculate moderated t-statistics by moderating standard errors
# toward a common value; this makes answers more robust
fit2 <- eBayes(fit2)

# get top probes, sorted by p-value ('topTable' gives top 10 by default)
tt <- topTable(fit2,sort.by = "p")
tt

###############################################################
# let's confirm the top probe
###############################################################

# create data frame with expression values and gender
probe <- rownames(tt)[1]
m <- match(probe, rownames(GSE1297.expr))
df <- data.frame(expr = GSE1297.expr[m,], gender = gender)

# group data frame by gender, then summarize by expression value (new approach)
means <- df %>% group_by(gender) %>% summarize(mean = mean(expr))
means

diff(means$mean) # -4.675664
tt$logFC[1]

# convert to FC #
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
#          find all DE probes, this should be the total
#          number of probes in the dataset. Note that
#          the default value is 10, so this must be set
#          to get more than 10 probes
tt.05 <- topTable(fit2,sort.by = "p", p.value = 0.05, number = nrow(GSE1297.expr))
nrow(tt.05)

#######################################################################
# Aside: A closer look at the eBayes step above; eBayes 
# (empirical Bayes) "shrinks" the estimated standard deviations towards  
# a common value (based on the assumption that most genes are not 
# differentially expressed). This leads to more robust results and
# increased 'power' to detect differentially expressed genes
#######################################################################

# create data frame for normal and adjusted std devs
df <- data.frame(sigma.original = fit2$sigma, sigma.adjusted = sqrt(fit2$s2.post))

# sort the data frame by sigma.original
df <- arrange(df, sigma.original)

# add a column of x values from 1 - nrow(df)
df <- mutate(df, x = 1:nrow(df))

plot1 <- ggplot(df) + geom_line(aes(x, sigma.original, color = "original")) +
             geom_line(aes(x, sigma.adjusted, color = "adjusted")) +
             theme_classic() +
             ggtitle("eBayes \"shrinks\" sigma towards a common value") +
             scale_color_manual(values = c("red", "black")) +
             geom_hline(yintercept = sqrt(fit2$s2.prior[1]), linetype = 2) +
             theme(legend.title = element_blank()) +
             xlab("index (smallest to highest)") + ylab("std dev") + 
             annotate("text", x = 19000, y=.5, label = "common std dev")

print(plot1)
print(plot1 + xlim(0,5000) + ylim(0,0.7))


###############################################################
# Create a heatmap using the top probes (FDR < 0.05)
###############################################################

# extract expression values for DE probes
m <- match(rownames(tt.05), rownames(GSE1297.expr))
X <- GSE1297.expr[m,]

# create a color range consisting of 200 values between yellow and blue
col.heat <- colorRampPalette(c("yellow", "blue"))(200)

# set colors for gender #
col.gender <- as.integer(as.factor(gender))
col.gender <- c("pink", "blue")[col.gender]

# Generate the heatmap; note that clustering is done on the original data, 
# but the rows are scaled (converted to z-scores) by default for 
# visualization
heatmap(X, ColSideColors = col.gender, col = col.heat)

########################################################################
# Aside: we can also cluster samples manually, using
#   dist: calculates pairwise distance between rows (default
#       distance is Euclidian distance)
#   hclust: clusters samples based on a distance matrix
########################################################################
d <- dist(t(X)) # calculate distances between samples, using t(X)
h <- hclust(d)  # cluster the samples

#plot the clusters
label <- gsub("Sex: ", "", gender)
plot(h, label = label)


###############################################################
# Let's find the gene associated with the top probe
# This requires using microarray annotations available from
# GEO (see below) or bioconductor (not discussed) 
###############################################################
platform <- annotation(GSE1297[[1]])   
platform

##############################################################################
# the line below uses the getGEO function from GEOquery to 
# download and extract the platform data
##############################################################################

# download platform data from GEO
pl <- getGEO(platform)

# platform object must be converted to a Table (note the capital 'T')
pl <- Table(pl)


##########################################################
# Next, find the gene for the desired probe
# The column names may vary by platform, but for GPL96,
# the probes are in the ID column and the genes are in 
# the "Gene Symbol" column
##########################################################
probe <- rownames(tt.05)[1] # get the probe of interest
m <- match(probe, pl$ID) # find the row for this probe on the platform
pl$`Gene Symbol`[m] # get the gene symbol(s) that correspond to this probe

# get genes for top probes 
m <- match(rownames(tt.05), pl$ID)
pl$`Gene Symbol`[m]

###################################################################
# Sometimes we are interested in a gene and need to identify 
# corresponding probes. We will do this for RPS11. We use 
# 'grep' instead of 'match' because 'match' only returns the index
# of the first match; grep returns the indices of all matches by 
# default (or the matching string if value = TRUE),
# and also allows the use of regular expressions. The exact 
# nature of this search depends on the format of the Gene. 
###################################################################

# some basic regular expression syntax:
# "^a$" - an exact match of 'a' starting from the beginning ("^") and ending
#         at the end ("$") of a string
# "a|b" - matches either 'a' or 'b' anywhere in the string
# "\\ba\\b" - matches the *word* 'a' (\\b is the beginning
#             or end of a word boundary)

x <- c("a", "a dog barked", "the cat meowed")

# matches any string that contains an 'a'
grep("a", x, value = TRUE) 

# matches any string that contains ONLY an 'a'
grep("^a$", x, value = TRUE) 

# matches strings containing either 'dog' or 'cat'
grep("dog|cat", x, value = TRUE) 

# matches strings containing the word 'a'
grep("\\ba\\b", x, value = TRUE)

# To find probes for RPS11, the code below does not work because it will
# find any gene with 'RPS11', in its name, which includes, e.g.,
# MRPS11 and RPS11P1

# returns a vector of indices where the Gene Symbol contains 'RPS11'
# (note that value = FALSE by default)
g <- grep("RPS11", pl$`Gene Symbol`)  
View(pl[g,])

# In general, the following regular expression will work in order to 
# match the entries containing the gene (written as a 'word'): \\bgene\\b

# Note that a word match is necessary here beause the 
# `Gene Symbol` column either contains a single
# gene name, or multiple gene names separated by " /// ". 

g <- grep("\\bRPS11\\b", pl$`Gene Symbol`)

# verify results
View(pl[g,])

# get probes
pl$ID[g]

##############################################################
# How can I find information about specific genes?
# A good reference for genes is the website Gene Cards
#  (http://www.genecards.org/)
##############################################################

