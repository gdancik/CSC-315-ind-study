library(limma)

# for formatting, output decimal numbers to 3 decimal places
options(digits = 3)

# The data includes 'paired' or 'dependent' samples if 2 or more samples
# are related to each other. With microarray data, this can happen
# when there are 2 or more gene expression profiles from the same 
# patient. If this is the case, the limma analysis is slightly different,
# and requires that the related samples be specified using the 'block'
# argument and the correlation between related samples specified 
# using the 'correlation' argument. An example is given below.
# which is modified from the duplicateCorrelation help page

# generate sample expression data; for the first two probes, make
# expression in samples 4-6 higher than expression in samples 1-3
sd <- 0.3*sqrt(4/rchisq(100,df=4))
X <- matrix(rnorm(100*6,sd=sd),100,6)
rownames(X) <- paste("Probe",1:100)
colnames(X) <- paste0("Sample", 1:6)
X[1:2,4:6] <- X[1:2,4:6] + 2

# View the first 6 probes
head(X)

# generate the clinical information
data.p <- data.frame(Patient = rep(c("Patient1", "Patient2", "Patient3"), 2),
                     Group = c(rep("Before",3), rep("After", 3)))

# view the clinical info - 
# note that we have 6 samples but only 3 patients; for each patient, 
# we have a sample from "Before" and "After" a particular treatment
head(data.p)

# generate the design matrix 
design <- model.matrix(~0+data.p$Group)
colnames(design) <- c("After", "Before")

# Since samples are correlated, we need to find the correlation, which is
# based on the 'blocks' which define the relationships. 

dupcor <- duplicateCorrelation(X,design,block=data.p$Patient)

# now specify the blocks and the correlation in lmFit
fit <- lmFit(X,design,block=data.p$Patient,correlation=dupcor$consensus)

# the rest is the same
contrast.matrix <- makeContrasts(After - Before, levels=design)

## fit model based on contrasts (e.g., Female - Male)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)

# display top 10 results - note that first 2 probes are the most differentially
# expressed
topTable(fit2) 
