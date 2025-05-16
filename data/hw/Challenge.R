library(limma)
library(class)

rm(list = ls()) ## this will delete all objects in the workspace

###################################################################
# Useful functions
###################################################################

## scale rows of given matrix to have mean 0 and sd of 1
row.scale <-function(x) {
  x.scale = t(scale(t(x)))
  return(x.scale)
}

# returns the balanced accuracy
balanced.accuracy <-function(predicted, true) {
  t <- table(true = true, predicted = predicted)
  if (nrow(t) != 2) {
    stop("invalid number of rows in accuracy table")
  }
  acc <- diag(t) / rowSums(t)
  sens1 <- acc[1]
  sens2 <- acc[2]
  list(table = t, sensitivity1 = sens1, sensitivity2 = sens2, balanced.accuracy = mean(acc))
}


###################################################################
# Load the Challenge Data
###################################################################

# X.train - the log2 gene expression data for the training samples
# Y.train - the class labels (NMI or MI) for the training samples
# X.test - the log2 gene expression data for the test samples

load(url("https://gdancik.github.io/CSC-315/data/hw/Challenge.RData"))

###################################################################
# See PDF for instructions
###################################################################


