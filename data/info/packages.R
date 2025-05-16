#####################################################################################
# CSC 315: Required packages
# Run this script to install the packages we will be using the semester
# These packages are already installed on our classroom computers
#####################################################################################

# package for data manipulation and visualization
install.packages('tidyverse')

# package to combine multiple ggplot2 plots into a single plot
install.packages('cowplot')

# package for knitting HTML documents
install.packages('rmarkdown')

#package for permutations
install.packages('gtools')

## package for colorRampPalette function used to color heatmaps ##
install.packages('RColorBrewer')

##################################################
# Packages for Genomic Data Analysis
##################################################

install.packages("UCSCXenaTools")

##################################################
# Bioconductor packages
##################################################

# Bioconductor packages are now installed through BiocManager
install.packages('BiocManager')

# load BiocManager and install the packages
library(BiocManager)
BiocManager::install('limma')
BiocManager::install('edgeR')

