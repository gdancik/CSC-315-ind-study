library(dplyr)

# Check if a coin is biased
# H0: p = 0.50
# HA: p != 0.50,  p = probability of getting heads

# flip a coin 1000 times and carry out a hypothesis test
#   return TRUE if we reject H0 (if p < 0.05)

is_coin_biased <- function() {
  s <- sample(c("H", "T"), 1000, replace = TRUE)
  num_heads <- sum(s == 'H')
  res <- prop.test(num_heads, n = 1000, p = 0.50, correct = TRUE)
  res$p.value <= 0.05
}

# let's look at 20,000 coins
r <- replicate(20000, is_coin_biased())

# even though no coins are biased, we would identify biased
# coins 5% of the time (because we are using a significance
# level of 0.05)
table(r) %>% prop.table()

# In other words, for 20,000 coins, we expect 1000 of them
# to appear biased based on the p-value -- these would all
# be false positives. Therefore, if exactly 1000 appear 
# biased based on the p-value, we do not have any statistical
# evidence that any coins are actually biased (we are not
# surprised)

# However, what if 4000 coins had p-values < 0.05.
# We would expect 1000 of these to be false positives 
# This gives us the following false discovery rate (FDR):

# FDR = number of expected predictions / number of actual predictions

FDR = 1000 / 4000
FDR

# In the context of gene expression, the results are ranked
# by p-value, and we pick the top genes to control the FDR
# e.g., a list of 4000 genes that appear differentially 
# expressed with an FDR of 25%
