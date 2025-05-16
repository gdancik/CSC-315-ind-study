####################################################
# Dogs Example
####################################################

library(ggplot2)
library(dplyr)
library(cowplot)

# H0: p = 1/7
# H1: p != 1/7

# Study finds p.hat = 15/54  (so n = 54) 

# What is the distribution of p.hat (the sample proportion) 
# under the null hypothesis???

p.hat <- 15/54   # the sample proportion
p <- 1/7         # the true proportion under the null hypothesis
n <-  54         # the sample size

mu <- p          # the expected value of the sample proportion
sd <- sqrt(p*(1-p)/n) # the sd of the sample proportion

df.phat <- data.frame(x = seq(mu-4*sd, mu+4*sd, length.out = 100)) %>%
           mutate(y = dnorm(x, mean = mu, sd = sd))
                      
plot.phat <- ggplot(df.phat) + geom_line(aes(x, y)) +
  theme_classic() + ggtitle("p.hat ~ N(1/7, sqrt((1/7)(6/7)/54)") +
  labs(x = "p.hat", y = "density") +
  geom_vline(xintercept = p.hat) + 
  geom_text(aes(x=p.hat-.04, y=6), label = "observed p.hat")
plot.phat

###############################################################
# The test statistic is, in general equal to
# Z = (observed.value - mean) / standard deviation
# For the sample proportion, its test statistic is
# Z = (p.hat - mu) / sd, where sd = sqrt(p0(1-p0)/n),
# and is Z ~ N(0,1), under H0
###############################################################
Z = (p.hat - mu) / (sd)
x = seq(-4,4, length.out = 100)

df.z <- data.frame(x = seq(-4, 4, length.out = 100)) %>%
  mutate(y = dnorm(x))

plot.z <- ggplot(df.z) + geom_line(aes(x, y)) +
  theme_classic() + ggtitle("Z ~ N(0,1)") +
  labs(x = "Z", y = "density") +
  geom_vline(xintercept = Z) +
  geom_text(aes(x=Z-.8, y=.25), label = "observed Z")
plot.z

# compare distributions of p.hat with observed proportion and the 
# standard normal distribution with the observed Z statistic
plot_grid(plot.phat, plot.z, nrow = 2)


###########################################
# calculate p-value from test statistic Z
# the p-value is the area in the tails
###########################################
p.value = 2*pnorm(-abs(Z))
p.value

###########################################
# use prop.test
###########################################
p2 = prop.test(15,54, p = 1/7, correct = FALSE)
p2$statistic # prop.test calculates X-squared statistic = Z^2
sqrt(p2$statistic[[1]]) # confirm that our statistics match
p2$p.value  # confirm that our p=values match

###########################################
# more accurate with continuity correction
###########################################
p3 = prop.test(15,54, p = 1/7, correct = TRUE)
p3$p.value

# What is the conclusion regarding the null hypothesis, in the context
# of this problem?

# Because p-value < 0.05, reject H0 and accept HA
# There is sufficient evidence that the dogs are
# able to detect cancer samples


# Coke vs. Pepsi example

# (i) find the test statistic

# (ii) find the p-value

# (iii) state the conclusion

# Additional questions: 

# What would it mean in the context of this hypothesis test if a Type I error 
# occurred?
 
# What would it mean in the context of this hypothesis test if a Type II error 
# occurred?
