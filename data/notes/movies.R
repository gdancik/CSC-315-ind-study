###########################################################
# Amazon movie reviews. The data used by this script is a
# processed version of data available from here:
# http://snap.stanford.edu/data/web-Movies.html
###########################################################

library(ggplot2)
library(dplyr)

# read in file; file must be in your working directory or you must
# add the path to the file 
movies <- read.csv("movies_processed.txt")

# how many ratings are there?
nrow(movies)

# how many users are included?
movies$UserID %>% unique() %>% length()

# how many movies are included?
movies$productID %>% unique() %>% length()

###################################################################
# Let's summarize the ratings #
###################################################################
t <- table(movies$Rating)
counts <- data.frame(t)

## relative frequency bar graph using ggplot, based on counts
counts <- mutate(counts, prop = Freq/sum(Freq))
ggplot(counts, aes(x=Var1, y=prop)) +
  geom_col(aes(fill = Var1)) +
  ggtitle("Distribution of movie Ratings") +
  labs(x = "Rating", y = "Relative frequency") +
  theme_classic() + theme(legend.position = "none")


############################################
# Get ratings for each product
############################################
s <- split(movies$Rating, movies$productID)

#####################################
# Look at distribution of number of 
# ratings / product
#####################################
mylengths <- lengths(s)   # same as sapply(s, length)
hist(mylengths, main = "# Ratings / Movie", ylab = "# ratings")

# what was rated the most frequently, and how many ratings?
mylengths %>% sort(decreasing = TRUE) %>% head(n = 3)

#########################################
# What are the top rated movies?
# (we will require at least 200 ratings)
#########################################
avgs <- sapply(s, mean)
avg = avgs[mylengths >=200]
sort(avgs, decreasing = TRUE)[1:10]


#############################################
### Where the $#%* is Shawshank Redemption? #
#############################################
m <- match("B000P0J0EW", names(avgs))
s[m]    # all ratings are high
avgs[m] # average rating 

###############################################
### Customers who enjoyed Shawshank Redemption 
### also enjoyed (or didn't enjoy) .....
###############################################

# get users that rated Shawshank
users <- filter(movies, productID == "B000P0J0EW")$UserID

# get movie ratings for users that rated Shawshank
ss.movies <- filter(movies, UserID %in% users)

# sort movie ratings by frequency per movie
# (require at least 2 ratings / movie)
ss.counts <- count(ss.movies, productID, sort = TRUE)
ss.counts <- filter(ss.counts, n >= 2)

# look at first 5 movies (alphabetically by ID)
first5 <- sort(ss.counts$productID)[1:5]

df <- filter(ss.movies, productID %in% first5)
## compare ratings for: "0767803434" "0767804724" "0767811100"
ggplot(df, aes(productID, Rating)) + 
  geom_boxplot(fill = "red") + theme_classic() +
  ggtitle("Movie ratings for customers who like Shawhank Redemption")
