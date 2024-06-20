# Proof of concept using mice with clustering approaches
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-06-05

library(mice)

# Approach described below:
#   + Impute multiple datasets
#   + Calculate distance matrix for each imputed dataset (with dist() function)
#   + Average all those distance matrices to create a "consensus" matrix
#   + Run hclust on that matrix for "consensus" cluster

# Another approach (not implemented, but might be possible with clusterMI 
# package, https://cran.r-project.org/web/packages/clusterMI/index.html):
#   + Impute multiple datasets
#   + Run clustering analysis on each imputed dataset
#   + Summarize results on all clustering output

################################################################################
# Start by trying hclust on mtcars (a built-in dataset), just to see how 
# process would work if there were no missing data

# First calculate Euclidian distance matrix
mtcars_dist <- dist(x = mtcars)
# Now do clustering
mtcars_hclust <- hclust(d = mtcars_dist)
# Plot to see what that looks like; produces a dendrogram
plot(mtcars_hclust)

################################################################################
# Now try the imputation approach. Start by creating a dataset that has missing
# data. A fairly brute-force approach is applied here; the ampute function from 
# the mice package is an alternative.

# Make a copy of the mtcars dataset to add missing data
my_cars <- mtcars
# Add 10 missing points, by randomly picking row & column numbers; fingers 
# crossed we don't pick same cell twice :)
num_missing <- 10
row_na <- sample(x = 1:nrow(my_cars), replace = TRUE, size = num_missing)
col_na <- sample(x = 1:ncol(my_cars), replace = TRUE, size = num_missing)
cells_na <- cbind(row_na, col_na)
# For those cells, replace values with missing
my_cars[cells_na] <- NA

# Now run mice to impute data for 10 datasets
cars_imp <- mice(data = my_cars, m = 10)

# A little more gymnastics to get the complete, imputed datasets out;
# mice::complete gives us a list, each element is an imputed dataset
cars_data <- mice::complete(cars_imp, action = "all")

# Calculate distance matrix for each imputed dataset; output is another list, 
# each element is a distance matrix
cars_dist <- lapply(X = cars_data, FUN = dist)

# Now create consensus with all those distance matrices; code below takes the 
# average for each cell across the multiple distance matrices
consensus_dist <- Reduce(f = "+", x = cars_dist)/length(cars_dist)

# Finally, run hclust for consensus cluster on that consensus distance matrix
consensus_clust <- hclust(d = consensus_dist)
plot(consensus_clust)

################################################################################
# Comparisons of mice built-in visualization functions, including:
# bwplot
# densityplot
# plot
# stripplot
# xyplot

# Boxplot of observed and imputed data, by imputation number. Observed data is 
# first box, remaining boxes are imputed values. Appear to *only* show imputed
# values (much smaller sample sizes)
bwplot(cars_imp)

# Density plot, comparing to observed
# densityplot(cars_imp, ~ mpg) # this one errors out?
# densityplot(cars_imp, ~ wt) # errors
# densityplot(cars_imp, ~ qsec) # errors
densityplot(cars_imp, ~ drat)
densityplot(cars_imp, ~ hp)

# Plot of mean & sd for each imputation iteration. Not so useful.
plot(cars_imp)

# Plots all variables and indicates imputed values for each imputation 
# iteration; would like to be able to only show those variables that were 
# imputed...
stripplot(cars_imp)

# Pairwise comparison between two variables. Points for all iterations are 
# shown; blue for observed, red for imputed values
xyplot(cars_imp, hp ~ drat)

################################################################################
# + t-tests comparing observed vs. imputed distributions 
# + boxplots comparing observed vs. imputed distributions 
# + [stretch] scatterplots of pairwise variable comparisons with error bars for 
#   imputed points

# 
# 1. For the visual distribution, how can I do a box plot (with all 52 
#    variables, or only those with missing values)?
# 2. For the ggplot of distribution, how can I use two colors to differentiate 
#    the two types of variables (original and imputed)?
# 3. For the visual comparison between two variables, now I use the 
#    "consensus_data", so there is only one dot for one imputed value, because 
#    that is the mean value of 10 imputed values. But in our meeting, we say we 
#    probably can have 10 little dots for one imputed value, because there are 
#    10 imputed datasets, if not use the mean of them. How can we do that to 
#    consider variation as well?
# 4. How to put error bars on the imputed values in the plot of variable-based 
#    comparison, if possible?

