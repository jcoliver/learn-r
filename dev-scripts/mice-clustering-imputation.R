# Proof of concept using mice with clustering approaches
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-06-05

library(mice)

# General idea:
# Use mice to impute missing values, creating some number of datasets (10, 100)
# Run clustering analysis on each dataset
# Summarize results...somehow
# Lowest hanging fruit:
# Calculate distance matrix for each imputed dataset
# Average all those distance matrices ("consensus" matrix)
# Run hclust on that matrix for "consensus" cluster

# Start by trying hclust on mtcars
# First calculate Euclidian distance matrix
mtcars_dist <- dist(x = mtcars)
# Now do clustering
mtcars_hclust <- hclust(d = mtcars_dist)
# Plot to see what that looks like; produces a histogram
plot(mtcars_hclust)

# Make a copy of the mtcars dataset to add missing data
my_cars <- mtcars
# Add 10 missing points, by randomly picking row & column numbers; fingers 
# crossed we don't pick same cell twice?
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

# Calculate distance matrix for each imputed dataset
cars_dist <- lapply(X = cars_data, FUN = dist)

# Now create consensus with all those distance matrices
consensus_dist <- Reduce(f = "+", x = cars_dist)/length(cars_dist)

# Finally, run hclust for "consensus" cluster
consensus_clust <- hclust(d = consensus_dist)
plot(consensus_clust)
