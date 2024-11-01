# Outlier detection example
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-10-31

# Start by creating the Poisson regression model with glm. We'll use the 
# built-in mtcars dataset.

# Here we use one continuous variable, the time it takes a car to travel 1/4 
# mile from a stop (qsec), to predict the number of carburetors a car has 
# (carb). Yes, a bit backwards and contrived, but the example works.
carb_glm <- glm(carb ~ qsec, 
                family = "poisson", 
                data = mtcars)
# Use the dfbeta function to calculate the effect on each coefficient for each 
# observation. Big DFBETA means point has oversized effect on model coefficient 
# estimates
carb_dfbeta <- dfbeta(carb_glm)
# But how big is too big? Standard threshold is 2/sqrt(N), where N is sample 
# size
dfbeta_threshold <- 2/sqrt(nrow(mtcars))
# Are any of the values of DFBETA above the threshold? DFBETAs can be positive 
# or negative indicating the direction of influence, but we are only interested 
# in *magnitude*, so we need to take the absolute value of the DFBETA scores. 
# We start by asking if ANY values have a magnitude larger than the threshold.
any(abs(carb_dfbeta) > dfbeta_threshold)
# TRUE
# Yup. Find out which ones. The DFBETA scores are a matrix with as many columns 
# as we have coefficients in our model (in this case, three columns, including 
# one for the intercept in the model). We are not so concerned with which 
# *coefficients* (columns) are being affected, but rather if a particular 
# *observation* (row) is having an oversized effect. So we do some logic math 
# to find any rows of the DFBETA scores matrix that have at least one value 
# above the threshold.
dfbeta_test <- rowSums(abs(carb_dfbeta) > dfbeta_threshold)
# Which rows had at least one value too large?
oversized <- which(dfbeta_test > 0)
# Now we can print out those rows to see which are the offending rows.
mtcars[oversized, ]

# With those rows identified as having an oversized effect, we will then drop 
# them from the dataset, and do the entire model building and evaluation step 
# again.

# Create a subset of data, without the rows that had too-large DFBETAs
mt_subset <- mtcars[-oversized, ]
# Re-run the GLM on the smaller dataset
carb_glm_2 <- glm(carb ~ qsec, 
                  family = "poisson", 
                  data = mt_subset)
carb_dfbeta_2 <- dfbeta(carb_glm_2)
dfbeta_threshold_2 <- 2/sqrt(nrow(mt_subset))
# Any too-large DFBETA values?
any(abs(carb_dfbeta_2) > dfbeta_threshold_2)
# TRUE
# Still another row marked as having too-large effect on coefficients. Re-run 
# the process after dropping that one row.
# Start by identifying and removing that row.
dfbeta_test_2 <- rowSums(abs(carb_dfbeta_2) > dfbeta_threshold_2)
oversized <- which(dfbeta_test_2 > 0)
mt_subset_2 <- mt_subset[-oversized, ]
# Re-run GLM. Again, with that new, reduced dataset.
carb_glm_3 <- glm(carb ~ qsec, 
                  family = "poisson", 
                  data = mt_subset_2)
carb_dfbeta_3 <- dfbeta(carb_glm_3)
dfbeta_threshold_3 <- 2/sqrt(nrow(mt_subset_2))
# Any too-large DFBETA values?
any(abs(carb_dfbeta_3) > dfbeta_threshold_3)
# FALSE
# Whew. OK, for the DFBETA approach, we can be comfortable that our last model, 
# carb_glm_3 is free from oversized influence of any observations in the 
# mt_subset_2 data.

################################################################################
# What about Cook's distance? Usually one or the other approach is fine, but 
# for the paranoid, there is also the option of doing both. We can start with 
# the original GLM model, which was based on all the data.

# First we calculate Cook's distance for each observation in the dataset
carb_cooks <- cooks.distance(carb_glm)
# The threshold for "too big" is based on sample size and model size (number of 
# coefficients in the model)
cooks_cutoff <- 4 / (nrow(mtcars) - length(carb_glm$coefficients) - 2)
# The resulting vector called "influential" will provide the index of the rows 
# that have values of Cook's distance that are above the threshold
influential <- which(carb_cooks > cooks_cutoff)
# We use that vector to print the rows deemed "too influential"
mtcars[influential, ]
# It's that Maserati again. We ended up dropping that row in our DFBETA 
# approach, so let us see if our final model (carb_glm_3) has any rows that are 
# too influential based on Cook's distance (remember that carb_glm_3 is A-OK 
# according to DFBETAs)
carb_cooks_2 <- cooks.distance(carb_glm_3)
# Need to re-calculate the cutoff because our sample size is different
cooks_cutoff_2 <- 4 / (nrow(mt_subset_2) - length(carb_glm_3$coefficients) - 2)
# Find the influential rows
influential <- which(carb_cooks_2 > cooks_cutoff_2)
# Print the influential rows
mtcars[influential, ]
# No rows show up (it is a 0-row matrix), which tells us that this last model, 
# carb_glm_3 has no points with too large effect, based on both the DFBETAs 
# and the Cook's distance metrics. We would then use this model for subsequent 
# interpretations and discussions.