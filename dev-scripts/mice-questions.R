# mice dataset fun
# Jeffrey Oliver
# jcoliver@arizona.edu
# 2021-08-17

library(mice)
library(car)

# Make up some imputed analyses for example (using default of 5 imputed data 
# sets)
imp <- mice(data = nhanes, print = FALSE, seed = 20210817)
fit <- with(imp, lm(bmi ~ chl + hyp))
# Use mice::getfit to get model for each imputed data set
f1 <- getfit(fit)

# 1. model fit for each imputed data set; here we use R-squared, but statistic 
# will vary based on model. i.e. you might want to extract AIC from models that 
# include such measures of fit

# Will hold R squared value for each model
r_squared <- numeric(length(f1))
# Cycle through each model and extract r-squared
for (i in 1:length(f1)) {
  # Pull out model from the ith data set
  one_model <- f1[[i]]
  # Run summary for the ith model
  model_summary <- summary(one_model)
  # Extract R squared
  r_sq <- model_summary$r.squared
  # Store in that r_squared vector
  r_squared[i] <- r_sq
}
# For fun, run summary on the R squared vector to get a sense of variation in 
# model fit
summary(r_squared)

# 2. vif

# Similar approach as above where you extract each model (one for each imputed 
# data set) and run car::vif on the model

# Will hold variance inflation factor for each model, because we have a value 
# for each predictor, we'll use a list to store values
vif_mod <- list(length(f1))
# Cycle through each model and extract r-squared
for (i in 1:length(f1)) {
  # Pull out model from the ith data set
  one_model <- f1[[i]]
  # Run car::vif on model
  model_vif <- car::vif(one_model)
  # Store in that vif_mod list
  vif_mod[[i]] <- model_vif
}
# Convert list to a data frame
vif_df <- data.frame(do.call(rbind, vif_mod))
# Print distribution of VIF for each predictor
summary(vif_df)

# 3. mean center

# Use passive imputation to create mean-centered values following imputation

# Add empty columns that will hold mean-centered values in imputed data sets
nhanes_mc <- nhanes # copying original data
nhanes_mc$bmi_mc <- NA
nhanes_mc$chl_mc <- NA
nhanes_mc$hyp_mc <- NA

# Run mice initially to create prediction and method matrices
ini <- mice(data = nhanes_mc, print = FALSE, seed = 20210817)
# We can ignore the warnings, as we aren't going to use the ini imputed 
# datasets for anything
pred <- ini$pred

# Set the mean-centered values as ones we do NOT want to actively impute
pred[c("bmi", "chl", "hyp"), c("bmi_mc", "chl_mc", "hyp_mc")] <- 0

# Use the method matrix to indicate how to mean center variables post-imputation
meth<- ini$meth
meth["bmi_mc"] <- "~I(bmi - mean(bmi))"
meth["chl_mc"] <- "~I(chl - mean(chl))"
meth["hyp_mc"] <- "~I(hyp - mean(hyp))"

# Run passive imputation with those pred and meth matrices
passive_imp <- mice(data = nhanes_mc, 
                    meth = meth, 
                    pred = pred, 
                    seed = 20210817,
                    print = FALSE)

# For the paranoid, check that first data set to make sure the values were 
# actually mean-centered
# Extracts the first imputed data set
passive_one <- complete(data = passive_imp, 
                        action = 1)
# Should show Mean = 0.00 for bmi_mc, chl_mc, and hyp_mc
summary(passive_one)

# Run the analysis, using mean-centered variables in function call
pass_fit <- with(passive_imp, lm(bmi_mc ~ chl_mc + hyp_mc))
# Proceed as you see fit
