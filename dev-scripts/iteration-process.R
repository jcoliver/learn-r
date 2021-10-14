# to lapply
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-08-27

# Make a small data set
poorappetitena <- rnorm(n = 100)
shakeoffthebluesna <- rnorm(n = 100)
botheredna <- 1/(1 + exp((-1) * (2 * poorappetitena + (-2) * shakeoffthebluesna + rnorm(n = 100, sd = 0.1))))
botheredna <- round(x = botheredna, digits = 0)

# Put data together in a data frame
df <- data.frame(botheredna = botheredna,
                 poorappetitena = poorappetitena,
                 shakeoffthebluesna = shakeoffthebluesna)

# Run generalized linear regression on a single predictor
model1 <- glm(botheredna ~ poorappetitena, data = df, family = binomial)

# Run generalized linear regression on a single predictor, where predictor 
# name is stored in a variable
pred_var <- "poorappetitena"
formula_value <- as.formula(paste0("botheredna ~ ", pred_var))
model2 <- glm(formula_value, data = df, family = binomial)

# Now to figure out how to get the coefficients, looking at the things included
# in summary, there is a "coeffients" object
ls(summary(model2))
#                 Estimate Std. Error  z value     Pr(>|z|)
# (Intercept)    0.3606457  0.2527051 1.427141 1.535393e-01
# poorappetitena 1.5678185  0.3375642 4.644504 3.408943e-06

# For now, let's plan on grabbing just the second row (the non-intercept 
# coefficient information)

# Make a function that takes the name of the predictor, reponse, and the data 
# set and pull out coefficients
glm_coeffs <- function(pred_name, resp_name, my_data) {
  # Create that formula object
  formula_value <- as.formula(paste0(resp_name, " ~ ", pred_name))
  # Run the model with that formula we just created
  my_model <- glm(formula_value, data = my_data, family = binomial)
  # Run summary on model
  model_summary <- summary(my_model)
  # Extract the second row of the coefficients object from the summary
  pred_coeffs <- model_summary$coefficients[2, ]
  # And send that back
  return(pred_coeffs)
}

# Test the function 
model3_coeffs <- glm_coeffs(pred_name = "poorappetitena", 
                            resp_name = "botheredna",
                            my_data = df)
model3_coeffs
# Estimate   Std. Error      z value     Pr(>|z|) 
# 1.567818e+00 3.375642e-01 4.644504e+00 3.408943e-06 
# These match with previous results. Yay.

# Now loop over all predcitors, storing results in a 
all_predictors <- c("poorappetitena", "shakeoffthebluesna")

# We need a list to hold our results
results <- list()

# Loop over all values in the predictors vector, use our function, and store 
# results in the data frame. Each time through the loop, the value stored in 
# the i variable will be drawn from the all_predictors vector
for (i in 1:length(all_predictors)) {
  predictor_name <- all_predictors[i]
  model_i <- glm_coeffs(pred_name = predictor_name, 
                        resp_name = "botheredna",
                        my_data = df)
  # Store that result in our list
  results[[predictor_name]] <- model_i
}

# results is a list
results
# $poorappetitena
# Estimate   Std. Error      z value     Pr(>|z|) 
# 1.567818e+00 3.375642e-01 4.644504e+00 3.408943e-06 
# 
# $shakeoffthebluesna
# Estimate    Std. Error       z value      Pr(>|z|) 
# -1.802834e+00  3.715798e-01 -4.851808e+00  1.223408e-06 

# It will probably be easier to look at the results by converting that list of 
# results into a data frame
results_df <- as.data.frame(do.call(rbind, results))

# Finally, to move the name of the predictor from a rowname to an actual column:
results_df$predictor <- rownames(results_df)
rownames(results_df) <- NULL
results_df
# Estimate Std. Error   z value     Pr(>|z|)          predictor
# 1  1.567818  0.3375642  4.644504 3.408943e-06     poorappetitena
# 2 -1.802834  0.3715798 -4.851808 1.223408e-06 shakeoffthebluesna

# 