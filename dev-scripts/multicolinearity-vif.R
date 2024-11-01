# Testing for multicolinearity with variance inflation factors (VIF)
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2018-09-04

################################################################################

#' Want to see if predictors are collinear, so we need to calculate variance 
#' inflation factors (VIF) for each predictor. If any predictor has VIF > 5, 
#' it is probably too correlated with other predictors to be included in the 
#' model. There is also some good background information on Wikipedia:
#' https://en.wikipedia.org/wiki/Variance_inflation_factor

#' The basic approach for VIF is to regress one predictor on all the remaining
#' predictors, use the r-squared value from that model to calculate the VIF, 
#' where VIF is the inverse of 1 - r-squared. For example, if we have a 
#' regression model Y1 ~ X1 + X2 + X3, we need to run three models to calculate
#' VIF for each of the predictors:
#' X1 ~ X2 + X3 # To calculate VIF for X1
#' X2 ~ X1 + X3 # To calculate VIF for X2
#' X3 ~ X1 + X2 # To calculate VIF for X3
#' The steps to do this for X1:
#' x1.model <- lm(formula = X1 ~ X2 + X3)
#' x1.r2 <- summary(x1.model)$r.squared
#' x1.vif <- 1 / (1 - x1.r2)
#' To make this abstract and repeatable, we can write a function to do this:

vif <- function(formula, data = NULL) {
  reg.model <- lm(formula = formula, data = data)
  r2 <- summary(reg.model)$r.squared
  return(1 / (1 - r2))
}

#' We can see this in use with the mtcars data set. We are interested in a 
#' multivariate model predicting fuel efficiency (in miles per gallon). The 
#' full model would be:
#'     mpg ~ disp + hp + wt + qsec
#' Where the predictors are displacement (disp), horsepower (hp), 1/4 mile time 
#' (qsec) and weight (wt). 
#' 
#' The general approach is to calculate VIF for each of the predictors. If any 
#' has a VIF above 5, drop the predictor with the highest VIF from the model and 
#' re-calculate the VIF for all remaining predictors.
#' To calculate VIF for displacement, we are evaluating this model:
#'     disp ~ hp + wt + qsec
#' so the call to vif would be:
vif(formula = disp ~ hp + wt + qsec, data = mtcars)
#' [1] 7.985439

#' Now we can do this for each of our remaining predictors
#' hp
vif(formula = hp ~ disp + wt + qsec, data = mtcars)
#' [1] 5.166758

#' wt
vif(formula = wt ~ hp + disp + qsec, data = mtcars)
#' [1] 6.916942

#' qsec
vif(formula = qsec ~ hp + wt + disp, data = mtcars)
#' [1] 3.133119

#' In this case, the highest VIF was for disp, so we drop that one and re-run 
#' the VIF calculations with the remaining three predictors (note the disp 
#' variable has been dropped from all calculations)
#' hp
vif(formula = hp ~ wt + qsec, data = mtcars)
#' [1] 4.921956

#' wt
vif(formula = wt ~ hp + qsec, data = mtcars)
#' [1] 2.530443

#' qsec
vif(formula = qsec ~ hp + wt, data = mtcars)
#' [1] 2.873804

#' All VIF values are below 5, so we are set to test the model:
#'     mpg ~ hp + wt + qsec
#' Which does not, according to the VIF criterion, suffer from substantial 
#' multicolinearity.