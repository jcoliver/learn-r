# Plot interaction from mice output
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-10-05

# Want to plot predictions for model based on imputed data
library(mice)
library(dplyr)
library(tidyr)
library(ggplot2)

# Make up some imputed analyses for example (using default of 5 imputed data 
# sets)
imp <- mice(data = nhanes2, print = FALSE, seed = 20210817)

# Fit a model with interaction term
fit <- with(imp, lm(bmi ~ chl * hyp))

# Pool the results
pooled_fit <- pool(fit)

# Create the "data" that will be plotted. We ultimately want:
# bmi  hyp     chl
# ...   no     min chl for hypertension = no
# ...   no     max chl for hypertension = no
# ...  yes     min chl for hypertension = yes
# ...  yes     max chl for hypertension = yes
# Where "..." represents the predicted value of bmi for corresponding values of 
# hyp and chl (the predictor variables)

# Want to restrict to observed values of our continuous variable chl (i.e. 
# don't want lines beyond our observed data); we do this separately for two 
# values of hyp. dplyr to the rescue!
plot_data <- nhanes2 %>%
  drop_na() %>%
  select(hyp, chl) %>% # Only need predictor variables
  group_by(hyp) %>%    # Get min/max for two values of categorical variable
  summarize(min_chl = min(chl),
            max_chl = max(chl)) %>%
  pivot_longer(cols = -hyp, # A little data wrangling to get table right
               names_to = "val",
               values_to = "chl") %>%
  select(-val) %>%
  ungroup()

# Now to add the predicted values of our response variable (bmi). The formula 
# for the prediction will be different based on the value of hyp. For hyp = no, 
# it's:
#   bmi = Intercept + B1 * chl
# For hyp = yes, it's:
#   bmi = Intercept + B1 * chl + B2 + B3 * chl, or
#   bmi = Intercept + B2 + (B1 + B3) * chl

# Where B1, B2, and B3 are the pooled coefficient estimates from the model
# We need to pull out those B values from the pooled_fit object
B0 <- pooled_fit$pooled$estimate[1] # aka the intercept
B1 <- pooled_fit$pooled$estimate[2]
B2 <- pooled_fit$pooled$estimate[3]
B3 <- pooled_fit$pooled$estimate[4]

# With those coefficient estimates in hand, do the predicted values for bmi "by
# hand"
plot_data <- plot_data %>%
  mutate(bmi = if_else(hyp == "no",
                       true = B0 + B1 * chl,
                       false = B0 + B2 + (B1 + B3) * chl))

# Whew! Now we can finally plot
predict_plot <- ggplot(data = plot_data,
                       mapping = aes(x = chl, y = bmi, group = hyp, color = hyp)) +
  geom_line()
predict_plot

# For the paranoid, can also make the equivalent plot based on observed data 
# (no imputation), to make sure it's not too far off
obs_plot <- ggplot(data = nhanes2,
                   mapping = aes(x = chl, y = bmi, group = hyp, color = hyp)) +
  geom_smooth(method = "lm", se = FALSE)
obs_plot
