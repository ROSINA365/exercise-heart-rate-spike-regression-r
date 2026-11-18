

############################# Install packages ########################################
required_packages <- c("tidyverse", "janitor", "psych", "GGally", "car")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

#################################  Load packages ###########################################
library(tidyverse)
library(janitor)   
library(psych)
library(GGally)
library(car)



# ########################### Load dataset ###############################

setwd("C:/Users/user/Documents/Data Mining for Managers DSCI 724/Project/Simon R/archive")
##this checks the working directory
getwd()
 
data <- read_csv("exercise_dataset.csv") %>%
  clean_names()  # Clean column names (lowercase, underscores)

# View structure
str(data)

# ##########################################################
# Create Response Variable - Heart Rate Spike
# Formula: Heart Rate Spike = Recorded Heart Rate / (220 - Age)


data <- data %>%
  mutate(expected_max_hr = 220 - age,  # compute expected max heart rate
         heart_rate_spike = heart_rate / expected_max_hr)

#############################################################
################ Select variables for analysis updated with correct column names
selected_vars <- data %>% 
  select(
    heart_rate_spike, 
    bmi, 
    duration, 
    intensity = exercise_intensity, 
    age, 
    gender, 
    weather = weather_conditions
  )

# Convert categorical variables to factors
# used the pipe operator %>% as it takes the object on the left (selected_vars)
# and passes it as the first argument into the function on the right.

selected_vars <- selected_vars %>%
  mutate(
    gender = as.factor(gender),
    weather = as.factor(weather)
  )
#  View selected dataset structure
str(selected_vars)

# ######################## Descriptive Statistics ##########################################
# Numeric summary: mean, sd, min, max

numeric_summary <- selected_vars %>%
  select(where(is.numeric)) %>%
  psych::describe()

print(numeric_summary)


################# Descriptive statistics in flexbox #####################
library(psych)
library(tidyverse)
library(flextable)
library(tibble)

# Select numeric variables only
numeric_vars <- selected_vars %>% dplyr::select(where(is.numeric))

# Compute descriptive statistics
desc_stats <- psych::describe(numeric_vars)

# Convert to clean dataframe for flextable
desc_df <- desc_stats %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::select(Variable, mean, sd, min, median, max) %>%
  dplyr::mutate(
    mean   = round(mean, 4),
    sd     = round(sd, 4),
    min    = round(min, 4),
    median = round(median, 4),
    max    = round(max, 4)
  )

# Create polished flextable (simple but professional)
ft_desc <- flextable(desc_df) |>
  autofit() |>
  fontsize(size = 10, part = "all") |>
  align(align = "center", part = "all") |>
  bold(part = "header") |>
  bg(bg = "#F5F5F5", part = "header")

ft_desc

################################################################################

# ###################### Boxplots for Tukey’s five-number summary 
# One boxplot per numeric variable

numeric_vars <- selected_vars %>% select(where(is.numeric))
numeric_var_names <- colnames(numeric_vars)

for (var in numeric_var_names) {
  boxplot(numeric_vars[[var]], main = paste("Boxplot of", var),
          ylab = var, col = "lightblue")
}

# Set plot layout (e.g., 2 rows x 3 columns, adjust as needed)
par(mfrow = c(2, 3))

for (var in numeric_var_names) {
  boxplot(numeric_vars[[var]], 
          main = paste("Boxplot of", var),
          ylab = var, 
          col = "lightblue")
}

# Reset layout after plotting
par(mfrow = c(1, 1))

# ################################################################################
# ###################### Correlation Matrix numeric variables only

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

# Graphical correlation heatmap
GGally::ggcorr(numeric_vars, label = TRUE, hjust = 0.75)

install.packages("corrplot")
library(corrplot)

# Create visual correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.8,
         tl.col = "black", tl.srt = 45)


########################################################################################
####################### Predictive Results
#  Predictive Results (Base Model) 
# Load pipe and dplyr functions again
library(tidyverse)
# or, at minimum:
# library(dplyr)

# Now this will work:
selected_vars <- selected_vars %>%
  mutate(
    gender  = as.factor(gender),
    weather = as.factor(weather)
  )


# Ensure factors (in case session restarted)
selected_vars <- selected_vars %>%
  mutate(
    gender  = as.factor(gender),
    weather = as.factor(weather)
  )

# Fit base model (no interactions yet)
m_base <- lm(
  heart_rate_spike ~ bmi + duration + intensity + age + gender + weather,
  data = selected_vars
)

# Helpers for metrics
rmse <- function(model) sqrt(mean(residuals(model)^2))
mae  <- function(model) mean(abs(residuals(model)))

# Summary + key metrics
s <- summary(m_base)
cat("\n--- BASE MODEL: heart_rate_spike ~ bmi + duration + intensity + age + gender + weather ---\n")
cat("R2:", round(s$r.squared, 4),
    " | Adj R2:", round(s$adj.r.squared, 4),
    " | RSE:", round(s$sigma, 5), "\n")
cat("AIC:", AIC(m_base),
    " | RMSE:", round(rmse(m_base), 5),
    " | MAE:", round(mae(m_base), 5), "\n\n")

print(s$coefficients)

### putting the values in a flextable
library(flextable)
library(dplyr)
library(tibble)

# Assume your model is m_base
s <- summary(m_base)

# Convert coefficient matrix to data frame
coef_df <- as.data.frame(s$coefficients) %>%
  rownames_to_column("Term") %>%        # move row names into a column
  rename(
    Estimate   = Estimate,
    Std_Error  = `Std. Error`,
    t_value    = `t value`,
    p_value    = `Pr(>|t|)`
  )

# round numeric columns
coef_df <- coef_df %>%
  mutate(
    Estimate  = round(Estimate, 6),
    Std_Error = round(Std_Error, 6),
    t_value   = round(t_value, 3),
    p_value   = round(p_value, 4)
  )

# Create flextable
ft_coef <- flextable(coef_df) |>
  autofit() |>
  fontsize(size = 9, part = "all") |>
  align(align = "center", part = "all") |>
  bold(part = "header") |>
  bg(part = "header", bg = "#F5F5F5")

ft_coef   # this will display your coefficient table nicely

#######################################################################
library(Metrics)
####################### Test Interaction Terms
##  Make sure factors are correctly set
## --------------------------------------------
library(tidyverse)

selected_vars <- selected_vars %>%
  mutate(
    gender  = as.factor(gender),
    weather = as.factor(weather)
  )


############ Base model (already fitted earlier)
##    heart_rate_spike ~ bmi + duration + intensity + age + gender + weather
##
m_base <- lm(
  heart_rate_spike ~ bmi + duration + intensity + age + gender + weather,
  data = selected_vars
)

## ########################################
##  Interaction model:
##    Add plausible interactions:
##    age:intensity (older people at higher intensity)
##    duration:intensity (long & hard sessions)
##    bmi:intensity (higher body mass at higher intensity)
## --------------------------------------------
m_int <- lm(
  heart_rate_spike ~ 
    bmi + duration + intensity + age + gender + weather +
    age:intensity +
    duration:intensity +
    bmi:intensity,
  data = selected_vars
)

## ####################################################
##Compare base vs interaction model
##This tells us if interactions significantly improve fit

anova(m_base, m_int)


## Look specifically at interaction term coefficients

s_int <- summary(m_int)

# Full coefficient table
coef_int_full <- s_int$coefficients
print(coef_int_full)

# Extract only interaction rows (those with ":")
interaction_rows <- grep(":", rownames(coef_int_full))
coef_interactions <- coef_int_full[interaction_rows, ]
cat("\n=== Interaction Terms Only ===\n")
print(coef_interactions)

############################################################################################
## Nice flextable for the interaction terms

if (!require(flextable)) install.packages("flextable")
library(flextable)
library(tibble)
library(dplyr)

interaction_df <- as.data.frame(coef_interactions) %>%
  rownames_to_column("Interaction_Term") %>%
  rename(
    Estimate  = Estimate,
    Std_Error = `Std. Error`,
    t_value   = `t value`,
    p_value   = `Pr(>|t|)`
  ) %>%
  mutate(
    Estimate  = round(Estimate, 6),
    Std_Error = round(Std_Error, 6),
    t_value   = round(t_value, 3),
    p_value   = round(p_value, 4)
  )

ft_interactions <- flextable(interaction_df) |>
  autofit() |>
  fontsize(size = 9, part = "all") |>
  align(align = "center", part = "all") |>
  bold(part = "header") |>
  bg(part = "header", bg = "#F5F5F5")

ft_interactions


##############################################################
######Predicting the base model #############################

## PREDICTIVE RESULTS – BASE MODEL


# Make sure factors are set safe to run again
selected_vars <- selected_vars %>%
  dplyr::mutate(
    gender  = as.factor(gender),
    weather = as.factor(weather)
  )

# Fit base model (no interactions)
m_base <- lm(
  heart_rate_spike ~ bmi + duration + intensity + age + gender + weather,
  data = selected_vars
)

# Helper functions for metrics
rmse <- function(model) sqrt(mean(residuals(model)^2))
mae  <- function(model) mean(abs(residuals(model)))

# Get summary object
s_base <- summary(m_base)

# Print key predictive results
cat("\n--- BASE MODEL: heart_rate_spike ~ bmi + duration + intensity + age + gender + weather ---\n")
cat("R2:      ", round(s_base$r.squared, 4), "\n")
cat("Adj R2:  ", round(s_base$adj.r.squared, 4), "\n")
cat("RSE:     ", round(s_base$sigma, 5), "\n")
cat("AIC:     ", round(AIC(m_base), 2), "\n")
cat("RMSE:    ", round(rmse(m_base), 5), "\n")
cat("MAE:     ", round(mae(m_base), 5), "\n\n")

# Print full coefficient table
print(s_base$coefficients)

# Creating a nice coefficient table for Word
if (!require(flextable)) install.packages("flextable")
if (!require(tibble)) install.packages("tibble")
if (!require(dplyr)) install.packages("dplyr")

library(flextable)
library(tibble)
library(dplyr)

coef_base_df <- as.data.frame(s_base$coefficients) %>%
  rownames_to_column("Term") %>%
  dplyr::rename(
    Estimate   = Estimate,
    Std_Error  = `Std. Error`,
    t_value    = `t value`,
    p_value    = `Pr(>|t|)`
  ) %>%
  dplyr::mutate(
    Estimate  = round(Estimate, 6),
    Std_Error = round(Std_Error, 6),
    t_value   = round(t_value, 3),
    p_value   = round(p_value, 4)
  )

ft_base <- flextable(coef_base_df) |>
  autofit() |>
  fontsize(size = 9, part = "all") |>
  align(align = "center", part = "all") |>
  bold(part = "header") |>
  bg(part = "header", bg = "#F5F5F5")

ft_base  

################################################################################
########################Interaction Terms #####################
## ============================
## INTERACTION TERMS – MODEL AND COMPARISON
## ============================

# Interaction model adding plausible interactions:
# age:intensity
# duration:intensity
# bmi:intensity

m_int <- lm(
  heart_rate_spike ~ 
    bmi + duration + intensity + age + gender + weather +
    age:intensity +
    duration:intensity +
    bmi:intensity,
  data = selected_vars
)

# Summary of interaction model
s_int <- summary(m_int)

cat("\n--- INTERACTION MODEL: with age:intensity, duration:intensity, bmi:intensity ---\n")
cat("R2:      ", round(s_int$r.squared, 4), "\n")
cat("Adj R2:  ", round(s_int$adj.r.squared, 4), "\n")
cat("RSE:     ", round(s_int$sigma, 5), "\n")
cat("AIC:     ", round(AIC(m_int), 2), "\n")
cat("RMSE:    ", round(rmse(m_int), 5), "\n")
cat("MAE:     ", round(mae(m_int), 5), "\n\n")

# Compare base vs interaction model using ANOVA
anova_compare <- anova(m_base, m_int)
anova_compare

# Extract only interaction rows from the coefficient table
coef_int_full <- s_int$coefficients
interaction_rows <- grep(":", rownames(coef_int_full))
coef_interactions <- coef_int_full[interaction_rows, , drop = FALSE]

cat("\n=== Interaction Terms Only ===\n")
print(coef_interactions)

# flextable for interaction terms
interaction_df <- as.data.frame(coef_interactions) %>%
  rownames_to_column("Interaction_Term") %>%
  dplyr::rename(
    Estimate  = Estimate,
    Std_Error = `Std. Error`,
    t_value   = `t value`,
    p_value   = `Pr(>|t|)`
  ) %>%
  dplyr::mutate(
    Estimate  = round(Estimate, 6),
    Std_Error = round(Std_Error, 6),
    t_value   = round(t_value, 3),
    p_value   = round(p_value, 4)
  )

ft_interactions <- flextable(interaction_df) |>
  autofit() |>
  fontsize(size = 9, part = "all") |>
  align(align = "center", part = "all") |>
  bold(part = "header") |>
  bg(part = "header", bg = "#F5F5F5")

ft_interactions  


#############################################################################################
## NON-LINEAR TRANSFORMATIONS

library(tidyverse)

# Create non-linear predictors
selected_vars_nl <- selected_vars %>%
  mutate(
    age_sq      = age^2,
    bmi_sq      = bmi^2,
    duration_sq = duration^2,
    intensity_sq = intensity^2,
    log_age      = log(age),
    log_bmi      = log(bmi),
    log_duration = log(duration),
    log_intensity = ifelse(intensity > 0, log(intensity), NA)   # avoid log(0)
  )

## Fit model with quadratic terms
m_quad <- lm(
  heart_rate_spike ~ 
    bmi + duration + intensity + age + gender + weather +
    age_sq + bmi_sq + duration_sq + intensity_sq,
  data = selected_vars_nl
)

## Fit model with log terms
m_log <- lm(
  heart_rate_spike ~ 
    bmi + duration + intensity + age + gender + weather +
    log_age + log_bmi + log_duration + log_intensity,
  data = selected_vars_nl
)

## Extract metrics for comparison

model_metrics <- function(model) {
  list(
    R2 = summary(model)$r.squared,
    Adj_R2 = summary(model)$adj.r.squared,
    RSE = summary(model)$sigma,
    AIC = AIC(model),
    RMSE = sqrt(mean(residuals(model)^2)),
    MAE = mean(abs(residuals(model)))
  )
}

base_metrics <- model_metrics(m_base)
quad_metrics <- model_metrics(m_quad)
log_metrics  <- model_metrics(m_log)

cat("\n--- BASE MODEL METRICS ---\n")
print(base_metrics)

cat("\n--- QUADRATIC MODEL METRICS ---\n")
print(quad_metrics)

cat("\n--- LOG TRANSFORMATION MODEL METRICS ---\n")
print(log_metrics)

## Show significance of nonlinear terms only
cat("\n=== Quadratic Terms Only ===\n")
print(summary(m_quad)$coefficients[grep("_sq", rownames(summary(m_quad)$coefficients)), ])

cat("\n=== Log Terms Only ===\n")
print(summary(m_log)$coefficients[grep("log_", rownames(summary(m_log)$coefficients)), ])



#########################################################################################################
## PREDICTOR vs RESPONSE PLOTS


library(tidyverse)

# 1. Numeric predictors: scatterplots with smoothing line
numeric_predictors <- c("bmi", "duration", "intensity", "age")

par(mfrow = c(2, 2))

for (var in numeric_predictors) {
  plot(selected_vars[[var]], selected_vars$heart_rate_spike,
       xlab = var,
       ylab = "heart_rate_spike",
       main = paste("heart_rate_spike vs", var),
       pch = 16, col = "darkblue")
  
  # Add LOWESS smoothing line
  lines(lowess(selected_vars[[var]], selected_vars$heart_rate_spike),
        col = "red", lwd = 2)
}

par(mfrow = c(1, 1))

####the output here is it displays the plot all at once making it hard to read them


# 2. Categorical predictors: boxplots
par(mfrow = c(1, 2))

boxplot(heart_rate_spike ~ gender, data = selected_vars,
        main = "heart_rate_spike by Gender",
        col = "lightblue", ylab = "heart_rate_spike")

boxplot(heart_rate_spike ~ weather, data = selected_vars,
        main = "heart_rate_spike by Weather",
        col = "lightgreen", ylab = "heart_rate_spike")

par(mfrow = c(1, 1))


###############################################################
#### here running the code, so that it displays each plot on its own page 
## making it easy to read

### Scatterplot: heart_rate_spike vs BMI############
ggplot(selected_vars, aes(x = bmi, y = heart_rate_spike)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", color = "red", se = TRUE, linewidth = 1) +
  labs(title = "BMI vs Heart Rate Spike",
       x = "BMI", y = "Heart Rate Spike") +
  theme_minimal()



######Duration vs heart_rate_spike#####
ggplot(selected_vars, aes(x = duration, y = heart_rate_spike)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(title = "Duration vs Heart Rate Spike",
       x = "Duration (minutes)", y = "Heart Rate Spike") +
  theme_minimal()

######Intensity vs heart_rate_spike#####
ggplot(selected_vars, aes(x = intensity, y = heart_rate_spike)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(title = "Intensity vs Heart Rate Spike",
       x = "Intensity (1–10)", y = "Heart Rate Spike") +
  theme_minimal()

######Age vs heart_rate_spike #####
ggplot(selected_vars, aes(x = age, y = heart_rate_spike)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(title = "Age vs Heart Rate Spike",
       x = "Age (years)", y = "Heart Rate Spike") +
  theme_minimal()

##########################################################
#########Categorical Predictors in ggplot (much nicer than base R)
###Gender

ggplot(selected_vars, aes(x = gender, y = heart_rate_spike, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#66b3ff", "#99e699")) +
  labs(title = "Heart Rate Spike by Gender",
       x = "Gender", y = "Heart Rate Spike") +
  theme_minimal()


#####Weather #####################
ggplot(selected_vars, aes(x = weather, y = heart_rate_spike, fill = weather)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Heart Rate Spike by Weather Condition",
       x = "Weather", y = "Heart Rate Spike") +
  theme_minimal()


#####################################################################
## DIAGNOSTIC PLOTS FOR FINAL MODEL

par(mfrow = c(2, 2))  # 4 plots in one window
plot(m_base)          # final model is m_base
par(mfrow = c(1, 1))


###Beautiful Diagnostic Plots Using ggfortify 
install.packages("ggfortify")
library(ggfortify)

autoplot(m_base, label.size = 3) +
  theme_minimal(base_size = 12)

###Residuals vs Fitted
library(ggplot2)

ggplot(m_base, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()


#######Normal Q-Q Plot
ggplot(m_base, aes(sample = .stdresid)) +
  stat_qq(alpha = 0.4, color = "steelblue") +
  stat_qq_line(color = "red", linetype = 2) +
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_minimal()


#####Scale–Location Plot
ggplot(m_base, aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Scale-Location Plot",
       x = "Fitted Values",
       y = "√|Standardized Residuals|") +
  theme_minimal()

#########Residuals vs Leverage
ggplot(m_base, aes(.hat, .stdresid)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs Leverage",
       x = "Leverage",
       y = "Standardized Residuals") +
  theme_minimal()

######################################################################################################
### Model WITHOUT interaction terms
# VIF MODEL: No interaction terms


library(car)
library(flextable)
library(officer)   #  Needed for fp_border()
library(dplyr)

# VIF model (no interactions)
m_vif <- lm(
  heart_rate_spike ~ bmi + duration + intensity + age + gender + weather,
  data = selected_vars
)

vif_values <- car::vif(m_vif)

# Handle matrix output (caused by factors)
if (is.matrix(vif_values)) {
  vif_table <- data.frame(
    Predictor = rownames(vif_values),
    VIF       = round(vif_values[, "GVIF^(1/(2*Df))"], 3)
  )
} else {
  vif_table <- data.frame(
    Predictor = names(vif_values),
    VIF       = round(as.numeric(vif_values), 3)
  )
}

# Border style for header and bottom lines
std_border <- fp_border(color = "black", width = 1)

# Build flextable
ft_vif <- flextable(vif_table) |>
  autofit() |>
  fontsize(size = 11, part = "all") |>
  align(align = "center", part = "all") |>
  bold(part = "header") |>
  border_remove() |> 
  hline_top(part = "header", border = std_border) |>
  hline_bottom(part = "header", border = std_border) |>
  hline_bottom(part = "body", border = std_border)

ft_vif


###############################graphical representation ##########################
library(ggplot2)


vif_table$Predictor <- factor(
  vif_table$Predictor,
  levels = c("age", "bmi", "weather", "duration", "gender", "intensity")
)

# Bar chart of VIFs
ggplot(vif_table, aes(x = Predictor, y = VIF)) +
  geom_col(fill = "#0096FF") + 
  geom_text(aes(label = VIF),
            vjust = -0.5,
            size = 3.5) +
  geom_hline(yintercept = 5, linetype = "dashed", colour = "red", linewidth = 0.8) +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictor Variables",
    y = "VIF Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



