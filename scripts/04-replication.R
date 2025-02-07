#### Preamble ####

# Purpose: This script is designed to replicate the findings of the original study
# on the impact of financial interventions on microenterprise performance, considering gender differences. 
# The replication process involves regression analyses to assess the effects of loans, 
# cash grants, and in-kind grants on key business outcomes.

# Author: Yiming Tang
# Date: 10 Feb 2024
# Contact: ym.tang@utoronto.ca
# License: MIT
# Pre-requisites: R environment with packages 'readr',  'ggplot2', `dplyr`, 'dplyr', 'knitr', 
# 'kableExtra', and 'stargazer' installed.

#### Workspace setup ####
library(readr)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)
library(stargazer)

#### Replication ####

# load dataset
data <- read_csv("inputs/cleaned_data.csv")

# define a function to perform the regression for the treatment and a outcome variable
# This function do the linear regression, save the regression plot in the specified file path, and prepare to draw the summary table
regress <- function(data, outcome, file_path, var2name) {
  name <- var2name[[outcome]]
  # regression
  fit <- lm(reformulate(c("micro_credit", "in_kind_grant", "cash_grant"), response = outcome), data = data)
  
  coefs <- summary(fit)$coefficients
  confint <- confint(fit)
  
  # regression plot
  plot_data <- data.frame(
    Treatment = c("Micro Credit", "In-Kind Grant", "Cash Grant"),
    Estimate = coefs[2:4, 1],
    Lower = confint[2:4, 1],
    Upper = confint[2:4, 2]
  )
  
  regression_plot <- ggplot(plot_data, aes(x = Treatment, y = Estimate)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
    labs(y = name, title = paste("Impact of Treatment on", name)) +
    theme_minimal()
  
  full_file_path <- paste0(file_path, name, ".jpg")
  ggsave(full_file_path, plot = regression_plot, width = 8, height = 6, dpi = 300)
  
  return(fit)
}

# replicate results for business outcome estimands
var2name <- c(
  hasbiz = "Has Business",
  new_biz_assets = "New Asset",
  monthly_expanses = "Monthly Expenditure",
  monthly_revenue = "Monthly Revenue",
  monthly_profits = "Monthly Profit"
)

models <- lapply(names(var2name), function(var) {
  regress(data, var, "outputs/figures/", var2name)
})

stargazer(models, type = "latex", out = "outputs/tables/business_outcome.tex",
          title = "Business Outcome",
          covariate.labels = c("Micro Credit", "In-Kind Grant", "Cash Grant"),
          column.labels = var2name,
          add.lines = list(c("N", paste(sapply(models, function(x) sum(!is.na(x$model[[1]])))), collapse = " ")),
          single.row = TRUE, font.size = "small", digits = 2,
          no.space = TRUE)

# replicate results for business outcome by gender
female_data <- subset(data, is_male == 0)
male_data <- subset(data,  is_male == 1)

var2name <- c(
  hasbiz = "Male - Has Business",
  new_biz_assets = "Male - New Asset",
  monthly_expanses = "Male - Monthly Expenditure",
  monthly_revenue = "Male - Monthly Revenue",
  monthly_profits = "Male - Monthly Profit"
)

male_models <- lapply(names(var2name), function(var) {
  regress(male_data, var, "outputs/figures/male", var2name)
})

var2name <- c(
  hasbiz = "Female - Has Business",
  new_biz_assets = "Female - New Asset",
  monthly_expanses = "Female - Monthly Expenditure",
  monthly_revenue = "Female - Monthly Revenue",
  monthly_profits = "Female - Monthly Profit"
)

female_models <- lapply(names(var2name), function(var) {
  regress(female_data, var, "outputs/figures/female", var2name)
})


stargazer(female_models, type = "latex", out = "outputs/tables/business_outcome_female.tex",
          title = "Business Outcome - Female Participants",
          covariate.labels = c("Micro Credit", "In-Kind Grant", "Cash Grant"),
          column.labels = var2name,
          add.lines = list(c("N", paste(sapply(models, function(x) sum(!is.na(x$model[[1]])))), collapse = " ")),
          single.row = TRUE, font.size = "small", digits = 2,
          no.space = TRUE)


stargazer(male_models, type = "latex", out = "outputs/tables/business_outcome_male.tex",
          title = "Business Outcome - Male Participants",
          covariate.labels = c("Micro Credit", "In-Kind Grant", "Cash Grant"),
          column.labels = var2name,
          add.lines = list(c("N", paste(sapply(models, function(x) sum(!is.na(x$model[[1]])))), collapse = " ")),
          single.row = TRUE, font.size = "small", digits = 2,
          no.space = TRUE)

# replicate results for income outcome estimands
var2name <- c(
  labour_inc = "Labour Income",
  total_inc = "Total Income"
)

models <- lapply(names(var2name), function(var) {
  regress(data, var, "outputs/figures/", var2name)
})

stargazer(models, type = "latex", out = "outputs/tables/income_outcome.tex",
          title = "Income Outcome",
          covariate.labels = c("Micro Credit", "In-Kind Grant", "Cash Grant"),
          column.labels = var2name,
          add.lines = list(c("N", paste(sapply(models, function(x) sum(!is.na(x$model[[1]])))), collapse = " ")),
          single.row = TRUE, font.size = "small", digits = 2,
          no.space = TRUE)

var2name <- c(
  labour_inc = "Male - Labour Income",
  total_inc = "Male - Total Income"
)

male_models <- lapply(names(var2name), function(var) {
  regress(male_data, var, "outputs/figures/male", var2name)
})

var2name <- c(
  labour_inc = "Female - Labour Income",
  total_inc = "Female - Total Income"
)

female_models <- lapply(names(var2name), function(var) {
  regress(female_data, var, "outputs/figures/female", var2name)
})


stargazer(female_models, type = "latex", out = "outputs/tables/income_outcome_female.tex",
          title = "Income Outcome - Female Participants",
          covariate.labels = c("Micro Credit", "In-Kind Grant", "Cash Grant"),
          column.labels = var2name,
          add.lines = list(c("N", paste(sapply(models, function(x) sum(!is.na(x$model[[1]])))), collapse = " ")),
          single.row = TRUE, font.size = "small", digits = 2,
          no.space = TRUE)


stargazer(male_models, type = "latex", out = "outputs/tables/income_outcome_male.tex",
          title = "Income Outcome - Male Participants",
          covariate.labels = c("Micro Credit", "In-Kind Grant", "Cash Grant"),
          column.labels = var2name,
          add.lines = list(c("N", paste(sapply(models, function(x) sum(!is.na(x$model[[1]])))), collapse = " ")),
          single.row = TRUE, font.size = "small", digits = 2,
          no.space = TRUE)


# replicate balance check
data$treatment_status <- factor(data$treatment_status)
varlist <- c("is_male","educ_college","educ_somecollege","educ_hs",
                           "age","marital_m","worked","has_biz",
                           "family_inc_low","migrate","trained","has_kids")

varname <- c(
  "is_male" = "Male",
  "educ_college" = "College Education",
  "educ_somecollege" = "Some College Education",
  "educ_hs" = "High School Education",
  "age" = "Age",
  "marital_m" = "Married",
  "worked" = "Worked Before",
  "has_biz" = "Has a Business",
  "family_inc_low" = "Low Family Income",
  "migrate" = "Migration Desire",
  "trained" = "Received Training",
  "has_kids" = "Has Kids"
)

models <- lapply(varlist, function(var) {
  lm(reformulate("treatment_status", response = var), data = data)
})

stargazer(models, type = "latex", title = "Baseline Balance",
          font.size = "small", digits = 2,
          no.space = TRUE, single.row = TRUE,
          out = "outputs/tables/baseline_balance.tex",
          add.names = TRUE,
          covariate.labels = c("Control", "Microcredit", "In-Kind Grant", "Cash Grant"),
          column.labels = varname,
          omit.stat = c("f")) 

