#### Preamble ####
# Purpose: To validate the cleaned dataset for integrity of treatment status, 
# correctness of binary variables, and adherence of continuous variables to specified ranges.
# Author: Yiming Tang
# Date: 10 Feb 2024
# Contact: ym.tang@mail.utoronto.ca
# License: MIT
# Pre-requisites: R environment with the 'readr' package installed.

#### Workspace setup ####
library(readr)

#### Test Data ####
data <- read_csv("inputs/cleaned_data.csv") 

# define.a funciton to test if treatment status all in 0, 1, 2, and 3
test_treatment_status_values <- function(data) {
  unique_values <- unique(data$treatment_status)
  if (all(unique_values %in% c(1, 2, 3, 4)) && length(unique_values) == 4) {
      return("Treatment status contains only values 1, 2, 3, and 4.")
    } else {
      return("Treatment status contains values outside of 1, 2, 3, and 4.")
    }
  }

# definea a function to test if the data is binary
test_binary <- function(data, column) {
  if (all(data[[column]] %in% c(0, 1), na.rm = TRUE)) {
    return(paste(column, "is binary and valid."))
  } else {
    return(paste(column, "contains non-binary values."))
  }
}

# define a function to test if the continuous variable within range
test_continuous_range <- function(data, column, min_val, max_val) {
  if (all(data[[column]] >= min_val & data[[column]] <= max_val, na.rm = TRUE)) {
    return(paste(column, "values are within the specified range (", min_val, "to", max_val, ")."))
  } else {
    return(paste(column, "contains values outside the specified range (", min_val, "to", max_val, ")."))
  }
}

binary_columns <- c('control', 'micro_credit', 'in_kind_grant', 'cash_grant', 'hasbiz', 
                    'is_male', 'worked', 'has_biz', 'family_inc_low', 'trained')

continuous_columns <- list(
  new_biz_assets = c(0, 100000),
  monthly_expanses = c(0, 50000),
  monthly_revenue = c(0, 100000),
  monthly_profits = c(-10000, 50000),
  age = c(18, 100)
)

# Testing treatment variables
treatment_result <- test_treatment_status_values(data)

# Testing binary variables
binary_results <- sapply(binary_columns, function(col) test_binary(data, col))

# Testing continuous variables
continuous_results <- sapply(names(continuous_columns), function(col) {
  test_continuous_range(data, col, continuous_columns[[col]][1], continuous_columns[[col]][2])
})

# Combining results
test_results <- c(treatment_result, binary_results, continuous_results)

# Print results
print(test_results)


