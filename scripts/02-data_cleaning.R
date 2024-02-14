#### Preamble ####

# Purpose: Clean and prepare the dataset derived from the original study.
# The cleaning process includes loading datasets, merging pre- and post-intervention data, 
# filtering for consistency, handling missing values, and encoding categorical 

# Author: Yiming Tang
# Date: 10 Feb 2024
# Contact: ym.tang@mail.utoronto.ca
# License: MIT
# Pre-requisites: R epackages `haven`, `dplyr`, and `tidyverse` installed. 

#### Workspace setup ####
library(haven)
library(dplyr)


#### Define helper function ####

# replace NA in the list with val
fill_na <- function(var, val) {
  var[is.na(var)] <- val
  return(var)
}

#### Clean data ####

# load dataset
pre_data_path <- "inputs/AEJ_CKO_baseline.dta"
out_data_path <- "inputs/AEJ_CKO_endline.dta"
pre_data <- read_dta(pre_data_path)
out_data <- read_dta(out_data_path)
pre_data <- pre_data %>%
  filter(u_key %in% out_data$u_key)
out_data <- out_data %>%
  filter(u_key %in% pre_data$u_key)

# define new dataset to store cleaned data
new_data <- data.frame(matrix(NA, nrow = nrow(pre_data), ncol = 0))

# treat variable: treatment_status
# create dummy variable for each treatment status
pre_data$treatments <- factor(pre_data$treatment_status,
                                    levels = c(0, 1, 2, 3),
                                    labels = c("control", "micro_credit", "in_kind_grant", "cash_grant"))
dummies <- model.matrix(~ treatments - 1, data = pre_data)
treats <- as.data.frame(dummies)
colnames(treats) <- gsub("treatments", "", colnames(treats))
new_data$treatment_status <- pre_data$treatment_status
new_data <- cbind(new_data, treats)

# business outcome variable: has business
# an individual does not have business if they do not have project or  have no 
# income source
new_data$hasbiz <- rep(1, nrow(out_data))
new_data$hasbiz[!is.na(out_data$why_no_project)] <- 0
new_data$hasbiz[out_data$income_sources == ""] <- NA

# business outcome variable: new business assets
# new business assets are defined by the ssumation of the following assets
vars <- c("inventory_amount", "goods_amount", "cash_amount", "premise_new", 
          "land_new", "furniture_new", "equipment_machinery_new", 
          "vehicules_new", "amount_maintanance")
out_data <- out_data %>%
  mutate_at(vars, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))
new_data$new_biz_assets <- rowSums(out_data[vars], na.rm = TRUE)

# business outcome variable: monthly expanses, monthly revenues, monthly profits
# values are adjusted to its corrected format
# null values are replace with 0
new_data$monthly_expanses <- fill_na(out_data$monthly_expenses, 0)

new_data$monthly_revenue <- ifelse(!is.na(out_data$correct_sales), out_data$correct_sales, 
                          out_data$monthly_revenue)
new_data$monthly_revenue <- fill_na(out_data$monthly_revenue, 0)

new_data$monthly_profits <- ifelse(!is.na(out_data$correct_profits), out_data$correct_profits, 
                          out_data$monthly_profits)
new_data$monthly_profits <- fill_na(monthly_profits, 0)

# income variable: labour income, total income
# labour income is calculated by monthly profit, wage income, and other labour
# income source
# total income is calculated by monthly profit, wage income, 
wage_inc <- fill_na(out_data$wage_month, 0)
other_labour_inc <- ifelse(out_data$other_income_source %in% c(2, 4), out_data$other_income_source, NA)
other_labour_inc <- fill_na(other_labour_inc, 0)
rent_income <- fill_na(out_data$rent_asset, 0)
other_inc <- fill_na(out_data$other_income, 0)
gov_inc <- fill_na(out_data$received_govsupport_value, 0)
trans_inc <- fill_na(out_data$received_transfers_value, 0)
new_data$labour_inc <- out_data$monthly_profits + wage_inc + other_labour_inc
new_data$total_inc <- out_data$monthly_profits + rent_income + other_inc + gov_inc + trans_inc


# control variable
# gender, using female as reference
new_data$is_male <- ifelse(pre_data$gender_rand == 1, 1, 0)

# education categories
new_data$educ_college <- ifelse(pre_data$edu == 88, 1, 0)
new_data$educ_somecollege <- ifelse(pre_data$edu == 77, 1, 0)
new_data$educ_hs <- ifelse(pre_data$edu == 66, 1, 0)

# age
new_data$age <- 2017 - pre_data$birth_year

# marital status
new_data$marital_s <- ifelse(pre_data$marital == 1, 1, 0)
new_data$marital_m <- ifelse(pre_data$marital == 2, 1, 0)

# previous employment
new_data$worked <- ifelse(pre_data$worked_before %in% c(2, 99, NA), 0, 1)

# business ownership
new_data$has_biz <- ifelse(pre_data$emp_type %in% c("3", "4"), 1, 0)

# low family income
new_data$family_inc_low <- ifelse(pre_data$family_inc %in% c(1, 2), 1, 0)

# migration desire
new_data$migrate <- ifelse(pre_data$migrate_yesno == 1, 1, 0)

# training received
new_data$trained <- ifelse(pre_data$training_type != 0 & pre_data$treatment_status %in% c(1, 2, 3), 1, 0)

# has kids
new_data$has_kids <- ifelse(pre_data$child == 1, 1, 0)

# save data
write.csv(new_data, "inputs/cleaned_data.csv", row.names = FALSE)
