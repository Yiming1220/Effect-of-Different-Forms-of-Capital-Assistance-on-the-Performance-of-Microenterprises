#### Preamble ####
# Purpose: Simulates the orignal research data
# Author: Yiming Tang
# Date: 10 Feb 2024
# Contact: ym.tang@utoronto.ca
# License: MIT
# Pre-requisites: None


#### Workspace setup ####

#### Simulate data ####
set.seed(123)

n <- 3075 

# Simulating treatment variables
# 1: control, 2: micro credit 3: in kind credit 4: cash grant
treatment_status <- sample(1:4, n, replace=TRUE, prob=c(0.25, 0.25, 0.25, 0.25))
control <- ifelse(treatment_status == 1, 1, 0)
micro_credit <- ifelse(treatment_status == 2, 1, 0)
in_kind_grant <- ifelse(treatment_status == 3, 1, 0)
cash_grant <- ifelse(treatment_status == 4, 1, 0)

# Simulating binary variables
hasbiz <- rbinom(n, size=1, prob=0.5)
is_male <- rbinom(n, size=1, prob=0.5)
worked <- rbinom(n, size=1, prob=0.6)
has_biz <- rbinom(n, size=1, prob=0.5)
family_inc_low <- rbinom(n, size=1, prob=0.4)
trained <- rbinom(n, size=1, prob=0.3)

# Simulating continuous variables
new_biz_assets <- rnorm(n, mean=50000, sd=10000)
monthly_expanses <- runif(n, min=0, max=1000)
monthly_revenue <- runif(n, min=0, max=2000)
monthly_profits <- monthly_revenue - monthly_expanses 
labour_inc <- rnorm(n, mean=200, sd=50)
total_inc <- labour_inc + monthly_profits 
age <- rnorm(n, mean=35, sd=10)
migrate <- rbinom(n, size=1, prob=0.2)


# Simulating categorical variables with multiple levels
# 0: No college, 1: Some college, 2: College
educ_levels <- c(0, 1, 2)  
educ_college <- sample(educ_levels, n, replace=TRUE, prob=c(0.5, 0.3, 0.2))
educ_somecollege <- ifelse(educ_college == 1, 1, 0)
educ_hs <- ifelse(educ_college == 0, 1, 0)

marital_status <- c(0, 1)  # 0: Single, 1: Married
marital_s <- sample(marital_status, n, replace=TRUE, prob=c(0.4, 0.6))
marital_m <- ifelse(marital_s == 0, 1, 0)

# generate simulated data set
simulated_dataset <- data.frame(treatment_status, control, micro_credit, 
                                in_kind_grant, cash_grant, hasbiz, is_male, worked, 
                                has_biz, family_inc_low, trained, new_biz_assets, 
                                monthly_expanses, monthly_revenue, monthly_profits, 
                                labour_inc, total_inc, age, migrate, educ_college, 
                                educ_somecollege, educ_hs, marital_s, marital_m)
write.csv(simulated_dataset, file = "inputs/data/simulated.csv", row.names = FALSE)


                               