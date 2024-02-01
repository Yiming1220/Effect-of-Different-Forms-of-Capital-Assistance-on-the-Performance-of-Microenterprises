library(readr)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

# define a function to perform the regression for the treatment and a outcome variable
# This function do the linear regression, save the regression plot in the specified file path, and prepare to draw the summary table
regress <- function(data, outcome, file_path, var2name) {
  name <- var2name[[outcome]]
  fit <- lm(reformulate(c("micro_credit", "in_kind_grant", "cash_grant"), response = outcome), data = data)
  
  coefs <- summary(fit)$coefficients
  confint <- confint(fit)
  
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
  
  formatted_coefs <- paste0(
    round(coefs[2:4, "Estimate"], 2), 
    " (", 
    round(coefs[2:4, "Std. Error"], 2), 
    ")"
  )
  
  N <- sum(!is.na(data[[outcome]]))
  
  summary_df <- data.frame(Values = c(formatted_coefs, N))
  names(summary_df) <- name

  return(summary_df)
}

var2name <- c(
  hasbiz = "Has Business",
  new_biz_assets = "New Asset",
  monthly_expanses = "Monthly Expenditure",
  monthly_revenue = "Monthly Revenue",
  monthly_profits = "Monthly Profit"
)

data <- read_csv("inputs/data/cleaned_data.csv")
outcome_vars <- names(var2name)
combined_summary <- list()

for (var in outcome_vars) {
  summary_df <- regress(data, var, "outputs/figures/", var2name)
  combined_summary[[var]] <- summary_df
}

combined_summary_df <- do.call(cbind, combined_summary)
row.names(combined_summary_df) <- c("Micro Credit", "In-Kind Grant", "Cash Grant", "N")

html_table <- kable(combined_summary_df, "html", booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

html_file_path <- "outputs/tables/summary_table.html"
save_kable(html_table, file = html_file_path)

