# ==============================================================================
# SCRIPT 5: Tax Administration Demographics (Aging Workforce)
# ==============================================================================

filename <- "aged.csv"
if (!file.exists(filename)) stop("Please create 'eu_staff_aging.csv' first!")

df <- read.csv(filename, stringsAsFactors = FALSE)
var_name <- "staff_aging_change_18_23"

cat("\n--- ANALYSIS: Workforce Demographics (Aging Staff Change) ---\n")
cat("Hypothesis: Rapidly aging workforce -> Slower digital adoption -> Higher VAT Gap (+)\n")

run_model <- function(data_sub, label) {
  cat(paste0("\nSCENARIO: ", label, "\n"))
  cat("----------------------------------------------------------\n")
  
  form <- as.formula(paste("vat_gap ~ shadow_2022 +", var_name))
  model <- lm(form, data = data_sub)
  summ <- summary(model)
  
  pval_age <- summ$coefficients[var_name, "Pr(>|t|)"]
  coef_age <- summ$coefficients[var_name, "Estimate"]
  r_sq <- summ$adj.r.squared
  
  cat("R-Squared:           ", round(r_sq * 100, 2), "%\n")
  cat("Significance Shadow: ", round(summ$coefficients["shadow_2022", "Pr(>|t|)"], 5), "\n")
  cat("Significance Aging:  ", round(pval_age, 5))
  
  if(pval_age < 0.05) {
    cat(" [ SIGNIFICANT ]\n")
  } else if(pval_age < 0.10) {
    cat(" [ Marginal ]\n")
  } else {
    cat(" [ Not Significant ]\n")
  }
  
  cat("Coefficient Aging:   ", round(coef_age, 4), "\n")
  if(coef_age > 0) cat("-> Positive Relationship (Aging workforce correlates with HIGHER VAT Gap).\n")
  else cat("-> Negative Relationship (Aging workforce correlates with LOWER VAT Gap - Experience effect?).\n")
}

# RUN
# 1. All countries
run_model(df, "All EU-27 Countries")

# 2. Without Outliers (Romania has a huge aging rate + huge Gap, might drive the whole model)
# Let's see if the trend holds without Romania (RO) and Slovenia (SI) who are at the top.
outliers <- c("RO", "SI")
df_clean <- df[!df$country_code %in% outliers, ]
run_model(df_clean, "Without Extreme Aging Cases (RO, SI)")