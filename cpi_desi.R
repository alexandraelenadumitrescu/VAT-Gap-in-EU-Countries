# ==============================================================================
# SCRIPT FINAL - Testare CPI si DESI
# ==============================================================================

filename <- "cpi_desi.csv"
if (!file.exists(filename)) stop("Nu gasesc fisierul 'eu_comm_egov.csv'!")

df <- read.csv(filename, stringsAsFactors = FALSE)

# Lista variabilelor "grele"
vars_to_test <- c("cpi_2022", "desi_2022")

for (var_name in vars_to_test) {
  
  cat(paste0("\n----------------------------------------------------------\n"))
  cat(paste0(" MODEL: VAT Gap ~ Shadow Economy + ", var_name, "\n"))
  cat(paste0("----------------------------------------------------------\n"))
  
  formula_reg <- as.formula(paste("vat_gap ~ shadow_2022 +", var_name))
  model <- lm(formula_reg, data = df)
  summ <- summary(model)
  
  # Extragere date
  pval_shadow <- summ$coefficients["shadow_2022", "Pr(>|t|)"]
  pval_target <- summ$coefficients[var_name, "Pr(>|t|)"]
  coef_target <- summ$coefficients[var_name, "Estimate"]
  r_sq        <- summ$adj.r.squared
  
  # VIF
  cor_val <- cor(df$shadow_2022, df[[var_name]], use="complete.obs")
  vif <- 1 / (1 - cor_val^2)
  
  # Afisare
  cat("R-Squared (Putere Explicativa): ", round(r_sq * 100, 2), "%\n")
  cat("VIF (Risc coliniaritate):       ", round(vif, 3), "\n\n")
  
  cat("REZULTATE SEMNIFICATIE (Tinta p < 0.05):\n")
  cat("   Shadow Economy: ", round(pval_shadow, 5), "\n")
  cat(paste0("   ", var_name, ":        ", round(pval_target, 5)))
  
  if(pval_target < 0.05) {
    cat("  [ !!! SEMNIFICATIV !!! ]\n")
  } else {
    cat("  [ Nesemnificativ ]\n")
  }
  
  cat(paste0("\n   Coeficient: ", round(coef_target, 4)))
  if(var_name == "cpi_2022" && coef_target < 0) cat(" (Negativ = Corect: Coruptie mica -> Gap mic)")
  if(var_name == "desi_2022" && coef_target < 0) cat(" (Negativ = Corect: Digitalizare mare -> Gap mic)")
  cat("\n")
}