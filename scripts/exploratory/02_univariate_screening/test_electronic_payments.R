# ==============================================================================
# SCRIPT FINAL: Platile Electronice (Internet Banking)
# ==============================================================================

filename <- "../../../data/raw/electronic_payments_card_usage.csv"

if (!file.exists(filename)) {
  stop("Te rog creeaza fisierul 'eu_payments.csv' cu datele de mai sus!")
}

df <- read.csv(filename, stringsAsFactors = FALSE)

# 1. PREGATIRE DATE
var_name <- "internet_banking_2023"

# 2. DEFINIRE FUNCTIE DE ANALIZA
run_analysis <- function(data_subset, title) {
  cat(paste0("\n==========================================================\n"))
  cat(paste0(" SCENARIU: ", title, "\n"))
  cat(paste0("==========================================================\n"))
  
  # Creare model: VAT Gap ~ Shadow Economy + Internet Banking
  formula_str <- paste("vat_gap ~ shadow_2022 +", var_name)
  model <- lm(as.formula(formula_str), data = data_subset)
  summ <- summary(model)
  
  # Extragere valori cheie
  r_sq <- summ$adj.r.squared
  pval_shadow  <- summ$coefficients["shadow_2022", "Pr(>|t|)"]
  pval_digital <- summ$coefficients[var_name, "Pr(>|t|)"]
  coef_digital <- summ$coefficients[var_name, "Estimate"]
  
  # Calcul VIF (Multicoliniaritate)
  cor_val <- cor(data_subset$shadow_2022, data_subset[[var_name]])
  vif <- 1 / (1 - cor_val^2)
  
  # AFISARE REZULTATE
  cat("1. PUTERE EXPLICATIVA (R-sq): ", round(r_sq * 100, 2), "%\n")
  cat("2. MULTICOLINIARITATE (VIF):  ", round(vif, 3))
  if(vif < 5) cat(" (OK - Distinct)\n") else cat(" (RIDICAT)\n")
  
  cat("3. SEMNIFICATIE STATISTICA (p < 0.05):\n")
  cat(paste0("   Shadow Economy:   ", round(pval_shadow, 5), "\n"))
  cat(paste0("   Internet Banking: ", round(pval_digital, 5)))
  
  if(pval_digital < 0.05) {
    cat(" [ !!! SEMNIFICATIV !!! ]\n")
  } else if(pval_digital < 0.10) {
    cat(" [ Marginal Semnificativ ]\n")
  } else {
    cat(" [ Nesemnificativ ]\n")
  }
  
  cat(paste0("4. COEFICIENT: ", round(coef_digital, 4)))
  if(coef_digital < 0) cat(" (Negativ -> Corect: Mai multe plati online = Mai putina evaziune)\n")
  else cat(" (Pozitiv -> Neasteptat)\n")
}

# ------------------------------------------------------------------------------
# RULARE SCENARII
# ------------------------------------------------------------------------------

# SCENARIUL A: Toate Tarile
run_analysis(df, "Toate tarile UE-27")

# SCENARIUL B: Fara Outlieri (Fara Malta si Romania)
# Nota: La plati electronice, Romania s-ar putea sa ajute modelul, dar Malta il strica.
# Sa vedem ce se intampla daca scoatem doar Malta (economie insulara atipica).
df_no_mt <- df[df$country_code != "MT", ]
run_analysis(df_no_mt, "Fara Malta (Outlier structural)")

# SCENARIUL C: "Chirurgia Completa" (Fara RO, MT, CY)
outliers <- c("RO", "MT", "CY")
df_clean <- df[!df$country_code %in% outliers, ]
run_analysis(df_clean, "Fara RO, MT, CY (Trendul Central European)")