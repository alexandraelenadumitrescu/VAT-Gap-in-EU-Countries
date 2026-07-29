# ==============================================================================
# SCRIPT ANALIZA VAT GAP - Date Noi din eGovernment Benchmark
# ==============================================================================

# 1. CITIREA DATELOR
filename <- "../../../data/raw/egovernment_development_index.csv"

if (!file.exists(filename)) {
  stop("EROARE: Nu gasesc fisierul 'eu_comm_egov.csv'.")
}

df <- read.csv(filename, stringsAsFactors = FALSE)

# 2. CURATARE
df$vat_gap <- as.numeric(gsub("%", "", df$vat_gap))
df$shadow_2022 <- as.numeric(gsub("%", "", df$shadow_2022))

cat("Date incarcate cu succes pentru", nrow(df), "tari.\n")

# 3. LISTA NOILOR VARIABILE DIGITALE
# Acestea sunt coloanele extrase din imaginea noua
digital_vars <- c("online_avail_A1", 
                  "cross_border_B1",  # Foarte relevant pentru frauda carusel
                  "mobile_C1", 
                  "transparency_D1", 
                  "eID_score")

# 4. EXECUTIE ANALIZA

for (var_name in digital_vars) {
  
  if (var_name %in% names(df)) {
    cat(paste0("\n\n##########################################################\n"))
    cat(paste0(" MODEL: VAT Gap ~ Shadow Economy + ", var_name, "\n"))
    cat(paste0("##########################################################\n"))
    
    # Modelare
    formula_reg <- as.formula(paste("vat_gap ~ shadow_2022 +", var_name))
    model <- lm(formula_reg, data = df)
    summ <- summary(model)
    
    # Coeficienti
    beta_shadow  <- summ$coefficients["shadow_2022", "Estimate"]
    pval_shadow  <- summ$coefficients["shadow_2022", "Pr(>|t|)"]
    
    beta_digital <- summ$coefficients[var_name, "Estimate"]
    pval_digital <- summ$coefficients[var_name, "Pr(>|t|)"]
    
    # VIF
    cor_val <- cor(df$shadow_2022, df[[var_name]], use = "complete.obs")
    vif <- 1 / (1 - cor_val^2)
    
    # REZULTATE
    cat("1. PUTERE EXPLICATIVA (R-squared Adj): ", round(summ$adj.r.squared * 100, 2), "%\n")
    
    cat("2. VIF (Multicoliniaritate):           ", round(vif, 3))
    if(vif < 5) cat(" (OK - Distinct)\n") else cat(" (RIDICAT - Atentie)\n")
    
    cat("3. P-VALUES:\n")
    cat(paste0("   Shadow:  ", round(pval_shadow, 5), "\n"))
    cat(paste0("   ", var_name, ": ", round(pval_digital, 5), 
               ifelse(pval_digital < 0.05, " [SEMNIFICATIV!]", " [Nesemnificativ]"), "\n"))
    
    cat("4. DIRECTIE:\n")
    cat(paste0("   Coeficient: ", round(beta_digital, 4), "\n"))
    if(beta_digital < 0) cat("   [Confirmare] Semn negativ -> Digitalizarea reduce Gap-ul.\n")
    else cat("   [Contrazicere] Semn pozitiv -> Relatie neasteptata.\n")
    
  } else {
    cat(paste0("\n[!] Variabila ", var_name, " lipseste din CSV.\n"))
  }
}