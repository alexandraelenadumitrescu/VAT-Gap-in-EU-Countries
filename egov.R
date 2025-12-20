# ==============================================================================
# SCRIPT ANALIZA VAT GAP - Citire din fisier extern
# ==============================================================================

# 1. CITIREA DATELOR
# ------------------
# Citim direct din fisierul tau. Asigura-te ca fisierul este in Working Directory.
filename <- "eu_comm_egov.csv"

if (!file.exists(filename)) {
  stop("EROARE: Nu gasesc fisierul 'eu_comm_egov.csv'. Verifica daca este in folderul corect.")
}

df <- read.csv(filename, stringsAsFactors = FALSE)

# 2. CURATAREA DATELOR
# --------------------
# Eliminam semnul "%" si convertim in numere pentru VAT Gap si Shadow Economy
df$vat_gap <- as.numeric(gsub("%", "", df$vat_gap))
df$shadow_2022 <- as.numeric(gsub("%", "", df$shadow_2022))

# Verificam daca datele s-au incarcat corect
cat("Date incarcate cu succes: ", nrow(df), "tari.\n")
print(head(df))

# 3. DEFINIREA VARIABILELOR DE TESTAT
# -----------------------------------
# Lista indicatorilor digitali pe care vrei sa ii testezi pe rand
digital_vars <- c("egdi_2022", "online_avail_A1", "transparency_D1", "eID_score")

# 4. BUCLA DE ANALIZA (REGRESIE + MULTICOLINIARITATE)
# ---------------------------------------------------

for (var_name in digital_vars) {
  
  # Verificam daca variabila exista in fisier (pentru siguranta)
  if (var_name %in% names(df)) {
    
    cat(paste0("\n\n##########################################################\n"))
    cat(paste0(" ANALIZA PENTRU: ", var_name, "\n"))
    cat(paste0("##########################################################\n"))
    
    # a) Construirea modelului: VAT ~ Shadow + Digital_Var
    formula_reg <- as.formula(paste("vat_gap ~ shadow_2022 +", var_name))
    model <- lm(formula_reg, data = df)
    summ <- summary(model)
    
    # b) Extragere coeficienti si P-values
    coefs <- summ$coefficients
    
    beta_shadow <- coefs["shadow_2022", "Estimate"]
    pval_shadow <- coefs["shadow_2022", "Pr(>|t|)"]
    
    beta_digital <- coefs[var_name, "Estimate"]
    pval_digital <- coefs[var_name, "Pr(>|t|)"]
    
    r_sq <- summ$adj.r.squared
    
    # c) Calcul VIF (Variance Inflation Factor) pentru a testa Multicoliniaritatea
    # VIF = 1 / (1 - r^2_intre_predictori)
    # Calculam corelatia doar pe randurile complete (fara NA, ex: Cipru la EGDI)
    cor_val <- cor(df$shadow_2022, df[[var_name]], use = "complete.obs")
    vif <- 1 / (1 - cor_val^2)
    
    # d) AFISARE REZULTATE INTERPRETATE
    
    cat("1. PERFORMANTA MODELULUI (R-squared ajustat):\n")
    cat(paste0("   ", round(r_sq * 100, 2), "% din variatia VAT Gap este explicata de acest model.\n\n"))
    
    cat("2. TESTARE MULTICOLINIARITATE (Relatia Shadow <-> Digital):\n")
    cat(paste0("   Corelatie: ", round(cor_val, 3), "\n"))
    cat(paste0("   VIF:       ", round(vif, 3)))
    if (vif < 5) {
      cat(" -> OK. Variabilele sunt suficient de distincte.\n\n")
    } else {
      cat(" -> ATENTIE! Multicoliniaritate ridicata. Rezultatele pot fi distorsionate.\n\n")
    }
    
    cat("3. SEMNIFICATIE STATISTICA (P-Value < 0.05 este tinta):\n")
    cat(paste0("   Shadow Economy: ", round(pval_shadow, 5), 
               ifelse(pval_shadow < 0.05, " (Semnificativ)", " (Nesemnificativ)"), "\n"))
    cat(paste0("   ", var_name, ":      ", round(pval_digital, 5), 
               ifelse(pval_digital < 0.05, " (Semnificativ)", " (Nesemnificativ)"), "\n"))
    
    cat("\n4. DIRECTIA LEGATURII (Coeficient):\n")
    cat(paste0("   Coeficient Digital: ", round(beta_digital, 4), "\n"))
    if (beta_digital < 0) {
      cat("   CONCLUZIE: Cresterea digitalizarii REDUCE evaziunea (confirmare teoretica).\n")
    } else {
      cat("   CONCLUZIE: Relatie pozitiva sau neclara (contrazice ipoteza).\n")
    }
    
  } else {
    cat(paste0("\n[!] Atentie: Variabila ", var_name, " nu a fost gasita in fisierul CSV.\n"))
  }
}