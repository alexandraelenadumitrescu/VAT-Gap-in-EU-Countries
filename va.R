# ==============================================================================
# SCRIPT 4: Political Regime & Voluntary Compliance (Voice & Accountability)
# ==============================================================================

filename <- "va.csv"
if (!file.exists(filename)) stop("Creeaza fisierul 'eu_politics.csv'!")

df <- read.csv(filename, stringsAsFactors = FALSE)
var_name <- "voice_accountability_2022"

cat("\n--- ANALIZA: Democracy & Compliance (WGI - Voice and Accountability) ---\n")
cat("Ipoteza: Democratia ridicata creste conformarea voluntara (Gap mic).\n")

run_model <- function(data_sub, label) {
  cat(paste0("\nSCENARIU: ", label, "\n"))
  cat("----------------------------------------------------------\n")
  
  form <- as.formula(paste("vat_gap ~ shadow_2022 +", var_name))
  model <- lm(form, data = data_sub)
  summ <- summary(model)
  
  pval_pol <- summ$coefficients[var_name, "Pr(>|t|)"]
  coef_pol <- summ$coefficients[var_name, "Estimate"]
  r_sq <- summ$adj.r.squared
  
  cat("R-Squared:           ", round(r_sq * 100, 2), "%\n")
  cat("Semnificatie Shadow: ", round(summ$coefficients["shadow_2022", "Pr(>|t|)"], 5), "\n")
  cat("Semnificatie VA:     ", round(pval_pol, 5))
  
  if(pval_pol < 0.05) {
    cat(" [ SEMNIFICATIV ]\n")
  } else {
    cat(" [ Nesemnificativ ]\n")
  }
  
  cat("Coeficient VA:       ", round(coef_pol, 4), "\n")
  if(coef_pol < 0) cat("-> Relatie negativa (Mai multa democratie = Gap mai mic).\n")
}

# RULARE
# 1. Toate tarile
run_model(df, "Toate tarile")

# 2. Fara Outlieri (MT) - Malta are democratie ok, dar Gap mare (atipic)
run_model(df[df$country_code != "MT", ], "Fara Malta")

# 3. Comparatie cu Enforcement (Discutie)
cat("\n--- CONCLUZIE RAPIDA ---\n")
cat("Daca 'Voice and Accountability' (Democratia) este mai putin semnificativa decat \n")
cat("'Government Effectiveness' (Capacitatea Administrativa - testata anterior),\n")
cat("inseamna ca pentru TVA conteaza mai mult FORTA statului decat LIBERTATEA cetatenilor.\n")