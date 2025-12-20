# ==============================================================================
# SCRIPT 3: Tax Administration Enforcement (Capacitate Administrativa)
# ==============================================================================

filename <- "wgi.csv"
if (!file.exists(filename)) stop("Creeaza fisierul 'eu_enforcement.csv'!")

df <- read.csv(filename, stringsAsFactors = FALSE)

# 1. CURATARE DATE
# Daca ai procente in fisier, decomenteaza liniile de mai jos:
# df$vat_gap <- as.numeric(gsub("%", "", df$vat_gap))
# df$shadow_2022 <- as.numeric(gsub("%", "", df$shadow_2022))

var_name <- "enforcement_wgi_2022"

cat(paste0("\n--- ANALIZA: Enforcement Efficiency (WGI Proxy) ---\n"))

# 2. DEFINIRE SCENARII
run_model <- function(data_sub, label) {
  cat(paste0("\nSCENARIU: ", label, "\n"))
  cat("----------------------------------------------------------\n")
  
  # Model: Gap = Shadow + Enforcement
  form <- as.formula(paste("vat_gap ~ shadow_2022 +", var_name))
  model <- lm(form, data = data_sub)
  summ <- summary(model)
  
  # Extragere P-values si Coeficienti
  p_shadow <- summ$coefficients["shadow_2022", "Pr(>|t|)"]
  p_enforce <- summ$coefficients[var_name, "Pr(>|t|)"]
  coef_enforce <- summ$coefficients[var_name, "Estimate"]
  r_sq <- summ$adj.r.squared
  
  # Verificare VIF
  cor_val <- cor(data_sub$shadow_2022, data_sub[[var_name]])
  vif <- 1 / (1 - cor_val^2)
  
  cat("R-Squared:        ", round(r_sq * 100, 2), "%\n")
  cat("VIF (Coliniaritate): ", round(vif, 3), "\n")
  
  cat("Semnificatie Shadow: ", round(p_shadow, 5), "\n")
  cat("Semnificatie Enforcement: ", round(p_enforce, 5))
  
  if(p_enforce < 0.05) {
    cat(" [ !!! SEMNIFICATIV !!! ]\n")
  } else if(p_enforce < 0.10) {
    cat(" [ Marginal ]\n")
  } else {
    cat(" [ Nesemnificativ ]\n")
  }
  
  cat("Coeficient Enforcement: ", round(coef_enforce, 4))
  if(coef_enforce < 0) cat(" (Negativ -> Corect: Stat mai eficient = Gap mai mic)\n")
  else cat(" (Pozitiv -> Contraintuitiv)\n")
}

# 3. RULARE
# A. Toate tarile
run_model(df, "Toate tarile UE-27")

# B. Fara Outlierii "Clasici" (MT - Malta distorsioneaza des)
# Romania (RO) este un caz interesant aici: are WGI foarte mic si Gap mare. 
# S-ar putea sa FIE EXACT exemplul care valideaza teoria, deci nu o eliminam prima data.
df_no_mt <- df[df$country_code != "MT", ]
run_model(df_no_mt, "Fara Malta (Outlier structural)")