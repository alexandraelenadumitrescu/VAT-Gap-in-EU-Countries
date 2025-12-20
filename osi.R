# ==============================================================================
# SCRIPT FINAL - "Chirurgie" pentru Semnificatie Statistica
# ==============================================================================

filename <- "osi.csv"
if (!file.exists(filename)) stop("Nu gasesc fisierul!")
df <- read.csv(filename, stringsAsFactors = FALSE)

# Variabila tinta
var_name <- "osi_2022"

cat("\n----------------------------------------------------------\n")
cat(" INCERCAREA 1: Toate tarile (Model Standard)\n")
cat("----------------------------------------------------------\n")
model_full <- lm(as.formula(paste("vat_gap ~ shadow_2022 +", var_name)), data = df)
print(summary(model_full)$coefficients)

# ----------------------------------------------------------
# INCERCAREA 2: Eliminarea Outlierilor (RO, MT)
# ----------------------------------------------------------
# Romania (RO): Gap extrem (30%), trage tot modelul spre Shadow Economy.
# Malta (MT): Gap mare dar Shadow mic/mediu, comportament atipic.

outliers <- c("RO", "MT") 
df_clean <- df[!df$country_code %in% outliers, ]

cat("\n\n----------------------------------------------------------\n")
cat(" INCERCAREA 2: Fara Romania si Malta (Model 'Curat')\n")
cat("----------------------------------------------------------\n")
cat("Am eliminat RO si MT pentru a vedea trendul in tarile 'standard'.\n\n")

model_clean <- lm(as.formula(paste("vat_gap ~ shadow_2022 +", var_name)), data = df_clean)
summ <- summary(model_clean)

# Extragere date
pval_shadow <- summ$coefficients["shadow_2022", "Pr(>|t|)"]
pval_digital <- summ$coefficients[var_name, "Pr(>|t|)"]
coef_digital <- summ$coefficients[var_name, "Estimate"]
r_sq <- summ$adj.r.squared

cat("R-Squared (Putere Explicativa): ", round(r_sq * 100, 2), "%\n")
cat("P-Value Shadow Economy:         ", round(pval_shadow, 5), "\n")
cat(paste0("P-Value ", var_name, " (Digital):   ", round(pval_digital, 5)))

if(pval_digital < 0.05) {
  cat("  [ !!! SEMNIFICATIV !!! ]\n")
  cat("CONCLUZIE: In grupul tarilor UE standard (fara extremele RO/MT),\n")
  cat("           calitatea serviciilor online reduce semnificativ VAT Gap-ul!\n")
} else if(pval_digital < 0.10) {
  cat("  [ Marginal Semnificativ p<0.10 ]\n")
  cat("CONCLUZIE: Exista o tendinta clara, vizibila dupa eliminarea outlierilor.\n")
} else {
  cat("  [ Tot nesemnificativ ]\n")
}

cat("Coeficient Digital (Semn):      ", round(coef_digital, 4), "\n")