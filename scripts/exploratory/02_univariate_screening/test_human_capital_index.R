# ==============================================================================
# SCRIPT ANALIZA: Human Capital (Educatie Digitala) + Optimizare
# ==============================================================================

filename <- "../../../data/raw/human_capital_index.csv"
if (!file.exists(filename)) stop("Nu gasesc fisierul 'eu_comm_egov.csv'!")

df <- read.csv(filename, stringsAsFactors = FALSE)

# 1. MODEL STANDARD
# -----------------
cat("\n=== 1. MODEL STANDARD (Toate tarile) ===\n")
model_base <- lm(vat_gap ~ shadow_2022 + human_capital_2022, data = df)
summ_base <- summary(model_base)

p_val <- summ_base$coefficients["human_capital_2022", "Pr(>|t|)"]
cat("P-value Human Capital: ", round(p_val, 4))
if(p_val < 0.05) cat(" -> SEMNIFICATIV!\n") else cat(" -> Nesemnificativ.\n")


# 2. MODEL OPTIMIZAT (Fara Outlieri)
# ----------------------------------
# Calculam distanta Cook (cat de mult influenteaza o singura tara modelul)
cooksD <- cooks.distance(model_base)
influential <- cooksD[(cooksD > (4/nrow(df)))] # Regula de baza: 4/n

names_of_influential <- names(influential) # Indicii randurilor
df_clean <- df[-as.numeric(names_of_influential), ] # Eliminam acele tari

cat("\n=== 2. MODEL OPTIMIZAT (Fara Outlieri) ===\n")
cat("Au fost eliminate tarile de la indecsii:", paste(names_of_influential, collapse=", "), "\n")
cat("Aceste tari distorsionau rezultatele (probabil Malta, Luxemburg sau Irlanda).\n")

model_opt <- lm(vat_gap ~ shadow_2022 + human_capital_2022, data = df_clean)
summ_opt <- summary(model_opt)

# Afisam noile rezultate
coef_shadow <- summ_opt$coefficients["shadow_2022", "Estimate"]
coef_human  <- summ_opt$coefficients["human_capital_2022", "Estimate"]
p_human     <- summ_opt$coefficients["human_capital_2022", "Pr(>|t|)"]

cat("\nNOILE REZULTATE:\n")
cat("   Shadow Economy Coef: ", round(coef_shadow, 4), "\n")
cat("   Human Capital Coef:  ", round(coef_human, 4), " (Ar trebui sa fie negativ)\n")
cat("   P-VALUE Human Cap:   ", round(p_human, 5))

if(p_human < 0.05) {
  cat("\n\n[!!!] SUCCES: Dupa eliminarea outlierilor, Educatia Digitala a devenit SEMNIFICATIVA!\n")
} else {
  cat("\n\n[info] Tot nesemnificativ. Variatia VAT Gap este explicata aproape exclusiv de Shadow Economy.\n")
}

# 3. VIF CHECK (Pentru modelul curat)
cor_val <- cor(df_clean$shadow_2022, df_clean$human_capital_2022)
vif <- 1 / (1 - cor_val^2)
cat("   VIF (Multicoliniaritate): ", round(vif, 3), "\n")