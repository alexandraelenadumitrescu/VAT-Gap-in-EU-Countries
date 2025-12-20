# ==============================================================================
# R Script: Testare Logaritmi pentru Maximizare R2 și Corectare Erori
# ==============================================================================

library(tidyverse)
library(lmtest)
library(car)

# 1. LOAD DATA
df <- read.csv("multe.csv")
df_clean <- df %>% 
  dplyr::select(-Country, -Year) %>% 
  na.omit()

# 2. DEFINIREA CELOR 5 MODELE

# Model 1: Original (Liniar - Liniar)
# Y = b0 + b1*X1 + b2*X2
m1_base <- lm(VAT_Comp ~ ShadowEc + VAT_Rever, data = df_clean)

# Model 2: Log doar pe VAT Revenues (Liniar - Log)
# Y = b0 + b1*Shadow + b2*ln(Revenues)
m2_log_rev <- lm(VAT_Comp ~ ShadowEc + log(VAT_Rever), data = df_clean)

# Model 3: Log doar pe Shadow Economy (Liniar - Log)
# Y = b0 + b1*ln(Shadow) + b2*Revenues
m3_log_shadow <- lm(VAT_Comp ~ log(ShadowEc) + VAT_Rever, data = df_clean)

# Model 4: Log pe ambele variabile X (Liniar - LogLog)
# Y = b0 + b1*ln(Shadow) + b2*ln(Revenues)
m4_log_x <- lm(VAT_Comp ~ log(ShadowEc) + log(VAT_Rever), data = df_clean)

# Model 5: Log pe Variabila Y - Target (Log - Liniar)
# ln(Y) = b0 + b1*X1 + b2*X2
# ATENTIE: Logaritmarea Y-ului schimba scara, R2 nu e direct comparabil, dar vedem BP test.
m5_log_y <- lm(log(VAT_Comp) ~ ShadowEc + VAT_Rever, data = df_clean)


# 3. FUNCTIE PENTRU EXTRAGEREA REZULTATELOR
get_stats <- function(model, name) {
  s <- summary(model)
  bp <- bptest(model)$p.value        # Breusch-Pagan (Vrem > 0.05)
  sh <- shapiro.test(residuals(model))$p.value # Shapiro (Vrem > 0.05)
  r2_adj <- s$adj.r.squared
  aic <- AIC(model)
  
  return(data.frame(
    Model = name,
    Adj_R2 = round(r2_adj, 4),
    AIC = round(aic, 2),
    BP_Test_Pval = round(bp, 4),     # Heteroscedasticitate
    Shapiro_Pval = round(sh, 4),     # Normalitate
    Valid_Assumptions = ifelse(bp > 0.05 & sh > 0.05, "DA", "NU")
  ))
}

# 4. GENERARE TABEL COMPARATIV
results <- rbind(
  get_stats(m1_base, "1. Base (Linear)"),
  get_stats(m2_log_rev, "2. Log(VAT_Rever)"),
  get_stats(m3_log_shadow, "3. Log(ShadowEc)"),
  get_stats(m4_log_x, "4. Log(Both X)"),
  get_stats(m5_log_y, "5. Log(Target Y)")
)

print(results)

# 5. AFISAREA DETALIATA A CASTIGATORULUI (Daca exista unul valid)
best_model_row <- results %>% filter(Valid_Assumptions == "DA") %>% arrange(desc(Adj_R2)) %>% head(1)

if(nrow(best_model_row) > 0) {
  cat("\n--- CASTIGATORUL ESTE: ", best_model_row$Model, "---\n")
  cat("Acest model trece toate testele si are cel mai mare R2 ajustat dintre cele valide.\n")
} else {
  cat("\n--- Niciun model nu trece perfect toate testele. Verifica rezultatele manual. ---\n")
}

# Optional: Vedem sumarul pentru Modelul 2 (Log Revenues) cum ai cerut
cat("\n--- DETALII MODEL 2 (Cererea ta: Log doar pe Revenues) ---\n")
summary(m2_log_rev)



# Modelul Câștigător: Logaritm doar pe Shadow Economy
final_model <- lm(VAT_Comp ~ log(ShadowEc) + VAT_Rever, data = df_clean)

cat("--- REZULTATE FINALE (MODEL VALIDAT) ---\n")
summary(final_model)

# Verificare rapidă finală
cat("BP Test (p > 0.05): ", bptest(final_model)$p.value, "\n")



















# ==============================================================================
# R Script: Audit Complet Ipoteze Regresie (Modelul Castigator)
# ==============================================================================

library(tidyverse)
library(lmtest)
library(car)
library(olsrr) # Pachet excelent pentru diagnostic vizual (instaleaza-l daca nu il ai)
# install.packages("olsrr")

# 1. PREGATIRE DATE & MODEL
df <- read.csv("multe.csv")
df_clean <- df %>% dplyr::select(-Country, -Year) %>% na.omit()

# Modelul 3 (Câștigătorul): Log pe Shadow, Liniar pe VAT Revenues
final_model <- lm(VAT_Comp ~ log(ShadowEc) + VAT_Rever, data = df_clean)

cat("--- REZUMAT MODEL FINAL ---\n")
summary(final_model)

# ==============================================================================
# TESTAREA IPOTEZELOR (CHECKLIST ACADEMIC)
# ==============================================================================

cat("\n--- 1. IPOTEZA DE LINIARITATE (Ramsey RESET Test) ---\n")
# H0: Modelul este specificat corect liniar. Vrem p > 0.05
reset_test <- resettest(final_model, power = 2:3, type = "regressor")
print(reset_test)
if(reset_test$p.value > 0.05) cat("-> REZULTAT: Relatia este liniara (PASSED).\n") else cat("-> ATENTIE: Posibila non-liniaritate.\n")


cat("\n--- 2. IPOTEZA DE NORMALITATE A ERORILOR (Shapiro-Wilk) ---\n")
# H0: Erorile sunt normale. Vrem p > 0.05
shapiro <- shapiro.test(residuals(final_model))
print(shapiro)
if(shapiro$p.value > 0.05) cat("-> REZULTAT: Erorile sunt distribuite normal (PASSED).\n") else cat("-> FAILED: Erorile nu sunt normale.\n")


cat("\n--- 3. IPOTEZA DE HOMOSCEDASTICITATE (Breusch-Pagan) ---\n")
# H0: Varianta erorilor este constanta. Vrem p > 0.05
bp <- bptest(final_model)
print(bp)
if(bp$p.value > 0.05) cat("-> REZULTAT: Varianta este constanta (PASSED).\n") else cat("-> FAILED: Heteroscedasticitate detectata.\n")


cat("\n--- 4. IPOTEZA DE MULTICOLINIARITATE (VIF) ---\n")
# Regula: VIF < 5 (ideal < 3)
vif_val <- vif(final_model)
print(vif_val)
if(max(vif_val) < 5) cat("-> REZULTAT: Nu exista multicoliniaritate (PASSED).\n") else cat("-> FAILED: Variabilele se coreleaza intre ele.\n")


cat("\n--- 5. IPOTEZA DE INDEPENDENTA A ERORILOR (Autocorelare - Durbin Watson) ---\n")
# H0: Nu exista autocorelare. Vrem p > 0.05 si DW aproape de 2.
dw <- dwtest(final_model)
print(dw)
if(dw$p.value > 0.05) cat("-> REZULTAT: Erorile sunt independente (PASSED).\n") else cat("-> FAILED: Exista autocorelare (verificati datele time-series).\n")


cat("\n--- 6. VERIFICARE OUTLIERS (Cook's Distance) ---\n")
# Regula: Daca Cook's Distance > 1, acea tara distorsioneaza modelul.
cooksD <- cooks.distance(final_model)
influential <- cooksD[(cooksD > 1)] # Prag conservator (uneori se foloseste 4/n)

if(length(influential) == 0) {
  cat("-> REZULTAT: Nu exista outliers extremi (PASSED).\n")
} else {
  cat("-> ATENTIE: S-au gasit observatii influente:\n")
  print(influential)
}

# ==============================================================================
# GRAFICE DE DIAGNOSTIC
# ==============================================================================
par(mfrow=c(2,2))
plot(final_model) # Cele 4 grafice standard



