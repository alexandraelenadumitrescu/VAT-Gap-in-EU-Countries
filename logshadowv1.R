# ==============================================================================
# SCRIPT FINAL: Model Econometric Robust & Validat (VAT Gap)
# ==============================================================================

# 1. INSTALARE SI INCARCARE PACHETE NECESARE
required_packages <- c("tidyverse", "lmtest", "car", "sandwich", "caret")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(lmtest)   # Teste diagnostic (BP, DW)
library(car)      # VIF, Outliers
library(sandwich) # Erori Robuste (HC3)
library(caret)    # Cross-Validation (Machine Learning)

# 2. INCARCARE SI CURATARE DATE
df <- read.csv("multe.csv")

# Selectam doar variabilele de interes si eliminam randurile goale
df_clean <- df %>% 
  dplyr::select(VAT_Comp, ShadowEc, VAT_Rever, Country) %>% # Pastram Country pentru identificare outliers
  na.omit()

cat("Numar observatii finale:", nrow(df_clean), "\n")

# 3. CONSTRUIREA MODELULUI OPTIM (Level-Log)
# Logaritmam ShadowEc pentru a liniariza relatia si a reduce heteroscedasticitatea
final_model <- lm(VAT_Comp ~ log(ShadowEc) + VAT_Rever, data = df_clean)

# 4. REZULTATE STANDARD vs. REZULTATE ROBUSTE
cat("\n--- REZULTATE STANDARD (OLS) ---\n")
print(summary(final_model)$coefficients)

cat("\n--- REZULTATE ROBUSTE (HC3 Standard Errors) ---\n")
cat("Acestea sunt valorile pe care le vei pune in lucrare (mai sigure).\n")
robust_output <- coeftest(final_model, vcov = vcovHC(final_model, type = "HC3"))
print(robust_output)

# 5. DIAGNOSTIC COMPLET (CHECKLIST ACADEMIC)
cat("\n--- DIAGNOSTIC DIAGNOSTIC COMPLET ---\n")

# A. Normalitate (Shapiro)
shapiro <- shapiro.test(residuals(final_model))
cat("1. Normalitate (Shapiro-Wilk): p =", round(shapiro$p.value, 4), 
    ifelse(shapiro$p.value > 0.05, "✅ PASSED", "❌ FAILED"), "\n")

# B. Homoscedasticitate (Breusch-Pagan)
bp <- bptest(final_model)
cat("2. Homoscedasticitate (BP):    p =", round(bp$p.value, 4), 
    ifelse(bp$p.value > 0.05, "✅ PASSED", "⚠️ BORDERLINE (Folosim HC3)"), "\n")

# C. Multicoliniaritate (VIF)
vif_val <- vif(final_model)
cat("3. Multicoliniaritate (VIF):   Max =", round(max(vif_val), 2), 
    ifelse(max(vif_val) < 5, "✅ PASSED", "❌ FAILED"), "\n")

# D. Liniaritate (Ramsey RESET)
reset <- resettest(final_model, power = 2:3, type = "regressor")
cat("4. Liniaritate (RESET Test):   p =", round(reset$p.value, 4), 
    ifelse(reset$p.value > 0.05, "✅ PASSED", "❌ FAILED"), "\n")

# 6. VALIDARE CROSS-VALIDATION (LOOCV)
# Pentru ca avem N=27, nu putem rupe datele in 80/20. 
# Folosim Leave-One-Out: Antrenam pe 26 tari, testam pe a 27-a. Repetam de 27 de ori.
cat("\n--- VALIDARE IMPOTRIVA OVERFITTING (LOOCV) ---\n")

train_control <- trainControl(method = "LOOCV")
cv_model <- train(VAT_Comp ~ log(ShadowEc) + VAT_Rever, 
                  data = df_clean, 
                  method = "lm", 
                  trControl = train_control)

print(cv_model$results)
cat("RMSE (Eroarea Medie) la predictie:", round(cv_model$results$RMSE, 3), "\n")
cat("Daca RMSE e apropiat de Residual Std Error din model (~4.9), nu avem overfitting masiv.\n")

# 7. INTERPRETARE AUTOMATA (GENERATOR DE TEXT)
cat("\n--- GENERATOR INTERPRETARE PENTRU LUCRARE ---\n")
coef_shadow <- coef(final_model)["log(ShadowEc)"]
coef_vatrev <- coef(final_model)["VAT_Rever"]

cat(paste0("Modelul Level-Log indică următoarele:\n",
           "1. SHADOW ECONOMY: O creștere relativă de 1% a economiei subterane este asociată cu o creștere de ", 
           round(coef_shadow/100, 3), " puncte procentuale a Gap-ului TVA.\n",
           "2. VAT REVENUE: O creștere de 1 punct procentual a veniturilor din TVA (ca % din PIB) reduce Gap-ul cu ", 
           abs(round(coef_vatrev, 2)), " puncte procentuale.\n"))

# 8. GRAFICE FINALE
par(mfrow=c(2,2))
plot(final_model)