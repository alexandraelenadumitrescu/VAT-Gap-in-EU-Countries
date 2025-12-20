# ==============================================================================
# R Script: Model Robust (Evitare Overfitting)
# ==============================================================================

library(tidyverse)
library(car)
library(lmtest)

# 1. LOAD DATA
df <- read.csv("multe.csv")
# Corrected line: add 'dplyr::' before 'select'
df_clean <- df %>% 
  dplyr::select(-Country, -Year) %>% 
  na.omit()

target_var <- "VAT_Comp" 

# 2. CREAREA MODELULUI SIMPLIFICAT (MANUAL)
# Pe baza rulării anterioare, păstrăm DOAR variabilele care aveau sens statistic și economic:
# 1. VAT_Rever (Colectarea TVA - logic să reducă gap-ul)
# 2. ShadowEc (Economia subterană - logic să crească gap-ul)
# 3. Final_Consumption_PercGDP (Opțional - era la limită, îl testăm)

cat("--- MODEL 1: Doar cele mai puternice 2 variabile ---\n")
model_simple <- lm(VAT_Comp ~ ShadowEc + VAT_Rever, data = df_clean)
summary(model_simple)

cat("\n--- MODEL 2: Adăugăm și Consumul (Max 3 variabile) ---\n")
model_medium <- lm(VAT_Comp ~ ShadowEc + VAT_Rever + Final_Consumption_PercGDP, data = df_clean)
summary(model_medium)

# 3. COMPARAREA MODELELOR (AIC - Cu cât mai mic, cu atât mai bun)
cat("\n--- COMPARATIE AIC (Penalizează complexitatea) ---\n")
aic_simple <- AIC(model_simple)
aic_medium <- AIC(model_medium)

cat("AIC Model 2 Variabile:", aic_simple, "\n")
cat("AIC Model 3 Variabile:", aic_medium, "\n")

if(aic_simple < aic_medium) {
  cat("-> RECOMANDARE: Rămâi la Modelul cu 2 variabile (ShadowEc + VAT_Rever). E mai stabil.\n")
  final_model <- model_simple
} else {
  cat("-> RECOMANDARE: Modelul cu 3 variabile aduce suficientă informație în plus.\n")
  final_model <- model_medium
}

# 4. DIAGNOSTIC FINAL PE MODELUL ALES
cat("\n--- DIAGNOSTIC FINAL ---\n")
print(vif(final_model)) # Verificăm multicoliniaritatea (ar trebui să fie foarte mică acum)

# Teste ipoteze
shapiro <- shapiro.test(residuals(final_model))
bp <- bptest(final_model)

cat("Shapiro-Wilk (Normalitate): p =", round(shapiro$p.value, 4), "\n")
cat("Breusch-Pagan (Homoscedasticitate): p =", round(bp$p.value, 4), "\n")

# Plot
par(mfrow=c(2,2))
plot(final_model)

# Install package for Robust Errors if not already installed
if(!require(sandwich)) install.packages("sandwich")
library(sandwich)
library(lmtest)

cat("--- FINAL ROBUST MODEL (Correcting for Heteroscedasticity) ---\n")

# This function recalculates the P-values to be safe against the Breusch-Pagan failure
robust_results <- coeftest(final_model, vcov = vcovHC(final_model, type = "HC3"))

print(robust_results)