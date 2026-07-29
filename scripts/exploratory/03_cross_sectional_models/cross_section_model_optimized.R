# ==============================================================================
# ETAPA D: OPTIMIZAREA MODELULUI (SOLUTIA FINALA)
# ==============================================================================
cat("\n\n=== ETAPA D: MODEL OPTIMIZAT (Corectie Multicoliniaritate & Heteroscedasticitate) ===\n")

# 1. Alegerea variabilelor noi (Logica Economica)
# - Scoatem Governance (corelat cu ShadowEconomy)
# - Adaugam VAB.Agriculture (sector greu de taxat)
# - Logaritmam GDP_per_capita (relatie non-liniara)
# - Scoatem InternetAccess daca nu e semnificativ, lasam ShadowEconomy ca baza

# Noul Model OLS
model_final <- lm(VAT_Gap ~ ShadowEconomy + VAB.Agriculture + log(GDP_per_capita) + Unemployment_rate, 
                  data = data_2022)

cat("\n--- Rezultate Model Imbunatatit ---\n")
summary(model_final)

# 2. Reverificam Multicoliniaritatea (VIF)
# Ne asteptam sa fie mult mai mici acum (< 2 sau < 3)
cat("\n--- Test VIF (Dupa eliminarea variabilelor redundante) ---\n")
print(vif(model_final))

# 3. Tratarea Heteroscedasticitatii (Robust Standard Errors)
# Deoarece BP test a iesit < 0.05, folosim corectia White (HC1)
if(!require(sandwich)) install.packages("sandwich")
library(sandwich)

cat("\n--- Coeficienti cu Erori Standard Robuste (Corectie Heteroscedasticitate) ---\n")
# Aceasta este tabelul "adevarat" pe care il interpretezi in proiect
model_robust <- coeftest(model_final, vcov = vcovHC(model_final, type = "HC1"))
print(model_robust)

# 4. Comparatie R-Squared
r2_vechi <- summary(model_ols)$r.squared
r2_nou <- summary(model_final)$r.squared

cat("\nImbunatatire R-Squared:\n")
cat("Vechi:", round(r2_vechi, 4), "\n")
cat("Nou:  ", round(r2_nou, 4), "\n")