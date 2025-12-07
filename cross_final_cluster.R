# ==============================================================================
# PROIECT ECONOMETRIE: ANALIZA INTEGRATA VAT GAP 2022
# ==============================================================================

# 1. INCARCARE PACHETE
# ------------------------------------------------------------------------------
required_packages <- c("tidyverse", "caret", "corrplot", "factoextra", "cluster", 
                       "lmtest", "car", "glmnet", "moments")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(caret)
library(corrplot)
library(factoextra) # Pentru vizualizare clusteri
library(cluster)
library(lmtest)
library(car)
library(glmnet)

# 2. PREGATIREA DATELOR (Din obiectul 'panelG')
# ------------------------------------------------------------------------------
# Verificam existenta datelor
if(!exists("panelG")) stop("Eroare: Ruleaza intai codul de incarcare a datelor in obiectul 'panelG'!")

# Filtram doar anul 2022 si facem curatenie
data_2022 <- panelG %>% 
  filter(Year == 2022) %>%
  rename(VAT_Gap = Value) # Redenumim pentru claritate

# Setam tara ca index (nume de rand) pentru grafice
rownames(data_2022) <- data_2022$Country

# Corectie: Daca VAT_Gap e zecimal (0.08), il facem procent (8.0)
if(mean(data_2022$VAT_Gap, na.rm=TRUE) < 1) {
  data_2022$VAT_Gap <- data_2022$VAT_Gap * 100
}

cat("\n--- Date Pregatite pentru 2022 ---\n")
str(data_2022)

# ==============================================================================
# ETAPA A: ANALIZA EXPLORATORIE AVANSATA & UNSUPERVISED ML (CLUSTERING)
# ==============================================================================
cat("\n\n=== ETAPA A: MACHINE LEARNING NESUPERVIZAT (CLUSTERING) ===\n")

# A1. Histograma cu Outlieri
ggplot(data_2022, aes(x = VAT_Gap)) +
  geom_histogram(binwidth = 2, fill = "#4e79a7", color = "white", alpha=0.9) +
  geom_vline(aes(xintercept=mean(VAT_Gap)), color="red", linetype="dashed", size=1) +
  labs(title="Distributia Gap-ului de TVA in UE (2022)", 
       subtitle = "Linia rosie indica media UE. Tarile din dreapta sunt outlieri (problematici).",
       x="VAT Gap (%)", y="Numar tari") +
  theme_minimal()

# A2. Pregatire pentru Clustering (Standardizare obligatorie!)
# Selectam variabilele relevante pentru profilul tarii
vars_cluster <- data_2022 %>% 
  select(VAT_Gap, ShadowEconomy, InternetAccess, Governance, GDP_per_capita)

# Scalam datele (Z-score)
data_scaled <- scale(vars_cluster)

# A3. Algoritmul K-Means
set.seed(123)
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# Vizualizare Clusteri (Output pentru proiect)
fviz_cluster(km_res, data = data_scaled,
             palette = c("#2E9FDF", "#E7B800", "#FC4E07"), 
             ggtheme = theme_minimal(),
             main = "Harta Fiscala a UE (K-Means Clustering)",
             repel = TRUE) # Evita suprapunerea etichetelor

# Interpretare Clusteri (Medii pe grupuri)
data_2022$Cluster <- as.factor(km_res$cluster)
cat("\n--- Profilul Economic al Clusterelor (Medii) ---\n")
print(data_2022 %>% 
        group_by(Cluster) %>% 
        summarise(VAT_Gap = mean(VAT_Gap), 
                  ShadowEc = mean(ShadowEconomy),
                  Governance = mean(Governance),
                  GDP = mean(GDP_per_capita)))

# ==============================================================================
# ETAPA B: MODELARE ECONOMETRICA (EXPLICAREA FENOMENULUI)
# ==============================================================================
cat("\n\n=== ETAPA B: REGRESIE MULTIPLA (OLS) & DIAGNOSTIC ===\n")

# B1. Matricea de Corelatie (Verificare multicoliniaritate pre-model)
cor_matrix <- cor(vars_cluster)
corrplot(cor_matrix, method="number", type="upper", tl.col="black", title="Corelatii 2022")

# B2. Estimarea Modelului OLS
# Incercam sa explicam VAT Gap folosind determinanti cheie
model_ols <- lm(VAT_Gap ~ ShadowEconomy + InternetAccess + Governance + StandardVAT, 
                data = data_2022)

cat("\n--- Rezultate Regresie OLS ---\n")
summary(model_ols)

# B3. Diagnosticarea Modelului
# a) Normalitate (Shapiro-Wilk) - Daca p > 0.05 e bine
shapiro <- shapiro.test(resid(model_ols))
cat("Shapiro-Wilk (Normalitate): p =", round(shapiro$p.value, 4), 
    ifelse(shapiro$p.value > 0.05, "(Valid)", "(Nevalid)"), "\n")

# b) Homoscedasticitate (Breusch-Pagan) - Daca p > 0.05 e bine
bp <- bptest(model_ols)
cat("Breusch-Pagan (Homoscedasticitate): p =", round(bp$p.value, 4), 
    ifelse(bp$p.value > 0.05, "(Valid)", "(Heteroscedasticitate!)"), "\n")

# c) Multicoliniaritate (VIF) - Daca valori > 4 sau 5 e rau
cat("\nTest VIF (Multicoliniaritate):\n")
vif_vals <- vif(model_ols)
print(vif_vals)

# ==============================================================================
# ETAPA C: REGULARIZARE (LASSO) - SOLUTIA PENTRU ESANTION MIC
# ==============================================================================
cat("\n\n=== ETAPA C: SELECTIA VARIABILELOR RELEVANTE (LASSO) ===\n")

# Pregatim matricele pentru glmnet
x_lasso <- model.matrix(VAT_Gap ~ ShadowEconomy + InternetAccess + Governance + 
                          StandardVAT + GDP_per_capita + Urbanizare, data_2022)[,-1]
y_lasso <- data_2022$VAT_Gap

set.seed(123)
# Gasim lambda optim prin cross-validation
cv_lasso <- cv.glmnet(x_lasso, y_lasso, alpha = 1)
best_lambda <- cv_lasso$lambda.min

# Rulam modelul final
model_lasso <- glmnet(x_lasso, y_lasso, alpha = 1, lambda = best_lambda)

cat("\nCoeficientii selectati de LASSO (Variabilele care conteaza cu adevarat):\n")
coef(model_lasso)