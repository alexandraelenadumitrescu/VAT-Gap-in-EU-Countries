# ==============================================================================
# PROIECT ECONOMETRIE - SCRIPT FINAL ADAPTAT (panelG)
# ==============================================================================

# 1. INCARCARE PACHETE NECESARE
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")      # ML / Split
if(!require(corrplot)) install.packages("corrplot") # Corelatii
if(!require(lmtest)) install.packages("lmtest")     # Teste (BP, DW)
if(!require(car)) install.packages("car")           # VIF
if(!require(plm)) install.packages("plm")           # Panel Data
if(!require(glmnet)) install.packages("glmnet")     # Lasso/Ridge
if(!require(moments)) install.packages("moments")   # Skewness/Kurtosis

library(tidyverse)
library(caret)
library(corrplot)
library(lmtest)
library(car)
library(plm)
library(glmnet)
library(moments)

# 2. PREGATIREA DATELOR (Folosim obiectul 'panelG')
# ------------------------------------------------------------------------------
# Verificam daca panelG exista. Daca nu, opreste executia cu eroare.
if(!exists("panelG")) stop("Eroare: Obiectul 'panelG' nu a fost gasit in Environment!")

# Copiem datele pentru a nu modifica originalul si facem transformari
df <- panelG

# Transformam VAT Gap (Value) din zecimal in procent (daca e cazul)
# Daca media e sub 1, presupunem ca e zecimal (0.xx) si inmultim cu 100
if(mean(df$Value, na.rm=TRUE) < 1) {
  df$Value <- df$Value * 100
}

# Ne asiguram ca Tara si Anul sunt factori/numerici corecti
df$Country <- as.factor(df$Country)
df$Year <- as.numeric(as.character(df$Year)) # Asigurare ca e numeric

# Redenumim variabila dependenta pentru claritate (Optional, dar recomandat)
df <- df %>% rename(VAT_Gap = Value)

cat("\n--- Sumar Date Procesate ---\n")
str(df)

# ==============================================================================
# APLICATIA 1: ANALIZA TRANSVERSALA (ANUL 2022)
# ==============================================================================
cat("\n\n================ APLICATIA 1: CROSS-SECTION 2022 ================\n")

# 1.1. Filtrare date pentru 2022
data_2022 <- df %>% filter(Year == 2022)
rownames(data_2022) <- data_2022$Country # Punem numele tarilor pe randuri

# 1.2. Analiza Exploratorie (EDA)
# Histograma VAT Gap
ggplot(data_2022, aes(x = VAT_Gap)) +
  geom_histogram(bins = 10, fill = "cornflowerblue", color = "white") +
  labs(title = "Distributia VAT Gap in UE (2022)", x = "VAT Gap (%)", y = "Nr. Tari") +
  theme_minimal()

# Matrice de Corelatie (Selectam doar variabilele numerice relevante)
vars_cor <- data_2022 %>% 
  select(VAT_Gap, ShadowEconomy, InternetAccess, Governance, 
         GDP_per_capita, StandardVAT, Unemployment_rate, CPI_Score)

cor_matrix <- cor(vars_cor, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)

# 1.3. Modelare Clasica (OLS)
# Modelam VAT Gap in functie de Economia Subterana, Digitalizare, Guvernanta si Taxe
# Model propus: VAT_Gap = f(ShadowEconomy, InternetAccess, Governance, StandardVAT)
model_ols <- lm(VAT_Gap ~ ShadowEconomy + InternetAccess + Governance + StandardVAT, 
                data = data_2022)

cat("\n--- Rezultate Regresie OLS (2022) ---\n")
summary(model_ols)

# 1.4. Validarea Modelului
# a) Normalitatea reziduurilor
shapiro_res <- shapiro.test(resid(model_ols))
cat("\nTest Shapiro-Wilk (Normalitate): p-value =", shapiro_res$p.value, "\n")

# b) Homoscedasticitate (Breusch-Pagan)
bp_res <- bptest(model_ols)
cat("Test Breusch-Pagan (Homoscedasticitate): p-value =", bp_res$p.value, "\n")

# c) Multicoliniaritate (VIF)
cat("\nFactorul de Inflatie a Variatiei (VIF):\n")
print(vif(model_ols))

# 1.5. Extindere: Regularizare (Lasso) - util cand avem putine tari (27)
x_vars <- as.matrix(data_2022 %>% select(ShadowEconomy, InternetAccess, Governance, StandardVAT, GDP_per_capita))
y_var <- data_2022$VAT_Gap

set.seed(123)
cv_lasso <- cv.glmnet(x_vars, y_var, alpha = 1) # Gasire lambda optim
best_lambda <- cv_lasso$lambda.min
lasso_model <- glmnet(x_vars, y_var, alpha = 1, lambda = best_lambda)

cat("\nCoeficienti Lasso (selectia variabilelor relevante):\n")
print(coef(lasso_model))