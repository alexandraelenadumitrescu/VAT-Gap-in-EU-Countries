# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# TEMA: Analiza Gap-ului de TVA in tarile UE (2022 si Panel)
# ==============================================================================

# 0. INSTALARE SI INCARCARE PACHETE NECESARE
# ------------------------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")      # Pentru ML si impartire date
if(!require(corrplot)) install.packages("corrplot") # Matrice corelatie
if(!require(lmtest)) install.packages("lmtest")     # Teste ipoteze (BP, DW)
if(!require(car)) install.packages("car")           # VIF
if(!require(plm)) install.packages("plm")           # Date de tip Panel
if(!require(glmnet)) install.packages("glmnet")     # Ridge & Lasso (Regularizare)
if(!require(moments)) install.packages("moments")   # Skewness/Kurtosis

library(tidyverse)
library(caret)
library(corrplot)
library(lmtest)
library(car)
library(plm)
library(glmnet)
library(moments)

# ==============================================================================
# GENERARE DATE SIMULATE (STERGE ACEASTA PARTE CAND AI DATELE REALE)
# ==============================================================================
# Vom crea un dataset pentru cele 27 tari UE, perioada 2015-2022
set.seed(123)
tari <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR","HR","HU",
          "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")
ani <- 2015:2022

# Creare structura panel
data_panel <- expand.grid(Tara = tari, An = ani)

# Simulare variabile (inlocuieste cu datele reale de pe Eurostat/WorldBank)
data_panel$VAT_Gap <- runif(nrow(data_panel), 2, 35) # Variabila dependenta (%)
data_panel$GDP_capita <- runif(nrow(data_panel), 15000, 60000) # PIB/cap
data_panel$Gov_Effectiveness <- runif(nrow(data_panel), -1, 2) # WGI Index
data_panel$Tax_Rate <- runif(nrow(data_panel), 15, 27) # Cota standard TVA
data_panel$Digital_Index <- runif(nrow(data_panel), 30, 90) # DESI Index

# Introducere o corelatie artificiala pentru ca modelul sa aiba sens
data_panel$VAT_Gap <- 40 - 0.0003 * data_panel$GDP_capita - 
  4 * data_panel$Gov_Effectiveness + rnorm(nrow(data_panel), 0, 2)

# ==============================================================================
# APLICATIA 1: MODEL DE REGRESIE TRANSVERSALA (ANUL 2022) [cite: 8]
# ==============================================================================

# 1.1. Pregatirea datelor (Filtrare an 2022)
data_2022 <- data_panel %>% filter(An == 2022)
rownames(data_2022) <- data_2022$Tara # Setam numele randurilor pentru grafice

# 1.2. Analiza Exploratorie (EDA) [cite: 14]
cat("\n--- Statistici Descriptive (2022) ---\n")
summary(data_2022)

# Histograma variabila dependenta
ggplot(data_2022, aes(x = VAT_Gap)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distributia VAT Gap in UE (2022)", x = "VAT Gap (%)", y = "Frecventa")

# Matricea de corelatie [cite: 16]
matrice_cor <- cor(data_2022 %>% select(-Tara, -An))
corrplot(matrice_cor, method = "number", type = "upper", tl.col = "black")

# 1.3. Machine Learning: Clustering (K-Means) [cite: 20]
# Identificam grupuri de tari (ex: performante vs. neperformante fiscal)
set.seed(123)
data_scaled <- scale(data_2022 %>% select(-Tara, -An)) # Standardizare obligatorie
kmeans_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# Vizualizare Cluster
fviz_cluster(kmeans_res, data = data_scaled, geom = "point") + 
  labs(title = "Gruparea tarilor UE pe baza indicatorilor fiscali")
# Adaugam clusterul in datele originale
data_2022$Cluster <- as.factor(kmeans_res$cluster)

# 1.4. Impartire Train / Test (75% Train, 25% Test) [cite: 18]
set.seed(123)
index_train <- createDataPartition(data_2022$VAT_Gap, p = 0.75, list = FALSE)
train_set <- data_2022[index_train, ]
test_set  <- data_2022[-index_train, ]

# 1.5. Modelare Econometrica Clasica (OLS) [cite: 26]
# Model: VAT_Gap = f(GDP, Gov_Effectiveness, Tax_Rate, Digital)
model_ols <- lm(VAT_Gap ~ GDP_capita + Gov_Effectiveness + Tax_Rate + Digital_Index, 
                data = train_set)

cat("\n--- Rezultate Model OLS ---\n")
summary(model_ols)

# 1.6. Validarea Modelului (Teste de diagnostic) [cite: 28]
# a) Normalitatea reziduurilor (Shapiro-Wilk)
shapiro.test(resid(model_ols)) 

# b) Homoscedasticitate (Breusch-Pagan)
bptest(model_ols)

# c) Multicoliniaritate (VIF - Variance Inflation Factor)
vif(model_ols)

# 1.7. Predictie si Evaluare Performanta (pe setul de Test) [cite: 29]
pred_ols <- predict(model_ols, newdata = test_set)
rmse_ols <- RMSE(pred_ols, test_set$VAT_Gap)
cat("\nRMSE OLS pe test:", rmse_ols, "\n")

# 1.8. Extindere: Regularizare (Lasso & Ridge) [cite: 35]
# Aceasta este partea de "Integrarea tehnicilor ML" ceruta la punctul 5
x_train <- model.matrix(VAT_Gap ~ GDP_capita + Gov_Effectiveness + Tax_Rate + Digital_Index, train_set)[,-1]
y_train <- train_set$VAT_Gap
x_test <- model.matrix(VAT_Gap ~ GDP_capita + Gov_Effectiveness + Tax_Rate + Digital_Index, test_set)[,-1]

# Model LASSO (alpha = 1)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda <- cv_lasso$lambda.min
model_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# Predictie Lasso
pred_lasso <- predict(model_lasso, s = best_lambda, newx = x_test)
rmse_lasso <- RMSE(pred_lasso, test_set$VAT_Gap)

cat("\nComparatie Performanta (RMSE): \n")
cat("OLS:", rmse_ols, "\n")
cat("Lasso:", rmse_lasso, "\n")