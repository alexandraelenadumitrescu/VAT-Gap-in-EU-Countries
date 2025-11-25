# --- PASUL 0: Încărcare Pachete OBLIGATORII ---
# Instalăm pachetele dacă nu există deja
packages <- c("dplyr", "caret", "car", "lmtest")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(dplyr)  # Aici este operatorul %>%
library(caret)  # Pentru împărțirea datelor Train/Test
library(car)    # Pentru testul VIF
library(lmtest) # Pentru teste de heteroscedasticitate

# --- PASUL 1: Pregătirea Datelor ---
# Citim fișierul (asigură-te că numele e corect)
data <- read.csv("Date_Proiect_UE_2021_Final.csv")
rownames(data) <- data$geo

# Selectăm variabilele pentru model
# Y = VAT_Gap
# X = GDP, Unemployment, Internet, CPI, Agriculture, VAT_Rate
model_data <- data %>% 
  select(VAT_Gap, GDP_per_Capita, Unemployment_Rate, Internet_Access, 
         CPI, Agriculture_Share, VAT_Rate)

# Logaritmăm PIB-ul (pentru a normaliza distribuția - practică standard)
model_data$ln_GDP <- log(model_data$GDP_per_Capita)
# Scoatem PIB-ul brut și păstrăm doar logaritmul
model_data <- model_data %>% select(-GDP_per_Capita)

# --- PASUL 2: Împărțirea Train / Test (Cerința 2d din PDF) ---
set.seed(123) # Pentru rezultate identice la fiecare rulare

# Alegem aleatoriu 80% din țări pentru antrenare (cca 22 țări)
# Restul de 20% (5 țări) rămân ascunse pentru testare
train_index <- createDataPartition(model_data$VAT_Gap, p = 0.80, list = FALSE)
train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]

cat("\n=== Dimensiuni Seturi ===\n")
cat("Set Antrenare:", nrow(train_set), "țări\n")
cat("Set Testare:", nrow(test_set), "țări (", rownames(test_set), ")\n")

# --- PASUL 3: Construirea Modelului OLS (Cerința 3a) ---
# Modelul complet: VAT Gap explicat de toți factorii
ols_model <- lm(VAT_Gap ~ ., data = train_set)

cat("\n=== Rezultatele Regresiei (Summary) ===\n")
print(summary(ols_model))

# --- PASUL 4: Diagnosticarea Modelului (Cerința 3b) ---

# A. Verificarea Multicoliniarității (VIF)
# Dacă VIF > 5 sau 10, avem o problemă (variabilele se repetă)
cat("\n=== Testul VIF (Multicoliniaritate) ===\n")
vif_values <- vif(ols_model)
print(vif_values)

# B. Testul pentru Heteroscedasticitate (Breusch-Pagan)
# H0: Erorile sunt constante (Homoscedasticitate) - Asta vrem!
# P-value > 0.05 înseamnă că e bine.
bp_test <- bptest(ols_model)
cat("\n=== Testul Breusch-Pagan ===\n")
print(bp_test)

# C. Normalitatea Reziduurilor (Shapiro-Wilk)
# H0: Reziduurile sunt distribuite normal - Asta vrem!
# P-value > 0.05 înseamnă că e bine.
shapiro_test <- shapiro.test(ols_model$residuals)
cat("\n=== Testul Shapiro-Wilk (Normalitate) ===\n")
print(shapiro_test)

# --- PASUL 5: Predicție și Validare (Cerința 3c) ---
# Facem predicții pe cele 5 țări ascunse (Test Set)
predictions <- predict(ols_model, newdata = test_set)

# Comparăm Realitatea cu Predicția
results <- data.frame(
  Tara = rownames(test_set),
  Real = test_set$VAT_Gap,
  Predis = round(predictions, 2),
  Eroare = round(test_set$VAT_Gap - predictions, 2)
)

cat("\n=== Performanța pe Setul de Testare ===\n")
print(results)

# Calculăm RMSE (Root Mean Squared Error)
rmse_val <- RMSE(predictions, test_set$VAT_Gap)
r2_adj <- summary(ols_model)$adj.r.squared

cat("\nINDICATORI FINALI DE PERFORMANȚĂ:\n")
cat("RMSE (Eroarea medie):", round(rmse_val, 2), "puncte procentuale\n")
cat("R2 Ajustat (Cât explică modelul):", round(r2_adj, 4) * 100, "%\n")













install.packages("glmnet")
# --- PASUL 6: Optimizarea Modelului (Eliminăm variabilele cu VIF mare) ---
# Scoatem ln_GDP (VIF mare) și Unemployment_Rate (p-value 0.97 - irelevantă)
# Păstrăm: Internet, CPI, Agriculture, VAT_Rate
ols_model_2 <- lm(VAT_Gap ~ Internet_Access + CPI + Agriculture_Share + VAT_Rate, 
                  data = train_set)

cat("\n=== Rezultate Model OLS Optimizat ===\n")
print(summary(ols_model_2))
cat("VIF nou:", vif(ols_model_2), "\n") # Ar trebui să fie toate sub 2-3

# Recalculăm predicțiile cu modelul nou
pred_2 <- predict(ols_model_2, newdata = test_set)
rmse_2 <- RMSE(pred_2, test_set$VAT_Gap)
cat("RMSE Model Optimizat:", round(rmse_2, 2), "\n")


# --- PASUL 7: REGULARIZARE (Cerința 5 din PDF - Obligatorie!) ---
# Vom folosi LASSO și RIDGE pentru a vedea dacă batem OLS-ul
library(glmnet)

# Pregătim datele în format matrice (cerut de glmnet)
x_train <- as.matrix(train_set %>% select(-VAT_Gap))
y_train <- train_set$VAT_Gap
x_test <- as.matrix(test_set %>% select(-VAT_Gap))
y_test <- test_set$VAT_Gap

# A. RIDGE Regression (alpha = 0)
# Ridge păstrează toate variabilele dar le micșorează influența
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0) # Cross-Validation pentru lambda optim
best_lambda_ridge <- cv_ridge$lambda.min
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda_ridge)

# Predicție Ridge
pred_ridge <- predict(ridge_model, s = best_lambda_ridge, newx = x_test)
rmse_ridge <- RMSE(pred_ridge, y_test)

# B. LASSO Regression (alpha = 1)
# Lasso poate elimina complet variabilele inutile (le face coeficientul 0)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda_lasso)

# Predicție Lasso
pred_lasso <- predict(lasso_model, s = best_lambda_lasso, newx = x_test)
rmse_lasso <- RMSE(pred_lasso, y_test)


# --- TABEL COMPARATIV FINAL (Pentru Concluzii) ---
final_results <- data.frame(
  Model = c("OLS Initial", "OLS Optimizat", "Ridge (ML)", "Lasso (ML)"),
  RMSE = c(rmse_val, rmse_2, rmse_ridge, rmse_lasso)
)

cat("\n=== CLASAMENTUL MODELELOR (Cine prezice cel mai bine?) ===\n")
print(final_results)

# Afișăm coeficienții aleși de Lasso (ca să vezi ce variabile au supraviețuit)
cat("\nVariabile selectate de LASSO:\n")
print(coef(lasso_model))