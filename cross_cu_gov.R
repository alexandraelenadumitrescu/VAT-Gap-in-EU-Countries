# ============================================================================
# PROIECT ECONOMETRIE - ANALIZA GAP-ULUI DE TVA ÎN UE (2022)
# Aplicația 1: Cross-section analysis + ML
# Dataset COMPLET cu variabila Governance
# ============================================================================

# PASUL 1: ÎNCĂRCAREA PACHETELOR
# ============================================================================
library(tidyverse)
library(corrplot)
library(car)
library(lmtest)
library(sandwich)
library(ggplot2)
library(GGally)
library(caret)
library(glmnet)
library(readxl)  # Pentru citire Excel

set.seed(123)


# PASUL 2: ÎNCĂRCAREA DATELOR
# ============================================================================

# Citire din Excel (AJUSTEAZĂ CALEA CĂTRE FIȘIER!)
# Variantă 1: Dacă ai fișier .xlsx
#data_full <- read_excel("vat_gap_governance.xlsx")

# Variantă 2: Dacă ai salvat ca .csv
# data_full <- read.csv("vat_gap_governance.csv", stringsAsFactors = FALSE)
data_full<-panel_govern
# Verificare structură
cat("=== STRUCTURA DATELOR COMPLETE ===\n")
str(data_full)
cat("\nPrimele 10 rânduri:\n")
print(head(data_full, 10))

# Verificare ani disponibili
cat("\n=== ANI DISPONIBILI ÎN DATASET ===\n")
print(unique(data_full$Year))


# PASUL 3: FILTRARE CROSS-SECTION 2022
# ============================================================================

# Filtrare doar anul 2022
data <- data_full %>%
  filter(Year == 2022) %>%
  select(-Year)  # Eliminăm coloana Year

cat("\n=== DATASET CROSS-SECTION 2022 ===\n")
cat(sprintf("Număr de țări: %d\n", nrow(data)))
cat(sprintf("Număr de variabile: %d\n", ncol(data)))

# Verificare țările incluse
cat("\n=== ȚĂRI INCLUSE ÎN ANALIZĂ ===\n")
print(data$Country)


# PASUL 4: PREGĂTIREA ȘI CURĂȚAREA VARIABILELOR
# ============================================================================

# Redenumirea coloanelor pentru consistență
data <- data %>%
  rename(
    country = Country,
    vat_gap = Value,                    # VARIABILA DEPENDENTĂ
    shadow_economy = ShadowEconomy,
    corruption_index = CPI_Score,
    gdp_per_capita = GDP_per_capita,
    unemployment_rate = Unemployment_rate,
    internet_access = InternetAccess,
    standard_vat = StandardVAT,
    urbanization = Urbanizare,
    agri_value_added = VAB.Agriculture,
    final_consumption = FinalConsumption,
    governance = Governance              # VARIABILA NOUĂ ADĂUGATĂ
  )

# Conversia VAT gap în procente (dacă e în format 0-1)
if(max(data$vat_gap, na.rm = TRUE) <= 1) {
  data$vat_gap <- data$vat_gap * 100
  cat("\n✓ VAT gap convertit în procente (0-100)\n")
}

# Verificarea valorilor lipsă
cat("\n=== VERIFICARE VALORI LIPSĂ ===\n")
missing_summary <- colSums(is.na(data))
print(missing_summary[missing_summary > 0])

if(sum(missing_summary) == 0) {
  cat("✓ Nu există valori lipsă în dataset!\n")
}

# Verificarea valorilor extreme (outliers)
cat("\n=== VERIFICARE OUTLIERS ===\n")
outliers_check <- data %>%
  select(-country) %>%
  summarise(across(everything(), 
                   ~sum(abs(scale(.)) > 3, na.rm = TRUE)))
print(outliers_check)


# PASUL 5: ANALIZA EXPLORATORIE (EDA)
# ============================================================================

cat("\n\n========== ANALIZA EXPLORATORIE A DATELOR ==========\n")

# 5.1 Statistici descriptive complete
cat("\n=== STATISTICI DESCRIPTIVE ===\n")
summary_stats <- data %>%
  select(-country) %>%
  summary()
print(summary_stats)

# Statistici detaliate
cat("\n=== COEFICIENȚI DE VARIAȚIE ===\n")
cv_summary <- data %>%
  select(-country) %>%
  summarise(across(everything(), 
                   ~(sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100))
print(round(cv_summary, 2))

# 5.2 Analiza VAT gap pe regiuni
cat("\n=== STATISTICI VAT GAP PE REGIUNI ===\n")

# Definirea regiunilor
data <- data %>%
  mutate(region = case_when(
    country %in% c("Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary",
                   "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia") ~ "Est",
    country %in% c("Greece", "Spain", "Italy", "Portugal", "Cyprus", "Malta") ~ "Sud",
    country %in% c("Denmark", "Finland", "Sweden") ~ "Nord",
    TRUE ~ "Vest"
  ))

region_stats <- data %>%
  group_by(region) %>%
  summarise(
    n_tari = n(),
    mean_vat_gap = mean(vat_gap),
    sd_vat_gap = sd(vat_gap),
    mean_shadow = mean(shadow_economy),
    mean_governance = mean(governance),
    mean_cpi = mean(corruption_index)
  )
print(region_stats)

# 5.3 Top/Bottom 5 țări după VAT gap
cat("\n=== TOP 5 ȚĂRI CU CEL MAI MARE VAT GAP ===\n")
top5 <- data %>%
  arrange(desc(vat_gap)) %>%
  select(country, vat_gap, shadow_economy, corruption_index, governance) %>%
  head(5)
print(top5)

cat("\n=== TOP 5 ȚĂRI CU CEL MAI MIC VAT GAP ===\n")
bottom5 <- data %>%
  arrange(vat_gap) %>%
  select(country, vat_gap, shadow_economy, corruption_index, governance) %>%
  head(5)
print(bottom5)

# 5.4 VIZUALIZĂRI
# Distribuția VAT gap
p1 <- ggplot(data, aes(x = vat_gap)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(vat_gap)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribuția Gap-ului de TVA în UE (2022)",
       subtitle = sprintf("Media = %.2f%%, Mediana = %.2f%%", 
                          mean(data$vat_gap), median(data$vat_gap)),
       x = "VAT Gap (%)", y = "Frecvență") +
  theme_minimal()
print(p1)

# Boxplot cu regiuni
p2 <- ggplot(data, aes(x = region, y = vat_gap, fill = region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "VAT Gap pe Regiuni (2022)",
       x = "Regiune", y = "VAT Gap (%)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p2)

# Heatmap țări
data_sorted <- data %>% arrange(vat_gap)
p3 <- ggplot(data_sorted, aes(x = reorder(country, vat_gap), y = 1, fill = vat_gap)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkgreen", mid = "yellow", high = "darkred", 
                       midpoint = median(data$vat_gap),
                       name = "VAT Gap (%)") +
  labs(title = "VAT Gap pe țări (2022) - Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p3)

# 5.5 MATRICEA DE CORELAȚIE
cat("\n=== MATRICEA DE CORELAȚIE ===\n")

cor_data <- data %>% 
  select(vat_gap, shadow_economy, corruption_index, governance, 
         gdp_per_capita, unemployment_rate, internet_access,
         urbanization, agri_value_added, final_consumption, standard_vat)

correlation_matrix <- cor(cor_data, use = "complete.obs")
print(round(correlation_matrix, 3))

# Vizualizare matrice corelație
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.6,
         col = colorRampPalette(c("darkred", "white", "darkblue"))(200),
         title = "Matricea de Corelație - Variabile Econometrice",
         mar = c(0, 0, 2, 0))

# Identificarea corelațiilor puternice cu VAT gap
cat("\n=== CORELAȚII CU VAT GAP (ordonate descrescător) ===\n")
cor_with_vat <- correlation_matrix[, "vat_gap"] %>%
  sort(decreasing = TRUE)
print(round(cor_with_vat, 3))

# 5.6 Scatter plots pentru variabilele CHEIE
key_vars <- data %>% 
  select(vat_gap, shadow_economy, corruption_index, governance,
         gdp_per_capita, unemployment_rate)

ggpairs(key_vars, 
        title = "Scatter Plot Matrix - Variabile Principale",
        lower = list(continuous = wrap("smooth", alpha = 0.3, color = "blue")),
        upper = list(continuous = wrap("cor", size = 4))) +
  theme_minimal()


# PASUL 6: ÎMPĂRȚIREA ÎN TRAIN/TEST
# ============================================================================

cat("\n\n========== ÎMPĂRȚIREA DATELOR ÎN TRAIN/TEST ==========\n")

# 80% train, 20% test (22 țări train, 5 țări test)
train_index <- createDataPartition(data$vat_gap, p = 0.80, list = FALSE)

train_data <- data[train_index, ]
test_data <- data[-train_index, ]

cat(sprintf("\n✓ Set TRAIN: %d țări (%.0f%%)\n", 
            nrow(train_data), (nrow(train_data)/nrow(data))*100))
cat(sprintf("✓ Set TEST: %d țări (%.0f%%)\n", 
            nrow(test_data), (nrow(test_data)/nrow(data))*100))

cat("\n=== ȚĂRI ÎN SETUL DE TEST ===\n")
print(test_data %>% select(country, vat_gap, shadow_economy, governance))


# PASUL 7: MODELAREA ECONOMETRICĂ
# ============================================================================

cat("\n\n========== CONSTRUIREA MODELELOR ECONOMETRICE ==========\n")

# MODEL 1: Regresie simplă (shadow economy - corelație cea mai puternică)
cat("\n--- MODEL 1: REGRESIE SIMPLĂ ---\n")
cat("Variabila independentă: Shadow Economy\n\n")
model1 <- lm(vat_gap ~ shadow_economy, data = train_data)
summary(model1)

# MODEL 2: Model bifactorial (shadow + governance)
cat("\n--- MODEL 2: REGRESIE BIFACTORIALĂ ---\n")
cat("Variabile: Shadow Economy + Governance\n\n")
model2 <- lm(vat_gap ~ shadow_economy + governance, data = train_data)
summary(model2)

# MODEL 3: Model cu 4 variabile cheie (MODEL OPTIM)
cat("\n--- MODEL 3: MODEL CU 4 VARIABILE CHEIE (OPTIM) ---\n")
cat("Variabile: Shadow Economy, Governance, CPI, Unemployment\n\n")
model3 <- lm(vat_gap ~ shadow_economy + governance + 
               corruption_index + unemployment_rate, 
             data = train_data)
summary(model3)

cat("\n=== TEST VIF (Multicoliniaritate) - Model 3 ===\n")
vif_m3 <- vif(model3)
print(vif_m3)

# MODEL 4: Model complet cu toate variabilele
cat("\n--- MODEL 4: REGRESIE MULTIPLĂ COMPLETĂ ---\n")
model4 <- lm(vat_gap ~ shadow_economy + governance + corruption_index + 
               gdp_per_capita + unemployment_rate + internet_access + 
               urbanization + agri_value_added + final_consumption + standard_vat, 
             data = train_data)
summary(model4)

cat("\n=== TEST VIF (Multicoliniaritate) - Model Complet ===\n")
vif_m4 <- vif(model4)
print(vif_m4)
cat("\nInterpretare VIF:\n")
cat("  VIF < 5:  Multicoliniaritate SCĂZUTĂ ✓\n")
cat("  VIF 5-10: Multicoliniaritate MODERATĂ ⚠\n")
cat("  VIF > 10: Multicoliniaritate SEVERĂ ✗\n")

# MODEL 5: Selecție automată (stepwise)
cat("\n--- MODEL 5: SELECȚIE STEPWISE AUTOMATĂ ---\n")
model_step <- step(model4, direction = "both", trace = 0)
summary(model_step)

cat("\n=== VIF pentru model stepwise ===\n")
# Verificăm dacă modelul are suficiente variabile pentru VIF
n_predictors <- length(coef(model_step)) - 1  # -1 pentru intercept
if(n_predictors >= 2) {
  vif_step <- vif(model_step)
  print(vif_step)
} else {
  cat("NOTĂ: Modelul stepwise are doar", n_predictors, "predictor(i).\n")
  cat("VIF necesită cel puțin 2 predictori.\n")
  cat("Formula finală:\n")
  print(formula(model_step))
}


# PASUL 8: TESTAREA IPOTEZELOR CLASICE
# ============================================================================

cat("\n\n========== TESTAREA IPOTEZELOR ECONOMETRICE ==========\n")

# Alegem modelul optim pentru teste (model3)
model_optim <- model3

# 8.1 Test Breusch-Pagan (Heteroscedasticitate)
cat("\n--- Test Breusch-Pagan (Heteroscedasticitate) ---\n")
bp_test <- bptest(model_optim)
print(bp_test)
cat("\nH0: Homoscedasticitate (varianță constantă)\n")
cat(sprintf("p-value = %.4f\n", bp_test$p.value))
if(bp_test$p.value < 0.05) {
  cat("✗ Respingem H0 → HETEROSCEDASTICITATE prezentă\n")
  cat("  → Vom aplica erori robuste HC3\n")
} else {
  cat("✓ Acceptăm H0 → Homoscedasticitate confirmată\n")
}

# 8.2 Test Shapiro-Wilk (Normalitatea reziduurilor)
cat("\n--- Test Shapiro-Wilk (Normalitate Reziduuri) ---\n")
shapiro_test <- shapiro.test(residuals(model_optim))
print(shapiro_test)
cat("\nH0: Reziduurile sunt normal distribuite\n")
cat(sprintf("p-value = %.4f\n", shapiro_test$p.value))
if(shapiro_test$p.value < 0.05) {
  cat("✗ Respingem H0 → Reziduuri NENORMALE\n")
  cat("  NOTĂ: Cu eșantion mic, deviații minore sunt acceptabile\n")
} else {
  cat("✓ Acceptăm H0 → Reziduuri normal distribuite\n")
}

# 8.3 Test Durbin-Watson (Autocorelare)
cat("\n--- Test Durbin-Watson (Autocorelare) ---\n")
dw_test <- dwtest(model_optim)
print(dw_test)
cat("\nH0: Nu există autocorelare\n")
cat("Interpretare: DW ~ 2 → fără autocorelare\n")
cat(sprintf("Valoarea DW: %.4f\n", dw_test$statistic))

# 8.4 Vizualizarea reziduurilor
par(mfrow = c(2, 2))
plot(model_optim, main = "Diagnostic Plots - Model Optim")
par(mfrow = c(1, 1))

# 8.5 Erori robuste HC3
cat("\n--- Coeficienți cu ERORI ROBUSTE (HC3) ---\n")
cat("Corecție pentru heteroscedasticitate\n\n")
robust_results <- coeftest(model_optim, vcov = vcovHC(model_optim, type = "HC3"))
print(robust_results)

# Comparație: Erori standard vs. Erori robuste
cat("\n=== COMPARAȚIE: Erori Standard vs. Erori Robuste ===\n")
comparison_se <- data.frame(
  Variable = names(coef(model_optim)),
  SE_OLS = summary(model_optim)$coefficients[, "Std. Error"],
  SE_Robust = robust_results[, "Std. Error"],
  Difference_pct = ((robust_results[, "Std. Error"] - 
                       summary(model_optim)$coefficients[, "Std. Error"]) / 
                      summary(model_optim)$coefficients[, "Std. Error"]) * 100
)

# Afișare separată pentru a evita eroarea
cat("\nVariabile:\n")
print(comparison_se$Variable)
cat("\nComparație valori numerice:\n")
print(round(comparison_se[, -1], 4))  # Exclude coloana Variable


# PASUL 9: EVALUAREA CAPACITĂȚII PREDICTIVE
# ============================================================================

cat("\n\n========== PERFORMANȚA PE SETUL DE TEST ==========\n")

# Funcție pentru calcularea metricilor
calculate_metrics <- function(actual, predicted, model_name) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  r2 <- cor(actual, predicted)^2
  
  cat(sprintf("\n%s:\n", model_name))
  cat(sprintf("  RMSE:      %.4f\n", rmse))
  cat(sprintf("  MAE:       %.4f\n", mae))
  cat(sprintf("  MAPE:      %.2f%%\n", mape))
  cat(sprintf("  R² (test): %.4f\n", r2))
  
  return(c(RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2))
}

# Predicții pentru toate modelele
pred_m1 <- predict(model1, newdata = test_data)
pred_m2 <- predict(model2, newdata = test_data)
pred_m3 <- predict(model3, newdata = test_data)
pred_m4 <- predict(model4, newdata = test_data)
pred_step <- predict(model_step, newdata = test_data)

actuals <- test_data$vat_gap

# Calcularea metricilor
metrics_m1 <- calculate_metrics(actuals, pred_m1, "MODEL 1 (Shadow Economy)")
metrics_m2 <- calculate_metrics(actuals, pred_m2, "MODEL 2 (Shadow + Governance)")
metrics_m3 <- calculate_metrics(actuals, pred_m3, "MODEL 3 (4 variabile - OPTIM)")
metrics_m4 <- calculate_metrics(actuals, pred_m4, "MODEL 4 (Toate variabilele)")
metrics_step <- calculate_metrics(actuals, pred_step, "MODEL STEPWISE")

# Tabel comparativ predicții
comparison_table <- data.frame(
  Country = test_data$country,
  Actual = round(actuals, 2),
  M1_Pred = round(pred_m1, 2),
  M3_Pred = round(pred_m3, 2),
  M_Step_Pred = round(pred_step, 2),
  Error_M3 = round(actuals - pred_m3, 2)
)

cat("\n=== COMPARAȚIE PREDICȚII PE SETUL DE TEST ===\n")
print(comparison_table)

# Grafic: Actual vs Predicted (model optim)
comparison_df <- data.frame(
  Country = test_data$country,
  Actual = actuals,
  Predicted = pred_m3
)

ggplot(comparison_df, aes(x = Actual, y = Predicted)) +
  geom_point(size = 4, color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", 
              linetype = "dashed", size = 1) +
  geom_text(aes(label = Country), vjust = -0.7, size = 3.5, fontface = "bold") +
  labs(title = "Valori Reale vs. Predicții (Model Optim)",
       subtitle = sprintf("R² = %.3f | RMSE = %.3f | MAE = %.3f", 
                          metrics_m3["R2"], metrics_m3["RMSE"], metrics_m3["MAE"]),
       x = "VAT Gap Real (%)", 
       y = "VAT Gap Prezis (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))


# PASUL 10: EXTINDEREA MODELULUI
# ============================================================================

cat("\n\n========== EXTINDEREA MODELULUI - FORME FUNCȚIONALE ==========\n")

# 10.1 Model LOG-LOG
cat("\n--- Model LOG-LOG ---\n")
train_log <- train_data %>%
  mutate(
    log_vat_gap = log(vat_gap + 0.1),  # +0.1 pentru valori mici/negative
    log_shadow = log(shadow_economy),
    log_gdp = log(gdp_per_capita)
  )

model_log <- lm(log_vat_gap ~ log_shadow + governance + corruption_index,
                data = train_log)
summary(model_log)
cat("\nInterpretare: Elasticități (coeficienții sunt elasticități în log-log)\n")

# 10.2 Model POLINOMIAL (shadow economy squared)
cat("\n--- Model POLINOMIAL (grad 2) ---\n")
train_data$shadow_sq <- train_data$shadow_economy^2
test_data$shadow_sq <- test_data$shadow_economy^2

model_poly <- lm(vat_gap ~ shadow_economy + shadow_sq + governance + 
                   corruption_index + unemployment_rate,
                 data = train_data)
summary(model_poly)

# 10.3 Model cu DUMMY regional
cat("\n--- Model cu DUMMY REGIONAL (Est vs. Vest) ---\n")
train_data$east_dummy <- ifelse(train_data$region == "Est", 1, 0)
test_data$east_dummy <- ifelse(test_data$region == "Est", 1, 0)

model_dummy <- lm(vat_gap ~ shadow_economy + governance + corruption_index + 
                    unemployment_rate + east_dummy,
                  data = train_data)
summary(model_dummy)
cat(sprintf("\nCoeficientul dummy Est: %.4f\n", coef(model_dummy)["east_dummy"]))
cat("Interpretare: Țările din Est au în medie un VAT gap diferit cu...\n")

# 10.4 Model cu INTERACȚIUNE
cat("\n--- Model cu INTERACȚIUNE (Shadow × Governance) ---\n")
model_interaction <- lm(vat_gap ~ shadow_economy * governance + 
                          corruption_index + unemployment_rate,
                        data = train_data)
summary(model_interaction)
cat("\nInterpretare: Efectul Shadow Economy depinde de nivelul Governance\n")


# PASUL 11: REGULARIZARE (LASSO, RIDGE, ELASTIC NET)
# ============================================================================

cat("\n\n========== TEHNICI DE REGULARIZARE (MACHINE LEARNING) ==========\n")

# Pregătirea matricelor
X_train <- model.matrix(vat_gap ~ shadow_economy + governance + corruption_index + 
                          gdp_per_capita + unemployment_rate + internet_access + 
                          urbanization + agri_value_added + final_consumption + 
                          standard_vat, 
                        data = train_data)[, -1]

y_train <- train_data$vat_gap

X_test <- model.matrix(vat_gap ~ shadow_economy + governance + corruption_index + 
                         gdp_per_capita + unemployment_rate + internet_access + 
                         urbanization + agri_value_added + final_consumption + 
                         standard_vat, 
                       data = test_data)[, -1]

y_test <- test_data$vat_gap

# LASSO (alpha = 1)
cat("\n--- LASSO REGRESSION ---\n")
lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)
plot(lasso_cv, main = "Cross-Validation LASSO")

cat("\nCoeficienți LASSO (lambda optim):\n")
lasso_coef <- coef(lasso_cv, s = "lambda.min")
print(lasso_coef)

cat("\nVariabile ELIMINATE de LASSO (coeficient = 0):\n")
eliminated <- rownames(lasso_coef)[lasso_coef[,1] == 0]
if(length(eliminated) > 0) {
  print(eliminated)
} else {
  cat("Nicio variabilă eliminată\n")
}

lasso_pred <- predict(lasso_cv, s = "lambda.min", newx = X_test)
lasso_metrics <- calculate_metrics(y_test, lasso_pred, "LASSO")

# RIDGE (alpha = 0)
cat("\n--- RIDGE REGRESSION ---\n")
ridge_cv <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 5)
plot(ridge_cv, main = "Cross-Validation RIDGE")

ridge_pred <- predict(ridge_cv, s = "lambda.min", newx = X_test)
ridge_metrics <- calculate_metrics(y_test, ridge_pred, "RIDGE")

# ELASTIC NET (alpha = 0.5)
cat("\n--- ELASTIC NET ---\n")
elastic_cv <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds = 5)

elastic_pred <- predict(elastic_cv, s = "lambda.min", newx = X_test)
elastic_metrics <- calculate_metrics(y_test, elastic_pred, "ELASTIC NET")


# PASUL 12: COMPARAȚIE FINALĂ ÎNTRE TOATE MODELELE
# ============================================================================

cat("\n\n========== COMPARAȚIE FINALĂ - TOATE MODELELE ==========\n")

# Tabel comparativ
results_final <- data.frame(
  Model = c("M1: Shadow Economy", 
            "M2: Shadow + Governance",
            "M3: 4 variabile (OPTIM)",
            "M4: Toate variabilele",
            "M5: Stepwise",
            "M6: Cu Dummy",
            "M7: Cu Interacțiune",
            "LASSO",
            "RIDGE",
            "Elastic Net"),
  RMSE = c(
    metrics_m1["RMSE"],
    metrics_m2["RMSE"],
    metrics_m3["RMSE"],
    metrics_m4["RMSE"],
    metrics_step["RMSE"],
    sqrt(mean((y_test - predict(model_dummy, test_data))^2)),
    sqrt(mean((y_test - predict(model_interaction, test_data))^2)),
    lasso_metrics["RMSE"],
    ridge_metrics["RMSE"],
    elastic_metrics["RMSE"]
  ),
  R2_test = c(
    metrics_m1["R2"],
    metrics_m2["R2"],
    metrics_m3["R2"],
    metrics_m4["R2"],
    metrics_step["R2"],
    cor(y_test, predict(model_dummy, test_data))^2,
    cor(y_test, predict(model_interaction, test_data))^2,
    lasso_metrics["R2"],
    ridge_metrics["R2"],
    elastic_metrics["R2"]
  )
) %>%
  arrange(RMSE)

print(results_final)

# Vizualizare comparație
ggplot(results_final, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", RMSE)), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Comparație Performanță: RMSE pe setul de test",
       subtitle = "Valori mai mici = performanță mai bună",
       x = "", y = "RMSE (Root Mean Squared Error)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))


# PASUL 13: SCENARII DE PROGNOZĂ
# ============================================================================

cat("\n\n========== SCENARII DE PROGNOZĂ ==========\n")

# Folosim modelul optim pentru prognoze
best_model <- model3

# Valori medii pentru context
cat("\n=== VALORI MEDII ÎN EȘANTION (2022) ===\n")
cat(sprintf("Shadow Economy:    %.2f%%\n", mean(train_data$shadow_economy)))
cat(sprintf("Governance:        %.2f\n", mean(train_data$governance)))
cat(sprintf("CPI Score:         %.2f\n", mean(train_data$corruption_index)))
cat(sprintf("Unemployment:      %.2f%%\n", mean(train_data$unemployment_rate)))

# SCENARIU 1: Îmbunătățirea guvernanței cu 0.5 puncte
cat("\n--- SCENARIU 1: Îmbunătățirea Governance cu +0.5 ---\n")
scenario1 <- data.frame(
  shadow_economy = mean(train_data$shadow_economy),
  governance = mean(train_data$governance) + 0.5,
  corruption_index = mean(train_data$corruption_index),
  unemployment_rate = mean(train_data$unemployment_rate)
)

pred1 <- predict(best_model, newdata = scenario1, interval = "confidence", level = 0.95)
cat(sprintf("VAT Gap prezis: %.2f%% [IC 95%%: %.2f%% - %.2f%%]\n", 
            pred1[1], pred1[2], pred1[3]))
cat(sprintf("Reducere față de media actuală: %.2f pp\n", 
            mean(train_data$vat_gap) - pred1[1]))

# SCENARIU 2: Reducerea economiei subterane cu 5 pp
cat("\n--- SCENARIU 2: Reducere Shadow Economy cu -5 pp ---\n")
scenario2 <- data.frame(
  shadow_economy = mean(train_data$shadow_economy) - 5,
  governance = mean(train_data$governance),
  corruption_index = mean(train_data$corruption_index),
  unemployment_rate = mean(train_data$unemployment_rate)
)

pred2 <- predict(best_model, newdata = scenario2, interval = "confidence")
cat(sprintf("VAT Gap prezis: %.2f%% [IC 95%%: %.2f%% - %.2f%%]\n", 
            pred2[1], pred2[2], pred2[3]))
cat(sprintf("Reducere față de media actuală: %.2f pp\n", 
            mean(train_data$vat_gap) - pred2[1]))

# SCENARIU 3: Îmbunătățire combinată
cat("\n--- SCENARIU 3: Îmbunătățire COMBINATĂ ---\n")
cat("(Governance +0.5, Shadow -5, CPI +10)\n")
scenario3 <- data.frame(
  shadow_economy = mean(train_data$shadow_economy) - 5,
  governance = mean(train_data$governance) + 0.5,
  corruption_index = mean(train_data$corruption_index) + 10,
  unemployment_rate = mean(train_data$unemployment_rate)
)

pred3 <- predict(best_model, newdata = scenario3, interval = "confidence")
cat(sprintf("VAT Gap prezis: %.2f%% [IC 95%%: %.2f%% - %.2f%%]\n", 
            pred3[1], pred3[2], pred3[3]))
cat(sprintf("Reducere față de media actuală: %.2f pp\n", 
            mean(train_data$vat_gap) - pred3[1]))

# Vizualizare scenarii
scenarios_viz <- data.frame(
  Scenario = c("Actual (Media)", "Scenariu 1\n(Gov +0.5)", 
               "Scenariu 2\n(Shadow -5)", "Scenariu 3\n(Combinat)"),
  VAT_Gap = c(mean(train_data$vat_gap), pred1[1], pred2[1], pred3[1]),
  Lower = c(NA, pred1[2], pred2[2], pred3[2]),
  Upper = c(NA, pred1[3], pred2[3], pred3[3])
)

ggplot(scenarios_viz, aes(x = Scenario, y = VAT_Gap)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "red") +
  geom_text(aes(label = sprintf("%.2f%%", VAT_Gap)), vjust = -1.5, fontface = "bold") +
  labs(title = "Scenarii de Prognoză - Impact asupra VAT Gap",
       subtitle = "Bare = predicție punctuală | Linii roșii = interval de încredere 95%",
       x = "", y = "VAT Gap (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


cat("\n\n========================================")
cat("\n    ✓ ANALIZĂ FINALIZATĂ CU SUCCES!")
cat("\n========================================\n")

cat("\n=== REZUMAT FINAL ===\n")
cat(sprintf("✓ Dataset: %d țări UE (2022)\n", nrow(data)))
cat(sprintf("✓ Modele construite: 10 (OLS + ML)\n"))
cat(sprintf("✓ Cel mai bun model: %s (RMSE = %.3f)\n", 
            results_final$Model[1], results_final$RMSE[1]))
cat(sprintf("✓ R² pe test: %.3f\n", results_final$R2_test[1]))
cat("\n✓ Ipoteze testate: Heteroscedasticitate, Normalitate, Autocorelare")
cat("\n✓ Tehnici ML aplicate: LASSO, Ridge, Elastic Net")
cat("\n✓ Scenarii de prognoză create: 3\n")

cat("\n📊 Următorii pași pentru proiect:\n")
cat("  1. Interpretează rezultatele din punct de vedere economic\n")
cat("  2. Creează infograficul de prezentare (Canva/Piktochart)\n")
cat("  3. Scrie secțiunea de discuții și concluzii\n")
cat("  4. Redactează literatura de specialitate (5-10 articole)\n")
cat("  5. Completează Anexa 1 (Fișa de evaluare + Declarația AI)\n")
cat("\n========================================\n")