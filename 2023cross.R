# ============================================================================
# PROIECT ECONOMETRIE 2025-2026
# Tema: Determinanții VAT Gap în Uniunea Europeană
# Aplicația 1: Modele de regresie pe date cross-sectional
# ============================================================================

# Instalare și încărcare pachete necesare
# ============================================================================
packages <- c("tidyverse", "ggplot2", "corrplot", "car", "lmtest", 
              "stargazer", "glmnet", "caret", "randomForest", "GGally",
              "sandwich")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

lapply(packages, library, character.only = TRUE)

# ============================================================================
# 1. ÎNCĂRCARE ȘI PREGĂTIRE DATE
# ============================================================================

# Citire date
data <- read.csv("cross_2023.csv", stringsAsFactors = FALSE)

cat("\n=== STRUCTURA DATELOR ===\n")
str(data)
head(data)

# Curățare: conversie din % în valori numerice
# IMPORTANT: compliance = VAT GAP (nu rata de colectare!)
data <- data %>%
  mutate(
    vat_gap = as.numeric(gsub("%", "", compliance)),  # compliance E deja VAT gap!
    shadow = as.numeric(gsub("%", "", shadow))
  ) %>%
  select(-compliance)  # eliminăm coloana compliance pentru claritate

# Adaugă numele complete ale țărilor
country_names <- c(
  AT = "Austria", BE = "Belgium", BG = "Bulgaria", CY = "Cyprus",
  CZ = "Czech Republic", DE = "Germany", DK = "Denmark", EE = "Estonia",
  EL = "Greece", ES = "Spain", FI = "Finland", FR = "France",
  HR = "Croatia", HU = "Hungary", IE = "Ireland", IT = "Italy",
  LT = "Lithuania", LU = "Luxembourg", LV = "Latvia", MT = "Malta",
  NL = "Netherlands", PL = "Poland", PT = "Portugal", RO = "Romania",
  SE = "Sweden", SI = "Slovenia", SK = "Slovakia"
)

data$country_name <- country_names[data$country_code]

cat("\n=== MISSING VALUES ===\n")
colSums(is.na(data))

cat("\n=== DATE CURATE (primele 10 rânduri) ===\n")
print(data %>% select(country_code, country_name, vat_gap, shadow) %>% head(10))

cat("\n=== VERIFICARE LOGICĂ DATE ===\n")
cat("VAT Gap: valori mici = colectare bună | valori mari = colectare slabă\n")
cat("Shadow Economy: valori mari = economie subterană ridicată\n\n")

# ============================================================================
# 2. ANALIZĂ EXPLORATORIE (EDA)
# ============================================================================

cat("\n=== STATISTICI DESCRIPTIVE ===\n")
summary(data %>% select(vat_gap, shadow))

cat("\n=== STATISTICI DETALIATE ===\n")
data %>%
  select(vat_gap, shadow) %>%
  summarise(
    across(everything(), 
           list(mean = mean, 
                median = median, 
                sd = sd, 
                min = min, 
                max = max,
                cv = ~sd(.)/mean(.)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "value") %>%
  print()

# Histograme
p1 <- ggplot(data, aes(x = vat_gap)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(vat_gap)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribuția VAT Gap în UE (2023)",
       subtitle = "Valori mici = colectare bună",
       x = "VAT Gap (%)", y = "Frecvență") +
  theme_minimal()

p2 <- ggplot(data, aes(x = shadow)) +
  geom_histogram(bins = 10, fill = "coral", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(shadow)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribuția Shadow Economy în UE (2023)",
       x = "Shadow Economy (%)", y = "Frecvență") +
  theme_minimal()

print(p1)
print(p2)

# Boxplots
p3 <- ggplot(data, aes(y = vat_gap)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  coord_flip() +
  labs(title = "Boxplot VAT Gap", y = "VAT Gap (%)") +
  theme_minimal()

p4 <- ggplot(data, aes(y = shadow)) +
  geom_boxplot(fill = "coral", alpha = 0.6) +
  coord_flip() +
  labs(title = "Boxplot Shadow Economy", y = "Shadow Economy (%)") +
  theme_minimal()

print(p3)
print(p4)

# ============================================================================
# 3. TESTARE LINEARITATE - VIZUALIZARE
# ============================================================================

cat("\n=== TESTARE LINEARITATE - SCATTER PLOT ===\n")

p5 <- ggplot(data, aes(x = shadow, y = vat_gap)) +
  geom_point(size = 3, color = "darkblue", alpha = 0.6) +
  geom_text(aes(label = country_code), hjust = -0.2, vjust = -0.2, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.2) +
  labs(title = "VAT Gap vs Shadow Economy în UE (2023)",
       subtitle = "Albastru = Trend liniar | Roșu = Trend non-liniar (LOESS)",
       x = "Shadow Economy (%)", 
       y = "VAT Gap (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p5)

cor_test <- cor.test(data$shadow, data$vat_gap)
cat(sprintf("\nCorelație Pearson: r = %.3f, p-value = %.4f\n", 
            cor_test$estimate, cor_test$p.value))

if(cor_test$estimate > 0) {
  cat("✓ Corelație POZITIVĂ: Shadow Economy ↑ → VAT Gap ↑ (conform teoriei!)\n")
} else {
  cat("⚠ Corelație NEGATIVĂ: Shadow Economy ↑ → VAT Gap ↓ (contra-intuitiv!)\n")
}

# ============================================================================
# 4. STRATEGIE VALIDARE: LOOCV
# ============================================================================

cat("\n=== STRATEGIE VALIDARE ===\n")
cat(sprintf("Total observații: %d\n", nrow(data)))
cat("Metodă: Leave-One-Out Cross-Validation (LOOCV)\n")
cat("Justificare: Dataset mic → LOOCV oferă evaluare mai robustă\n")

loocv <- function(formula, data) {
  n <- nrow(data)
  predictions <- numeric(n)
  
  for(i in 1:n) {
    train <- data[-i, ]
    test <- data[i, ]
    model <- lm(formula, data = train)
    predictions[i] <- predict(model, newdata = test)
  }
  
  return(predictions)
}

loocv_log <- function(formula, data, response_name) {
  n <- nrow(data)
  predictions <- numeric(n)
  
  for(i in 1:n) {
    train <- data[-i, ]
    test <- data[i, ]
    model <- lm(formula, data = train)
    pred_log <- predict(model, newdata = test)
    predictions[i] <- exp(pred_log)
  }
  
  return(predictions)
}

# ============================================================================
# 5. MODEL LINIAR
# ============================================================================

cat("\n=== MODEL 1: REGRESIE LINIARĂ SIMPLĂ ===\n")

model1 <- lm(vat_gap ~ shadow, data = data)
summary(model1)

cat("\n--- Erori Standard Robuste (HC3) ---\n")
coeftest(model1, vcov = vcovHC(model1, type = "HC3"))

r2_m1 <- summary(model1)$r.squared
adj_r2_m1 <- summary(model1)$adj.r.squared
rmse_m1 <- sqrt(mean(model1$residuals^2))
aic_m1 <- AIC(model1)
bic_m1 <- BIC(model1)

cat(sprintf("\nR² = %.4f | Adj R² = %.4f | RMSE = %.4f\n", 
            r2_m1, adj_r2_m1, rmse_m1))
cat(sprintf("AIC = %.2f | BIC = %.2f\n", aic_m1, bic_m1))

pred_loocv_m1 <- loocv(vat_gap ~ shadow, data)

rmse_loocv_m1 <- sqrt(mean((data$vat_gap - pred_loocv_m1)^2))
mae_loocv_m1 <- mean(abs(data$vat_gap - pred_loocv_m1))
mape_loocv_m1 <- mean(abs((data$vat_gap - pred_loocv_m1) / data$vat_gap)) * 100

cat(sprintf("\n--- LOOCV Performance ---\n"))
cat(sprintf("RMSE (LOOCV) = %.4f\n", rmse_loocv_m1))
cat(sprintf("MAE (LOOCV) = %.4f\n", mae_loocv_m1))
cat(sprintf("MAPE (LOOCV) = %.2f%%\n", mape_loocv_m1))

coef_shadow <- coef(model1)[2]
cat(sprintf("\n=== INTERPRETARE COEFICIENT ===\n"))
cat(sprintf("β (shadow) = %.4f\n", coef_shadow))
if(coef_shadow > 0) {
  cat(sprintf("Interpretare: O creștere cu 1 p.p. a Shadow Economy → %.3f p.p. CREȘTERE VAT Gap\n", coef_shadow))
  cat("Concluzie: Mai multă economie subterană → MAI PUȚINĂ colectare TVA ✓\n")
} else {
  cat(sprintf("Interpretare: O creștere cu 1 p.p. a Shadow Economy → %.3f p.p. SCĂDERE VAT Gap\n", abs(coef_shadow)))
  cat("Concluzie: Relație contra-intuitivă - necesită investigație!\n")
}

# ============================================================================
# 6. VERIFICARE IPOTEZE CLASICE
# ============================================================================

cat("\n=== VERIFICARE IPOTEZE CLASICE ===\n")

cat("\n--- Test Normalitate Reziduuri ---\n")
shapiro_test <- shapiro.test(model1$residuals)
cat(sprintf("Shapiro-Wilk: W = %.4f, p = %.4f\n", 
            shapiro_test$statistic, shapiro_test$p.value))
if(shapiro_test$p.value > 0.05) {
  cat("✓ Reziduurile sunt normale (p > 0.05)\n")
} else {
  cat("✗ Reziduurile NU sunt normale (p < 0.05)\n")
}

qqnorm(model1$residuals, main = "Q-Q Plot - Model Liniar")
qqline(model1$residuals, col = "red")

hist(model1$residuals, breaks = 10, col = "lightblue", 
     main = "Distribuția Reziduurilor", xlab = "Reziduuri")

cat("\n--- Test Homoscedasticitate ---\n")
bp_test <- bptest(model1)
cat(sprintf("Breusch-Pagan: BP = %.4f, p = %.4f\n", 
            bp_test$statistic, bp_test$p.value))
if(bp_test$p.value > 0.05) {
  cat("✓ Varianță constantă (p > 0.05)\n")
} else {
  cat("✗ Heteroscedasticitate prezentă (p < 0.05)\n")
  cat("   → Folosim erori robuste HC3 (deja implementate) ✓\n")
}

plot(model1$fitted.values, model1$residuals,
     main = "Fitted vs Residuals", xlab = "Fitted", ylab = "Residuals",
     pch = 19, col = "darkblue")
abline(h = 0, col = "red", lwd = 2)

# ============================================================================
# 7. RAMSEY RESET TEST
# ============================================================================

cat("\n=== RAMSEY RESET TEST ===\n")
reset_test <- resettest(model1, power = 2:3, type = "fitted")
print(reset_test)

if(reset_test$p.value < 0.05) {
  cat("\n✗ Respingem H0 (p < 0.05) → Model MIS-SPECIFICAT\n")
  cat("   Concluzie: Există NON-LINEARITATE!\n")
} else {
  cat("\n✓ Acceptăm H0 (p > 0.05) → Model liniar adecvat\n")
}

# ============================================================================
# 8. MODEL PĂTRATIC
# ============================================================================

cat("\n=== MODEL 2: REGRESIE PĂTRATICĂ ===\n")

model2 <- lm(vat_gap ~ shadow + I(shadow^2), data = data)
summary(model2)

cat("\n--- Erori Standard Robuste (HC3) ---\n")
coef_robust <- coeftest(model2, vcov = vcovHC(model2, type = "HC3"))
print(coef_robust)

r2_m2 <- summary(model2)$r.squared
adj_r2_m2 <- summary(model2)$adj.r.squared
rmse_m2 <- sqrt(mean(model2$residuals^2))
aic_m2 <- AIC(model2)
bic_m2 <- BIC(model2)

cat(sprintf("\nR² = %.4f | Adj R² = %.4f | RMSE = %.4f\n", 
            r2_m2, adj_r2_m2, rmse_m2))
cat(sprintf("AIC = %.2f | BIC = %.2f\n", aic_m2, bic_m2))

pred_loocv_m2 <- loocv(vat_gap ~ shadow + I(shadow^2), data)

rmse_loocv_m2 <- sqrt(mean((data$vat_gap - pred_loocv_m2)^2))
mae_loocv_m2 <- mean(abs(data$vat_gap - pred_loocv_m2))
mape_loocv_m2 <- mean(abs((data$vat_gap - pred_loocv_m2) / data$vat_gap)) * 100

cat(sprintf("\n--- LOOCV Performance ---\n"))
cat(sprintf("RMSE (LOOCV) = %.4f\n", rmse_loocv_m2))
cat(sprintf("MAE (LOOCV) = %.4f\n", mae_loocv_m2))
cat(sprintf("MAPE (LOOCV) = %.2f%%\n", mape_loocv_m2))

improvement_adj_r2 <- (adj_r2_m2 - adj_r2_m1) / adj_r2_m1 * 100
improvement_loocv <- (rmse_loocv_m1 - rmse_loocv_m2) / rmse_loocv_m1 * 100

cat(sprintf("\nÎmbunătățire Adj R²: %.2f%%\n", improvement_adj_r2))
cat(sprintf("Îmbunătățire RMSE (LOOCV): %.2f%%\n", improvement_loocv))

cat("\n--- Test F (Linear vs Quadratic) ---\n")
anova_test <- anova(model1, model2)
print(anova_test)

if(anova_test$`Pr(>F)`[2] < 0.05) {
  cat("\n✓ Model pătratic SEMNIFICATIV SUPERIOR (F-test, p < 0.05)\n")
} else {
  cat("\n✗ Model pătratic NU este semnificativ superior\n")
}

coef_quad_val <- coef_robust["I(shadow^2)", "Estimate"]
coef_quad_p <- coef_robust["I(shadow^2)", "Pr(>|t|)"]

cat(sprintf("\nCoeficient shadow² (robust SE): %.6f (p = %.4f)\n", 
            coef_quad_val, coef_quad_p))

if(coef_quad_p < 0.05) {
  cat("✓ Termenul pătratic SEMNIFICATIV → NON-LINEARITATE!\n")
  
  b1 <- coef(model2)[2]
  b2 <- coef(model2)[3]
  if(b2 != 0) {
    inflection <- -b1 / (2 * b2)
    cat(sprintf("Punct de inflexiune: Shadow Economy = %.2f%%\n", inflection))
  }
} else {
  cat("✗ Termenul pătratic NU este semnificativ\n")
}

shadow_seq <- seq(min(data$shadow), max(data$shadow), length.out = 100)
pred_quad <- predict(model2, newdata = data.frame(shadow = shadow_seq))

p6 <- ggplot(data, aes(x = shadow, y = vat_gap)) +
  geom_point(size = 3, alpha = 0.6, color = "darkblue") +
  geom_line(data = data.frame(shadow = shadow_seq, vat_gap = pred_quad),
            aes(x = shadow, y = vat_gap), color = "red", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  labs(title = "Model Pătratic vs Model Liniar",
       subtitle = "Roșu = Quadratic | Albastru = Linear",
       x = "Shadow Economy (%)", y = "VAT Gap (%)") +
  theme_minimal()

print(p6)

# ============================================================================
# 9. MODEL LOG-LOG
# ============================================================================

cat("\n=== MODEL 3: LOG-LOG ===\n")

data$log_vat_gap <- log(data$vat_gap)
data$log_shadow <- log(data$shadow)

model3 <- lm(log_vat_gap ~ log_shadow, data = data)
summary(model3)

cat("\n--- Erori Standard Robuste (HC3) ---\n")
coeftest(model3, vcov = vcovHC(model3, type = "HC3"))

r2_m3 <- summary(model3)$r.squared
adj_r2_m3 <- summary(model3)$adj.r.squared
rmse_m3 <- sqrt(mean(model3$residuals^2))
aic_m3 <- AIC(model3)
bic_m3 <- BIC(model3)

cat(sprintf("\nR² = %.4f | Adj R² = %.4f | RMSE (log) = %.4f\n", 
            r2_m3, adj_r2_m3, rmse_m3))
cat(sprintf("AIC = %.2f | BIC = %.2f\n", aic_m3, bic_m3))

pred_loocv_m3 <- loocv_log(log_vat_gap ~ log_shadow, data, "vat_gap")

rmse_loocv_m3 <- sqrt(mean((data$vat_gap - pred_loocv_m3)^2))
mae_loocv_m3 <- mean(abs(data$vat_gap - pred_loocv_m3))
mape_loocv_m3 <- mean(abs((data$vat_gap - pred_loocv_m3) / data$vat_gap)) * 100

cat(sprintf("\n--- LOOCV Performance (original scale) ---\n"))
cat(sprintf("RMSE (LOOCV) = %.4f\n", rmse_loocv_m3))
cat(sprintf("MAE (LOOCV) = %.4f\n", mae_loocv_m3))
cat(sprintf("MAPE (LOOCV) = %.2f%%\n", mape_loocv_m3))

elasticity <- coef(model3)[2]
cat(sprintf("\n=== INTERPRETARE ELASTICITATE ===\n"))
cat(sprintf("Elasticitate = %.4f\n", elasticity))
if(elasticity > 0) {
  cat(sprintf("Interpretare: 1%% ↑ Shadow Economy → %.3f%% ↑ VAT Gap\n", elasticity))
} else {
  cat(sprintf("Interpretare: 1%% ↑ Shadow Economy → %.3f%% ↓ VAT Gap\n", abs(elasticity)))
}

# ============================================================================
# 10. ANALIZA OUTLIERILOR (COOK'S DISTANCE)
# ============================================================================

cat("\n=== ANALIZA OUTLIERILOR ===\n")

cooksd <- cooks.distance(model1)
influential_threshold <- 4/nrow(data)

plot(cooksd, type="h", main="Cook's Distance - Model Liniar",
     ylab="Cook's Distance", xlab="Observație")
abline(h = influential_threshold, col="red", lty=2)
text(1:nrow(data), cooksd, labels=data$country_code, cex=0.7, pos=3)

influential_idx <- which(cooksd > influential_threshold)
cat(sprintf("\nPrag Cook's Distance: %.4f\n", influential_threshold))
cat(sprintf("Observații influente: %d\n", length(influential_idx)))

if(length(influential_idx) > 0) {
  cat("\nȚări cu influență ridicată (outlieri):\n")
  outliers <- data[influential_idx, c("country_code", "country_name", "vat_gap", "shadow")]
  print(outliers)
  
  cat("\n--- Analiza de Robustețe (fără outlieri) ---\n")
  data_no_outliers <- data[-influential_idx, ]
  model1_robust <- lm(vat_gap ~ shadow, data = data_no_outliers)
  
  cat("\nModel FĂRĂ outlieri:\n")
  summary(model1_robust)
  
  cat("\nComparație coeficienți:\n")
  cat(sprintf("Cu outlieri: β = %.4f\n", coef(model1)[2]))
  cat(sprintf("Fără outlieri: β = %.4f\n", coef(model1_robust)[2]))
  cat(sprintf("Diferență: %.4f\n", coef(model1)[2] - coef(model1_robust)[2]))
}

# ============================================================================
# 11. COMPARAȚIE MODELE
# ============================================================================

cat("\n=== COMPARAȚIE MODELE ===\n")

comparison_insample <- data.frame(
  Model = c("Linear", "Quadratic", "Log-Log"),
  R2 = c(r2_m1, r2_m2, r2_m3),
  Adj_R2 = c(adj_r2_m1, adj_r2_m2, adj_r2_m3),
  RMSE = c(rmse_m1, rmse_m2, rmse_m3),
  AIC = c(aic_m1, aic_m2, aic_m3),
  BIC = c(bic_m1, bic_m2, bic_m3)
)

cat("\n--- In-Sample Performance ---\n")
print(comparison_insample)

comparison_loocv <- data.frame(
  Model = c("Linear", "Quadratic", "Log-Log"),
  RMSE_LOOCV = c(rmse_loocv_m1, rmse_loocv_m2, rmse_loocv_m3),
  MAE_LOOCV = c(mae_loocv_m1, mae_loocv_m2, mae_loocv_m3),
  MAPE_LOOCV = c(mape_loocv_m1, mape_loocv_m2, mape_loocv_m3)
)

cat("\n--- LOOCV Performance ---\n")
print(comparison_loocv)

cat("\n=== SELECTARE MODEL OPTIM ===\n")

best_adj_r2 <- comparison_insample$Model[which.max(comparison_insample$Adj_R2)]
cat(sprintf("Best by Adj R²: %s (%.4f)\n", best_adj_r2, max(comparison_insample$Adj_R2)))

best_aic <- comparison_insample$Model[which.min(comparison_insample$AIC)]
cat(sprintf("Best by AIC: %s (%.2f)\n", best_aic, min(comparison_insample$AIC)))

best_bic <- comparison_insample$Model[which.min(comparison_insample$BIC)]
cat(sprintf("Best by BIC: %s (%.2f)\n", best_bic, min(comparison_insample$BIC)))

best_loocv <- comparison_loocv$Model[which.min(comparison_loocv$RMSE_LOOCV)]
cat(sprintf("Best by LOOCV: %s (%.4f)\n", best_loocv, min(comparison_loocv$RMSE_LOOCV)))

all_criteria <- c(best_adj_r2, best_aic, best_bic, best_loocv)
optimal_model <- names(sort(table(all_criteria), decreasing = TRUE))[1]

cat(sprintf("\n✓ MODEL OPTIM: %s\n", optimal_model))
cat("   (bazat pe consensul criteriilor de selecție)\n")

# ============================================================================
# 12. VIZUALIZARE LOOCV
# ============================================================================

loocv_results <- data.frame(
  Country = data$country_code,
  Actual = data$vat_gap,
  Linear = pred_loocv_m1,
  Quadratic = pred_loocv_m2,
  LogLog = pred_loocv_m3,
  Error_Linear = data$vat_gap - pred_loocv_m1,
  Error_Quadratic = data$vat_gap - pred_loocv_m2,
  Error_LogLog = data$vat_gap - pred_loocv_m3
)

cat("\n--- Top 5 Prediction Errors ---\n")
cat("\nLinear:\n")
print(loocv_results[order(abs(loocv_results$Error_Linear), decreasing = TRUE)[1:5], 
                    c("Country", "Actual", "Linear", "Error_Linear")])

cat("\nQuadratic:\n")
print(loocv_results[order(abs(loocv_results$Error_Quadratic), decreasing = TRUE)[1:5], 
                    c("Country", "Actual", "Quadratic", "Error_Quadratic")])

loocv_long <- loocv_results %>%
  select(Country, Actual, Linear, Quadratic, LogLog) %>%
  pivot_longer(cols = c(Linear, Quadratic, LogLog), 
               names_to = "Model", values_to = "Predicted")

p7 <- ggplot(loocv_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  facet_wrap(~ Model) +
  labs(title = "LOOCV: Actual vs Predicted",
       subtitle = "Perfect prediction = diagonal",
       x = "Actual VAT Gap (%)", y = "Predicted (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p7)

# ============================================================================
# 13. EXPORT
# ============================================================================

cat("\n=== SCRIPT FINALIZAT! ===\n")
cat(sprintf("Dataset: %d țări UE\n", nrow(data)))
cat("Validare: LOOCV\n")
cat(sprintf("Model optim: %s\n", optimal_model))
cat("\nPROXIMI PAȘI:\n")
cat("1. Adaugă variabile control (GDP per capita, corruption, unemployment)\n")
cat("2. Aplică regularizare (Lasso, Ridge, Elastic Net)\n")
cat("3. Testează modele ML (Random Forest, XGBoost)\n")
cat("4. Dezvoltă modelul panel (Aplicația 2)\n")