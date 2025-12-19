# ============================================================================
# PROIECT ECONOMETRIE 2025-2026
# Tema: Determinanții VAT Gap în Uniunea Europeană
# Aplicația 1: Modele de regresie pe date cross-sectional
# ============================================================================

# Instalare și încărcare pachete necesare
# ============================================================================
packages <- c("tidyverse", "ggplot2", "corrplot", "car", "lmtest", 
              "stargazer", "glmnet", "caret", "randomForest", "GGally",
              "sandwich")  # pentru erori robuste

# Instalează pachetele care lipsesc
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Încarcă pachetele
lapply(packages, library, character.only = TRUE)

# ============================================================================
# 1. ÎNCĂRCARE ȘI PREGĂTIRE DATE
# ============================================================================

# Setează directorul de lucru (ajustează calea!)
# setwd("C:/path/to/your/project")

# Citire date
data <- read.csv("cross_2023.csv", stringsAsFactors = FALSE)

# Vizualizare structură date
cat("\n=== STRUCTURA DATELOR ===\n")
str(data)
head(data)

# Curățare: conversie din % în valori numerice
data <- data %>%
  mutate(
    compliance = as.numeric(gsub("%", "", compliance)),
    shadow = as.numeric(gsub("%", "", shadow)),
    # Creăm VAT Gap din compliance (VAT Gap = 100 - Compliance)
    vat_gap = 100 - compliance
  )

# Adaugă numele complete ale țărilor (opțional)
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

# Verificare missing values
cat("\n=== MISSING VALUES ===\n")
colSums(is.na(data))

cat("\n=== DATE CURATE (primele 10 rânduri) ===\n")
print(data %>% select(country_code, country_name, vat_gap, shadow) %>% head(10))

# ============================================================================
# 2. ANALIZĂ EXPLORATORIE (EDA)
# ============================================================================

cat("\n=== STATISTICI DESCRIPTIVE ===\n")
summary(data %>% select(vat_gap, shadow))

# Statistici detaliate
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

# --- Vizualizări ---

# Histograme
p1 <- ggplot(data, aes(x = vat_gap)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(vat_gap)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribuția VAT Gap în UE (2023)",
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

# Boxplots pentru outliers
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

# Scatter plot: VAT Gap vs Shadow Economy
p5 <- ggplot(data, aes(x = shadow, y = vat_gap)) +
  geom_point(size = 3, color = "darkblue", alpha = 0.6) +
  geom_text(aes(label = country_code), hjust = -0.2, vjust = -0.2, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +  # Linear
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.2) +       # Non-linear
  labs(title = "VAT Gap vs Shadow Economy în UE (2023)",
       subtitle = "Albastru = Trend liniar | Roșu = Trend non-liniar (LOESS)",
       x = "Shadow Economy (%)", 
       y = "VAT Gap (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p5)

# Corelația Pearson
cor_test <- cor.test(data$shadow, data$vat_gap)
cat(sprintf("\nCorelație Pearson: r = %.3f, p-value = %.4f\n", 
            cor_test$estimate, cor_test$p.value))

# ============================================================================
# 4. STRATEGIE VALIDARE: LOOCV (Leave-One-Out Cross-Validation)
# ============================================================================

# Cu doar 27 observații, train/test split tradițional nu e optim
# Folosim LOOCV pentru evaluare robustă
cat("\n=== STRATEGIE VALIDARE ===\n")
cat(sprintf("Total observații: %d\n", nrow(data)))
cat("Metodă: Leave-One-Out Cross-Validation (LOOCV)\n")
cat("Justificare: Dataset mic → LOOCV oferă evaluare mai robustă\n")

# Funcție LOOCV
loocv <- function(formula, data) {
  n <- nrow(data)
  predictions <- numeric(n)
  
  for(i in 1:n) {
    # Train pe toate observațiile exceptând i
    train <- data[-i, ]
    test <- data[i, ]
    
    # Fit model
    model <- lm(formula, data = train)
    
    # Predicție
    predictions[i] <- predict(model, newdata = test)
  }
  
  return(predictions)
}

# Pentru model log-log, tratăm separat
loocv_log <- function(formula, data, response_name) {
  n <- nrow(data)
  predictions <- numeric(n)
  
  for(i in 1:n) {
    train <- data[-i, ]
    test <- data[i, ]
    model <- lm(formula, data = train)
    pred_log <- predict(model, newdata = test)
    predictions[i] <- exp(pred_log) - 0.1  # Back-transform
  }
  
  return(predictions)
}

# ============================================================================
# 5. MODELARE ECONOMETRICĂ - MODEL LINIAR
# ============================================================================

cat("\n=== MODEL 1: REGRESIE LINIARĂ SIMPLĂ ===\n")

# Model liniar de bază pe TOT dataset-ul
model1 <- lm(vat_gap ~ shadow, data = data)
summary(model1)

# Erori standard robuste (heteroskedasticity-consistent)
library(sandwich)
library(lmtest)
coeftest(model1, vcov = vcovHC(model1, type = "HC3"))

# Extragere indicatori
r2_m1 <- summary(model1)$r.squared
adj_r2_m1 <- summary(model1)$adj.r.squared
rmse_m1 <- sqrt(mean(model1$residuals^2))
aic_m1 <- AIC(model1)
bic_m1 <- BIC(model1)

cat(sprintf("\nR² = %.4f | Adj R² = %.4f | RMSE = %.4f\n", 
            r2_m1, adj_r2_m1, rmse_m1))
cat(sprintf("AIC = %.2f | BIC = %.2f\n", aic_m1, bic_m1))

# LOOCV pentru model liniar
pred_loocv_m1 <- loocv(vat_gap ~ shadow, data)

# Metrici LOOCV
rmse_loocv_m1 <- sqrt(mean((data$vat_gap - pred_loocv_m1)^2))
mae_loocv_m1 <- mean(abs(data$vat_gap - pred_loocv_m1))
mape_loocv_m1 <- mean(abs((data$vat_gap - pred_loocv_m1) / data$vat_gap)) * 100

cat(sprintf("\n--- LOOCV Performance ---\n"))
cat(sprintf("RMSE (LOOCV) = %.4f\n", rmse_loocv_m1))
cat(sprintf("MAE (LOOCV) = %.4f\n", mae_loocv_m1))
cat(sprintf("MAPE (LOOCV) = %.2f%%\n", mape_loocv_m1))

# Interpretare coeficient
coef_shadow <- coef(model1)[2]
cat(sprintf("\nInterpretare: O creștere cu 1 p.p. a Shadow Economy este asociată cu o creștere de %.3f p.p. a VAT Gap.\n", coef_shadow))

# ============================================================================
# 6. TESTARE VALIDITATE MODEL - IPOTEZE CLASICE
# ============================================================================

cat("\n=== VERIFICARE IPOTEZE CLASICE ===\n")

# A. Normalitatea reziduurilor
cat("\n--- Test Normalitate Reziduuri ---\n")
shapiro_test <- shapiro.test(model1$residuals)
cat(sprintf("Shapiro-Wilk test: W = %.4f, p-value = %.4f\n", 
            shapiro_test$statistic, shapiro_test$p.value))
if(shapiro_test$p.value > 0.05) {
  cat("✓ Reziduurile sunt distribuite normal (p > 0.05)\n")
} else {
  cat("✗ Reziduurile NU sunt distribuite normal (p < 0.05)\n")
}

# QQ-plot
qqnorm(model1$residuals, main = "Q-Q Plot - Model Liniar")
qqline(model1$residuals, col = "red")

# Histogramă reziduuri
hist(model1$residuals, breaks = 10, col = "lightblue", 
     main = "Distribuția Reziduurilor - Model Liniar",
     xlab = "Reziduuri")

# B. Homoscedasticitate
cat("\n--- Test Homoscedasticitate ---\n")
bp_test <- bptest(model1)
cat(sprintf("Breusch-Pagan test: BP = %.4f, p-value = %.4f\n", 
            bp_test$statistic, bp_test$p.value))
if(bp_test$p.value > 0.05) {
  cat("✓ Varianța reziduurilor este constantă (p > 0.05)\n")
} else {
  cat("✗ Heteroscedasticitate prezentă (p < 0.05)\n")
}

# Plot: Fitted vs Residuals
plot(model1$fitted.values, model1$residuals,
     main = "Fitted values vs Residuals",
     xlab = "Fitted values", ylab = "Residuals",
     pch = 19, col = "darkblue")
abline(h = 0, col = "red", lwd = 2)

# ============================================================================
# 7. TESTARE LINEARITATE - RAMSEY RESET TEST
# ============================================================================

cat("\n=== RAMSEY RESET TEST ===\n")
reset_test <- resettest(model1, power = 2:3, type = "fitted")
print(reset_test)

if(reset_test$p.value < 0.05) {
  cat("\n✗ REZULTAT: Respingem H0 (p < 0.05) → Modelul liniar este MIS-SPECIFICAT\n")
  cat("   Concluzie: Există NON-LINEARITATE în relația dintre Shadow Economy și VAT Gap!\n")
} else {
  cat("\n✓ REZULTAT: Acceptăm H0 (p > 0.05) → Modelul liniar este adecvat\n")
}

# ============================================================================
# 8. MODEL NON-LINIAR - FORMĂ FUNCȚIONALĂ PĂTRATICĂ
# ============================================================================

cat("\n=== MODEL 2: REGRESIE PĂTRATICĂ ===\n")

# Model cu termen pătratic
model2 <- lm(vat_gap ~ shadow + I(shadow^2), data = data)
summary(model2)

# Erori standard robuste
coeftest(model2, vcov = vcovHC(model2, type = "HC3"))

# Extragere indicatori
r2_m2 <- summary(model2)$r.squared
adj_r2_m2 <- summary(model2)$adj.r.squared
rmse_m2 <- sqrt(mean(model2$residuals^2))
aic_m2 <- AIC(model2)
bic_m2 <- BIC(model2)

cat(sprintf("\nR² = %.4f | Adj R² = %.4f | RMSE = %.4f\n", 
            r2_m2, adj_r2_m2, rmse_m2))
cat(sprintf("AIC = %.2f | BIC = %.2f\n", aic_m2, bic_m2))

# LOOCV pentru model pătratic
pred_loocv_m2 <- loocv(vat_gap ~ shadow + I(shadow^2), data)

# Metrici LOOCV
rmse_loocv_m2 <- sqrt(mean((data$vat_gap - pred_loocv_m2)^2))
mae_loocv_m2 <- mean(abs(data$vat_gap - pred_loocv_m2))
mape_loocv_m2 <- mean(abs((data$vat_gap - pred_loocv_m2) / data$vat_gap)) * 100

cat(sprintf("\n--- LOOCV Performance ---\n"))
cat(sprintf("RMSE (LOOCV) = %.4f\n", rmse_loocv_m2))
cat(sprintf("MAE (LOOCV) = %.4f\n", mae_loocv_m2))
cat(sprintf("MAPE (LOOCV) = %.2f%%\n", mape_loocv_m2))

# Îmbunătățire față de model liniar
improvement_adj_r2 <- (adj_r2_m2 - adj_r2_m1) / adj_r2_m1 * 100
improvement_loocv <- (rmse_loocv_m1 - rmse_loocv_m2) / rmse_loocv_m1 * 100

cat(sprintf("\nÎmbunătățire Adj R²: %.2f%%\n", improvement_adj_r2))
cat(sprintf("Îmbunătățire RMSE (LOOCV): %.2f%%\n", improvement_loocv))

# Test F pentru semnificație model extins
anova_test <- anova(model1, model2)
print(anova_test)

if(anova_test# ============================================================================
   # PROIECT ECONOMETRIE 2025-2026
   # Tema: Determinanții VAT Gap în Uniunea Europeană
   # Aplicația 1: Modele de regresie pe date cross-sectional
   # ============================================================================
   
   # Instalare și încărcare pachete necesare
   # ============================================================================
   packages <- c("tidyverse", "ggplot2", "corrplot", "car", "lmtest", 
                 "stargazer", "glmnet", "caret", "randomForest", "GGally")
   
   # Instalează pachetele care lipsesc
   new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
   if(length(new_packages)) install.packages(new_packages)
   
   # Încarcă pachetele
   lapply(packages, library, character.only = TRUE)
   
   # ============================================================================
   # 1. ÎNCĂRCARE ȘI PREGĂTIRE DATE
   # ============================================================================
   
   # Setează directorul de lucru (ajustează calea!)
   # setwd("C:/path/to/your/project")
   
   # Citire date
   data <- read.csv("cross_2023.csv", stringsAsFactors = FALSE)
   
   # Vizualizare structură date
   cat("\n=== STRUCTURA DATELOR ===\n")
   str(data)
   head(data)
   
   # Curățare: conversie din % în valori numerice
   data <- data %>%
     mutate(
       compliance = as.numeric(gsub("%", "", compliance)),
       shadow = as.numeric(gsub("%", "", shadow)),
       # Creăm VAT Gap din compliance (VAT Gap = 100 - Compliance)
       vat_gap = 100 - compliance
     )
   
   # Adaugă numele complete ale țărilor (opțional)
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
   
   # Verificare missing values
   cat("\n=== MISSING VALUES ===\n")
   colSums(is.na(data))
   
   cat("\n=== DATE CURATE (primele 10 rânduri) ===\n")
   print(data %>% select(country_code, country_name, vat_gap, shadow) %>% head(10))
   
   # ============================================================================
   # 2. ANALIZĂ EXPLORATORIE (EDA)
   # ============================================================================
   
   cat("\n=== STATISTICI DESCRIPTIVE ===\n")
   summary(data %>% select(vat_gap, shadow))
   
   # Statistici detaliate
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
   
   # --- Vizualizări ---
   
   # Histograme
   p1 <- ggplot(data, aes(x = vat_gap)) +
     geom_histogram(bins = 10, fill = "steelblue", color = "white", alpha = 0.7) +
     geom_vline(aes(xintercept = mean(vat_gap)), color = "red", linetype = "dashed", size = 1) +
     labs(title = "Distribuția VAT Gap în UE (2023)",
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
   
   # Boxplots pentru outliers
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
   
   # Scatter plot: VAT Gap vs Shadow Economy
   p5 <- ggplot(data, aes(x = shadow, y = vat_gap)) +
     geom_point(size = 3, color = "darkblue", alpha = 0.6) +
     geom_text(aes(label = country_code), hjust = -0.2, vjust = -0.2, size = 3) +
     geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +  # Linear
     geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.2) +       # Non-linear
     labs(title = "VAT Gap vs Shadow Economy în UE (2023)",
          subtitle = "Albastru = Trend liniar | Roșu = Trend non-liniar (LOESS)",
          x = "Shadow Economy (%)", 
          y = "VAT Gap (%)") +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold", size = 14))
   
   print(p5)
   
   # Corelația Pearson
   cor_test <- cor.test(data$shadow, data$vat_gap)
   cat(sprintf("\nCorelație Pearson: r = %.3f, p-value = %.4f\n", 
               cor_test$estimate, cor_test$p.value))
   
   # ============================================================================
   # 4. STRATEGIE VALIDARE: LOOCV (Leave-One-Out Cross-Validation)
   # ============================================================================
   
   # Cu doar 27 observații, train/test split tradițional nu e optim
   # Folosim LOOCV pentru evaluare robustă
   cat("\n=== STRATEGIE VALIDARE ===\n")
   cat(sprintf("Total observații: %d\n", nrow(data)))
   cat("Metodă: Leave-One-Out Cross-Validation (LOOCV)\n")
   cat("Justificare: Dataset mic → LOOCV oferă evaluare mai robustă\n")
   
   # Funcție LOOCV
   loocv <- function(formula, data) {
     n <- nrow(data)
     predictions <- numeric(n)
     
     for(i in 1:n) {
       # Train pe toate observațiile exceptând i
       train <- data[-i, ]
       test <- data[i, ]
       
       # Fit model
       model <- lm(formula, data = train)
       
       # Predicție
       predictions[i] <- predict(model, newdata = test)
     }
     
     return(predictions)
   }
   
   # Pentru model log-log, tratăm separat
   loocv_log <- function(formula, data, response_name) {
     n <- nrow(data)
     predictions <- numeric(n)
     
     for(i in 1:n) {
       train <- data[-i, ]
       test <- data[i, ]
       model <- lm(formula, data = train)
       pred_log <- predict(model, newdata = test)
       predictions[i] <- exp(pred_log) - 0.1  # Back-transform
     }
     
     return(predictions)
   }
   
   # ============================================================================
   # 5. MODELARE ECONOMETRICĂ - MODEL LINIAR
   # ============================================================================
   
   cat("\n=== MODEL 1: REGRESIE LINIARĂ SIMPLĂ ===\n")
   
   # Model liniar de bază pe TOT dataset-ul
   model1 <- lm(vat_gap ~ shadow, data = data)
   summary(model1)
   
   # Erori standard robuste (heteroskedasticity-consistent)
   library(sandwich)
   library(lmtest)
   coeftest(model1, vcov = vcovHC(model1, type = "HC3"))
   
   # Extragere indicatori
   r2_m1 <- summary(model1)$r.squared
   adj_r2_m1 <- summary(model1)$adj.r.squared
   rmse_m1 <- sqrt(mean(model1$residuals^2))
   aic_m1 <- AIC(model1)
   bic_m1 <- BIC(model1)
   
   cat(sprintf("\nR² = %.4f | Adj R² = %.4f | RMSE = %.4f\n", 
               r2_m1, adj_r2_m1, rmse_m1))
   cat(sprintf("AIC = %.2f | BIC = %.2f\n", aic_m1, bic_m1))
   
   # LOOCV pentru model liniar
   pred_loocv_m1 <- loocv(vat_gap ~ shadow, data)
   
   # Metrici LOOCV
   rmse_loocv_m1 <- sqrt(mean((data$vat_gap - pred_loocv_m1)^2))
   mae_loocv_m1 <- mean(abs(data$vat_gap - pred_loocv_m1))
   mape_loocv_m1 <- mean(abs((data$vat_gap - pred_loocv_m1) / data$vat_gap)) * 100
   
   cat(sprintf("\n--- LOOCV Performance ---\n"))
   cat(sprintf("RMSE (LOOCV) = %.4f\n", rmse_loocv_m1))
   cat(sprintf("MAE (LOOCV) = %.4f\n", mae_loocv_m1))
   cat(sprintf("MAPE (LOOCV) = %.2f%%\n", mape_loocv_m1))
   
   # Interpretare coeficient
   coef_shadow <- coef(model1)[2]
   cat(sprintf("\nInterpretare: O creștere cu 1 p.p. a Shadow Economy este asociată cu o creștere de %.3f p.p. a VAT Gap.\n", coef_shadow))
   
   # ============================================================================
   # 6. TESTARE VALIDITATE MODEL - IPOTEZE CLASICE
   # ============================================================================
   
   cat("\n=== VERIFICARE IPOTEZE CLASICE ===\n")
   
   # A. Normalitatea reziduurilor
   cat("\n--- Test Normalitate Reziduuri ---\n")
   shapiro_test <- shapiro.test(model1$residuals)
   cat(sprintf("Shapiro-Wilk test: W = %.4f, p-value = %.4f\n", 
               shapiro_test$statistic, shapiro_test$p.value))
   if(shapiro_test$p.value > 0.05) {
     cat("✓ Reziduurile sunt distribuite normal (p > 0.05)\n")
   } else {
     cat("✗ Reziduurile NU sunt distribuite normal (p < 0.05)\n")
   }
   
   # QQ-plot
   qqnorm(model1$residuals, main = "Q-Q Plot - Model Liniar")
   qqline(model1$residuals, col = "red")
   
   # Histogramă reziduuri
   hist(model1$residuals, breaks = 10, col = "lightblue", 
        main = "Distribuția Reziduurilor - Model Liniar",
        xlab = "Reziduuri")
   
   # B. Homoscedasticitate
   cat("\n--- Test Homoscedasticitate ---\n")
   bp_test <- bptest(model1)
   cat(sprintf("Breusch-Pagan test: BP = %.4f, p-value = %.4f\n", 
               bp_test$statistic, bp_test$p.value))
   if(bp_test$p.value > 0.05) {
     cat("✓ Varianța reziduurilor este constantă (p > 0.05)\n")
   } else {
     cat("✗ Heteroscedasticitate prezentă (p < 0.05)\n")
   }
   
   # Plot: Fitted vs Residuals
   plot(model1$fitted.values, model1$residuals,
        main = "Fitted values vs Residuals",
        xlab = "Fitted values", ylab = "Residuals",
        pch = 19, col = "darkblue")
   abline(h = 0, col = "red", lwd = 2)
   
   # ============================================================================
   # 7. TESTARE LINEARITATE - RAMSEY RESET TEST
   # ============================================================================
   
   cat("\n=== RAMSEY RESET TEST ===\n")
   reset_test <- resettest(model1, power = 2:3, type = "fitted")
   print(reset_test)
   
   if(reset_test$p.value < 0.05) {
     cat("\n✗ REZULTAT: Respingem H0 (p < 0.05) → Modelul liniar este MIS-SPECIFICAT\n")
     cat("   Concluzie: Există NON-LINEARITATE în relația dintre Shadow Economy și VAT Gap!\n")
   } else {
     cat("\n✓ REZULTAT: Acceptăm H0 (p > 0.05) → Modelul liniar este adecvat\n")
   }
   
   Pr(>F)`[2] < 0.05) {
  cat("\n✓ Model pătratic este SEMNIFICATIV SUPERIOR modelului liniar (F-test, p < 0.05)\n")
} else {
  cat("\n✗ Model pătratic NU este semnificativ superior\n")
}

# Test semnificație termen pătratic cu erori robuste
coef_quad_robust <- coeftest(model2, vcov = vcovHC(model2, type = "HC3"))
coef_quad_val <- coef_quad_robust["I(shadow^2)", "Estimate"]
coef_quad_p <- coef_quad_robust["I(shadow^2)", "Pr(>|t|)"]

cat(sprintf("\nCoeficient shadow² (robust SE): %.6f (p-value = %.4f)\n", 
            coef_quad_val, coef_quad_p))

if(coef_quad_p < 0.05) {
  cat("✓ Termenul pătratic este SEMNIFICATIV → Relația este NON-LINEARĂ!\n")
  
  # Calcul punct de inflexiune (dacă există)
  b1 <- coef(model2)[2]
  b2 <- coef(model2)[3]
  if(b2 != 0) {
    inflection_point <- -b1 / (2 * b2)
    cat(sprintf("Punct de inflexiune: Shadow Economy = %.2f%%\n", inflection_point))
  }
} else {
  cat("✗ Termenul pătratic NU este semnificativ\n")
}

# Vizualizare model pătratic
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
# 9. MODEL LOG-LOG (ELASTICITĂȚI)
# ============================================================================

cat("\n=== MODEL 3: LOG-LOG ===\n")

# Adaugă transformări logaritmice (evităm log din 0)
data$log_vat_gap <- log(data$vat_gap + 0.1)  # +0.1 pentru valori foarte mici
data$log_shadow <- log(data$shadow)

# Model log-log
model3 <- lm(log_vat_gap ~ log_shadow, data = data)
summary(model3)

# Erori standard robuste
coeftest(model3, vcov = vcovHC(model3, type = "HC3"))

# Extragere indicatori
r2_m3 <- summary(model3)$r.squared
adj_r2_m3 <- summary(model3)$adj.r.squared
rmse_m3 <- sqrt(mean(model3$residuals^2))
aic_m3 <- AIC(model3)
bic_m3 <- BIC(model3)

cat(sprintf("\nR² = %.4f | Adj R² = %.4f | RMSE (log scale) = %.4f\n", 
            r2_m3, adj_r2_m3, rmse_m3))
cat(sprintf("AIC = %.2f | BIC = %.2f\n", aic_m3, bic_m3))

# LOOCV pentru model log-log
pred_loocv_m3 <- loocv_log(log_vat_gap ~ log_shadow, data, "vat_gap")

# Metrici LOOCV (pe scale original)
rmse_loocv_m3 <- sqrt(mean((data$vat_gap - pred_loocv_m3)^2))
mae_loocv_m3 <- mean(abs(data$vat_gap - pred_loocv_m3))
mape_loocv_m3 <- mean(abs((data$vat_gap - pred_loocv_m3) / data$vat_gap)) * 100

cat(sprintf("\n--- LOOCV Performance (original scale) ---\n"))
cat(sprintf("RMSE (LOOCV) = %.4f\n", rmse_loocv_m3))
cat(sprintf("MAE (LOOCV) = %.4f\n", mae_loocv_m3))
cat(sprintf("MAPE (LOOCV) = %.2f%%\n", mape_loocv_m3))

# Interpretare elasticitate
elasticity <- coef(model3)[2]
cat(sprintf("\nElasticitate: O creștere cu 1%% a Shadow Economy → %.3f%% creștere a VAT Gap\n", elasticity))

# ============================================================================
# 10. COMPARAȚIE MODELE - CRITERII SELECȚIE
# ============================================================================

cat("\n=== COMPARAȚIE MODELE ===\n")

# Tabel comparativ - In-sample fit
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

# Tabel comparativ - LOOCV performance
comparison_loocv <- data.frame(
  Model = c("Linear", "Quadratic", "Log-Log"),
  RMSE_LOOCV = c(rmse_loocv_m1, rmse_loocv_m2, rmse_loocv_m3),
  MAE_LOOCV = c(mae_loocv_m1, mae_loocv_m2, mae_loocv_m3),
  MAPE_LOOCV = c(mape_loocv_m1, mape_loocv_m2, mape_loocv_m3)
)

cat("\n--- LOOCV Cross-Validation Performance ---\n")
print(comparison_loocv)

# Selectare model optim bazat pe multiple criterii
cat("\n=== SELECTARE MODEL OPTIM ===\n")

# Criteriu 1: Adj R² maxim
best_adj_r2 <- comparison_insample$Model[which.max(comparison_insample$Adj_R2)]
cat(sprintf("Best by Adj R²: %s (%.4f)\n", 
            best_adj_r2, max(comparison_insample$Adj_R2)))

# Criteriu 2: AIC minim
best_aic <- comparison_insample$Model[which.min(comparison_insample$AIC)]
cat(sprintf("Best by AIC: %s (%.2f)\n", 
            best_aic, min(comparison_insample$AIC)))

# Criteriu 3: BIC minim (penalizează mai mult complexitatea)
best_bic <- comparison_insample$Model[which.min(comparison_insample$BIC)]
cat(sprintf("Best by BIC: %s (%.2f)\n", 
            best_bic, min(comparison_insample$BIC)))

# Criteriu 4: RMSE LOOCV minim
best_loocv <- comparison_loocv$Model[which.min(comparison_loocv$RMSE_LOOCV)]
cat(sprintf("Best by LOOCV RMSE: %s (%.4f)\n", 
            best_loocv, min(comparison_loocv$RMSE_LOOCV)))

# Decizie finală
all_criteria <- c(best_adj_r2, best_aic, best_bic, best_loocv)
optimal_model <- names(sort(table(all_criteria), decreasing = TRUE))[1]

cat(sprintf("\n✓ MODEL OPTIM RECOMANDAT: %s\n", optimal_model))
cat("   (bazat pe consensul criteriilor de selecție)\n")

# ============================================================================
# 11. VIZUALIZARE LOOCV PERFORMANCE
# ============================================================================

cat("\n=== VIZUALIZARE ACTUAL VS PREDICTED (LOOCV) ===\n")

# Combinare predicții LOOCV
loocv_results <- data.frame(
  Country = data$country_code,
  Actual = data$vat_gap,
  Linear = pred_loocv_m1,
  Quadratic = pred_loocv_m2,
  LogLog = pred_loocv_m3
)

# Calculate prediction errors
loocv_results$Error_Linear <- loocv_results$Actual - loocv_results$Linear
loocv_results$Error_Quadratic <- loocv_results$Actual - loocv_results$Quadratic
loocv_results$Error_LogLog <- loocv_results$Actual - loocv_results$LogLog

# Display top 5 largest errors for each model
cat("\n--- Top 5 Largest Prediction Errors (LOOCV) ---\n")
cat("\nLinear Model:\n")
print(loocv_results[order(abs(loocv_results$Error_Linear), decreasing = TRUE)[1:5], 
                    c("Country", "Actual", "Linear", "Error_Linear")])

cat("\nQuadratic Model:\n")
print(loocv_results[order(abs(loocv_results$Error_Quadratic), decreasing = TRUE)[1:5], 
                    c("Country", "Actual", "Quadratic", "Error_Quadratic")])

# Reshape pentru vizualizare
loocv_long <- loocv_results %>%
  select(Country, Actual, Linear, Quadratic, LogLog) %>%
  pivot_longer(cols = c(Linear, Quadratic, LogLog), 
               names_to = "Model", values_to = "Predicted")

# Plot: Actual vs Predicted
p7 <- ggplot(loocv_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  facet_wrap(~ Model) +
  labs(title = "LOOCV: Actual vs Predicted VAT Gap",
       subtitle = "Perfect prediction = diagonal line",
       x = "Actual VAT Gap (%)", 
       y = "Predicted VAT Gap (%)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

print(p7)

# Residual plot pentru model optim
if(optimal_model == "Quadratic") {
  p8 <- ggplot(loocv_results, aes(x = Quadratic, y = Error_Quadratic)) +
    geom_point(size = 3, color = "darkred", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 1) +
    geom_text(aes(label = Country), hjust = -0.2, vjust = -0.2, size = 3) +
    labs(title = "Residual Plot - Quadratic Model (LOOCV)",
         x = "Predicted VAT Gap (%)",
         y = "Residuals (Actual - Predicted)") +
    theme_minimal()
  
  print(p8)
}

# ============================================================================
# 12. EXPORT REZULTATE
# ============================================================================

# Export tabele pentru raport
# write.csv(comparison_insample, "model_comparison_insample.csv", row.names = FALSE)
# write.csv(comparison_loocv, "model_comparison_loocv.csv", row.names = FALSE)
# write.csv(loocv_results, "loocv_predictions.csv", row.names = FALSE)

# Export grafice
# ggsave("scatter_linearity_test.png", plot = p5, width = 10, height = 6, dpi = 300)
# ggsave("quadratic_vs_linear.png", plot = p6, width = 10, height = 6, dpi = 300)
# ggsave("loocv_actual_vs_predicted.png", plot = p7, width = 12, height = 6, dpi = 300)

cat("\n=== SCRIPT FINALIZAT CU SUCCES! ===\n")
cat("\n=== REZUMAT METODOLOGIC ===\n")
cat(sprintf("Dataset: %d țări UE (2023)\n", nrow(data)))
cat("Strategie validare: Leave-One-Out Cross-Validation (LOOCV)\n")
cat("Justificare: Dataset mic (n=27) → LOOCV oferă estimări mai robuste decât train/test split\n")
cat("Erori standard: Heteroskedasticity-consistent (HC3) pentru toate modelele\n")
cat(sprintf("\nModel optim recomandat: %s\n", optimal_model))
cat("\nPROXIMI PAȘI:\n")
cat("1. Adaugă variabile suplimentare (GDP per capita, corruption, unemployment, etc.)\n")
cat("2. Testează modele cu variabile dummy (regiuni, nivel dezvoltare)\n")
cat("3. Aplică tehnici de regularizare (Lasso, Ridge, Elastic Net)\n")
cat("4. Testează modele ML (Random Forest, XGBoost) cu LOOCV\n")
cat("5. Dezvoltă modelul panel pentru Aplicația 2\n")
cat("6. Interpretează rezultatele în context economic și compară cu literatura\n")