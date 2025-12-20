# ============================================================================
# ANALIZA ECONOMETRICĂ: VAT GAP ȘI ECONOMIA SUBTERANĂ ÎN UE
# ============================================================================

# Încărcarea bibliotecilor necesare
library(tidyverse)    # Pentru manipularea datelor
library(ggplot2)      # Pentru vizualizări
library(corrplot)     # Pentru matricea de corelații
library(car)          # Pentru testarea ipotezelor
library(lmtest)       # Pentru teste econometrice
library(nortest)      # Pentru teste de normalitate
library(moments)      # Pentru asimetrie și kurt

# ============================================================================
# 1. ÎNCĂRCAREA ȘI PREGĂTIREA DATELOR
# ============================================================================

# Crearea dataframe-ului
data <- data.frame(
  country_code = c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", 
                   "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", 
                   "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK"),
  vat_gap = c(1.0, 12.3, 8.6, 3.3, 8.0, 9.7, 8.9, 10.3, 11.4, 7.6, 3.0, 
              5.6, 7.7, 7.4, 8.3, 15.0, 15.1, 0.18, 5.4, 24.2, 7.0, 16.0, 
              3.6, 30.0, 5.3, 4.9, 10.5),
  shadow = c(6.6, 16.0, 33.1, 23.9, 13.5, 8.8, 9.7, 22.7, 20.93, 15.8, 
             10.8, 14.2, 29.7, 25.4, 10.1, 20.3, 22.4, 8.3, 19.9, 23.4, 
             8.2, 21.9, 15.7, 29.0, 10.8, 22.1, 13.1)
)

cat("\n========================================\n")
cat("DATE ÎNCĂRCATE CU SUCCES\n")
cat("========================================\n")
print(head(data))
cat("\nDimensiunea setului de date:", nrow(data), "observații,", ncol(data), "variabile\n")

# ============================================================================
# 2. ANALIZA STATISTICĂ DESCRIPTIVĂ
# ============================================================================

cat("\n========================================\n")
cat("STATISTICI DESCRIPTIVE\n")
cat("========================================\n")

# Statistici pentru VAT Gap
cat("\n--- VAT GAP (%) ---\n")
cat("Medie:", round(mean(data$vat_gap), 2), "%\n")
cat("Mediană:", round(median(data$vat_gap), 2), "%\n")
cat("Dev. Standard:", round(sd(data$vat_gap), 2), "%\n")
cat("Minim:", round(min(data$vat_gap), 2), "%\n")
cat("Maxim:", round(max(data$vat_gap), 2), "%\n")
cat("Coef. Variație:", round(sd(data$vat_gap)/mean(data$vat_gap)*100, 2), "%\n")
cat("Asimetrie:", round(skewness(data$vat_gap), 3), "\n")
cat("Kurtosis:", round(kurtosis(data$vat_gap), 3), "\n")

# Statistici pentru Shadow Economy
cat("\n--- SHADOW ECONOMY (%) ---\n")
cat("Medie:", round(mean(data$shadow), 2), "%\n")
cat("Mediană:", round(median(data$shadow), 2), "%\n")
cat("Dev. Standard:", round(sd(data$shadow), 2), "%\n")
cat("Minim:", round(min(data$shadow), 2), "%\n")
cat("Maxim:", round(max(data$shadow), 2), "%\n")
cat("Coef. Variație:", round(sd(data$shadow)/mean(data$shadow)*100, 2), "%\n")
cat("Asimetrie:", round(skewness(data$shadow), 3), "\n")
cat("Kurtosis:", round(kurtosis(data$shadow), 3), "\n")

# Corelația între variabile
cat("\n--- CORELAȚIA DINTRE VARIABILE ---\n")
corr_matrix <- cor(data[, c("vat_gap", "shadow")])
print(round(corr_matrix, 4))
cat("\nCorelația Pearson:", round(cor(data$vat_gap, data$shadow), 4), "\n")

# ============================================================================
# 3. VIZUALIZĂRI GRAFICE
# ============================================================================

# Histograme
par(mfrow = c(2, 2))

hist(data$vat_gap, 
     main = "Distribuția VAT Gap", 
     xlab = "VAT Gap (%)", 
     col = "lightblue", 
     breaks = 10)
abline(v = mean(data$vat_gap), col = "red", lwd = 2, lty = 2)

hist(data$shadow, 
     main = "Distribuția Shadow Economy", 
     xlab = "Shadow Economy (%)", 
     col = "lightgreen", 
     breaks = 10)
abline(v = mean(data$shadow), col = "red", lwd = 2, lty = 2)

# Boxplots
boxplot(data$vat_gap, 
        main = "Boxplot VAT Gap", 
        ylab = "VAT Gap (%)", 
        col = "lightblue")

boxplot(data$shadow, 
        main = "Boxplot Shadow Economy", 
        ylab = "Shadow Economy (%)", 
        col = "lightgreen")

# Scatter plot
par(mfrow = c(1, 1))
plot(data$shadow, data$vat_gap, 
     main = "Relația între Shadow Economy și VAT Gap",
     xlab = "Shadow Economy (%)", 
     ylab = "VAT Gap (%)",
     pch = 19, 
     col = "blue")
abline(lm(vat_gap ~ shadow, data = data), col = "red", lwd = 2)
text(data$shadow, data$vat_gap, labels = data$country_code, cex = 0.7, pos = 3)

# ============================================================================
# 4. SPECIFICAREA ȘI ESTIMAREA MODELELOR - FORME FUNCȚIONALE
# ============================================================================

cat("\n========================================\n")
cat("COMPARAREA FORMELOR FUNCȚIONALE\n")
cat("========================================\n")

# Transformări logaritmice (adăugăm o constantă mică pentru evitarea log(0))
data$log_vat_gap <- log(data$vat_gap + 0.01)
data$log_shadow <- log(data$shadow)

# --- MODEL 1: LINIAR-LINIAR (LIN-LIN) ---
cat("\n--- MODEL 1: LINIAR-LINIAR ---\n")
cat("Forma: VAT_GAP = β₀ + β₁ × SHADOW + ε\n")

model_linlin <- lm(vat_gap ~ shadow, data = data)
summary_linlin <- summary(model_linlin)
print(summary_linlin)

cat("\nInterpretare: O creștere cu 1 pp în Shadow Economy →\n")
cat("              creștere de", round(coef(model_linlin)[2], 4), "pp în VAT Gap\n")

# --- MODEL 2: LOG-LINIAR (LOG-LIN) ---
cat("\n--- MODEL 2: LOG-LINIAR ---\n")
cat("Forma: log(VAT_GAP) = β₀ + β₁ × SHADOW + ε\n")

model_loglin <- lm(log_vat_gap ~ shadow, data = data)
summary_loglin <- summary(model_loglin)
print(summary_loglin)

cat("\nInterpretare: O creștere cu 1 pp în Shadow Economy →\n")
cat("              creștere de", round(coef(model_loglin)[2]*100, 4), "% în VAT Gap\n")

# --- MODEL 3: LINIAR-LOG (LIN-LOG) ---
cat("\n--- MODEL 3: LINIAR-LOG ---\n")
cat("Forma: VAT_GAP = β₀ + β₁ × log(SHADOW) + ε\n")

model_linlog <- lm(vat_gap ~ log_shadow, data = data)
summary_linlog <- summary(model_linlog)
print(summary_linlog)

cat("\nInterpretare: O creștere cu 1% în Shadow Economy →\n")
cat("              creștere de", round(coef(model_linlog)[2]/100, 4), "pp în VAT Gap\n")

# --- MODEL 4: LOG-LOG ---
cat("\n--- MODEL 4: LOG-LOG ---\n")
cat("Forma: log(VAT_GAP) = β₀ + β₁ × log(SHADOW) + ε\n")

model_loglog <- lm(log_vat_gap ~ log_shadow, data = data)
summary_loglog <- summary(model_loglog)
print(summary_loglog)

cat("\nInterpretare (ELASTICITATE): O creștere cu 1% în Shadow Economy →\n")
cat("                             creștere de", round(coef(model_loglog)[2], 4), "% în VAT Gap\n")

# --- COMPARAREA MODELELOR ---
cat("\n========================================\n")
cat("COMPARAREA PERFORMANȚEI MODELELOR\n")
cat("========================================\n")

# Tabel comparativ
comparison <- data.frame(
  Model = c("Lin-Lin", "Log-Lin", "Lin-Log", "Log-Log"),
  R2 = c(summary_linlin$r.squared, 
         summary_loglin$r.squared, 
         summary_linlog$r.squared, 
         summary_loglog$r.squared),
  R2_adj = c(summary_linlin$adj.r.squared, 
             summary_loglin$adj.r.squared, 
             summary_linlog$adj.r.squared, 
             summary_loglog$adj.r.squared),
  AIC = c(AIC(model_linlin), 
          AIC(model_loglin), 
          AIC(model_linlog), 
          AIC(model_loglog)),
  BIC = c(BIC(model_linlin), 
          BIC(model_loglin), 
          BIC(model_linlog), 
          BIC(model_loglog))
)

# Afișare cu formatare corectă
comparison_display <- comparison
comparison_display[, 2:5] <- round(comparison_display[, 2:5], 4)
print(comparison_display)

# Selectarea modelului optim
best_model_idx <- which.max(comparison$R2_adj)
best_model_name <- comparison$Model[best_model_idx]

cat("\n*** MODELUL OPTIM BAZAT PE R² AJUSTAT:", best_model_name, "***\n")

# Selectăm modelul optim pentru analize ulterioare
if(best_model_name == "Lin-Lin") {
  model <- model_linlin
  model_type <- "linlin"
} else if(best_model_name == "Log-Lin") {
  model <- model_loglin
  model_type <- "loglin"
} else if(best_model_name == "Lin-Log") {
  model <- model_linlog
  model_type <- "linlog"
} else {
  model <- model_loglog
  model_type <- "loglog"
}

# Indicatori de bonitate pentru modelul selectat
r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared
cat("\nR²:", round(r_squared, 4), "\n")
cat("R² ajustat:", round(adj_r_squared, 4), "\n")

# Grafice comparative
par(mfrow = c(2, 2))

# Grafic pentru fiecare model
plot(data$shadow, data$vat_gap, main = "Model Lin-Lin",
     xlab = "Shadow Economy (%)", ylab = "VAT Gap (%)", pch = 19, col = "blue")
abline(model_linlin, col = "red", lwd = 2)

plot(data$shadow, data$log_vat_gap, main = "Model Log-Lin",
     xlab = "Shadow Economy (%)", ylab = "log(VAT Gap)", pch = 19, col = "blue")
abline(model_loglin, col = "red", lwd = 2)

plot(data$log_shadow, data$vat_gap, main = "Model Lin-Log",
     xlab = "log(Shadow Economy)", ylab = "VAT Gap (%)", pch = 19, col = "blue")
abline(model_linlog, col = "red", lwd = 2)

plot(data$log_shadow, data$log_vat_gap, main = "Model Log-Log",
     xlab = "log(Shadow Economy)", ylab = "log(VAT Gap)", pch = 19, col = "blue")
abline(model_loglog, col = "red", lwd = 2)

# ============================================================================
# 5. TESTAREA IPOTEZELOR CLASICE
# ============================================================================

cat("\n========================================\n")
cat("TESTAREA IPOTEZELOR MODELULUI CLASIC\n")
cat("========================================\n")

# Extragerea reziduurilor
residuals <- residuals(model)
fitted_values <- fitted(model)

# --- IPOTEZA 1: Normalitatea reziduurilor ---
cat("\n--- IPOTEZA 1: NORMALITATEA REZIDUURILOR ---\n")

# Test Shapiro-Wilk
shapiro_test <- shapiro.test(residuals)
cat("Test Shapiro-Wilk:\n")
cat("  W =", round(shapiro_test$statistic, 4), "\n")
cat("  p-value =", round(shapiro_test$p.value, 4), "\n")
cat("  Concluzie:", ifelse(shapiro_test$p.value > 0.05, 
                           "Reziduurile sunt normale (p > 0.05)", 
                           "Reziduurile NU sunt normale (p < 0.05)"), "\n")

# Test Jarque-Bera
jb_test <- jarque.test(residuals)
cat("\nTest Jarque-Bera:\n")
cat("  JB =", round(jb_test$statistic, 4), "\n")
cat("  p-value =", round(jb_test$p.value, 4), "\n")

# Grafic QQ-plot
par(mfrow = c(2, 2))
qqnorm(residuals, main = "Q-Q Plot al Reziduurilor")
qqline(residuals, col = "red", lwd = 2)

hist(residuals, 
     main = "Distribuția Reziduurilor", 
     xlab = "Reziduuri", 
     col = "lightgray", 
     breaks = 10)
curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)) * 
        length(residuals) * diff(hist(residuals, plot = FALSE)$breaks)[1], 
      add = TRUE, col = "red", lwd = 2)

# --- IPOTEZA 2: Homoscedasticitatea (varianță constantă) ---
cat("\n--- IPOTEZA 2: HOMOSCEDASTICITATEA ---\n")

# Test Breusch-Pagan
bp_test <- bptest(model)
cat("Test Breusch-Pagan:\n")
cat("  BP =", round(bp_test$statistic, 4), "\n")
cat("  p-value =", round(bp_test$p.value, 4), "\n")
cat("  Concluzie:", ifelse(bp_test$p.value > 0.05, 
                           "Varianta este constantă (homoscedasticitate)", 
                           "Există heteroscedasticitate"), "\n")

# Test White (adaptat la forma funcțională)
cat("\nTest White (forma simplificată):\n")
if(model_type == "linlin") {
  model_white <- lm(residuals^2 ~ shadow + I(shadow^2), data = data)
} else if(model_type == "loglin") {
  model_white <- lm(residuals^2 ~ shadow + I(shadow^2), data = data)
} else if(model_type == "linlog") {
  model_white <- lm(residuals^2 ~ log_shadow + I(log_shadow^2), data = data)
} else {
  model_white <- lm(residuals^2 ~ log_shadow + I(log_shadow^2), data = data)
}
white_test <- summary(model_white)
cat("  R² din regresia auxiliară:", round(white_test$r.squared, 4), "\n")
white_stat <- nrow(data) * white_test$r.squared
white_pval <- 1 - pchisq(white_stat, df = 2)
cat("  Statistică White:", round(white_stat, 4), "\n")
cat("  p-value:", round(white_pval, 4), "\n")

# Grafic reziduuri vs fitted
plot(fitted_values, residuals, 
     main = "Reziduuri vs Valori Estimate",
     xlab = "Valori Estimate", 
     ylab = "Reziduuri",
     pch = 19, 
     col = "blue")
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Grafic Scale-Location
plot(fitted_values, sqrt(abs(residuals)), 
     main = "Scale-Location Plot",
     xlab = "Valori Estimate", 
     ylab = "√|Reziduuri standardizate|",
     pch = 19, 
     col = "blue")
abline(h = mean(sqrt(abs(residuals))), col = "red", lwd = 2, lty = 2)

# --- IPOTEZA 3: Absența autocorelației ---
cat("\n--- IPOTEZA 3: ABSENȚA AUTOCORELAȚIEI ---\n")
cat("Notă: Pentru date cross-sectional, autocorelația nu este o problemă relevantă.\n")
cat("Această ipoteză este importantă pentru serii de timp.\n")

# --- IPOTEZA 4: Absența multicoliniarității ---
cat("\n--- IPOTEZA 4: ABSENȚA MULTICOLINIARITĂȚII ---\n")
cat("Nu este aplicabilă pentru regresie simplă (o singură variabilă independentă).\n")
cat("Va fi testată dacă se adaugă variabile în model.\n")

# --- IPOTEZA 5: Specificarea corectă a modelului ---
cat("\n--- IPOTEZA 5: SPECIFICAREA CORECTĂ A MODELULUI ---\n")

# Test RESET Ramsey
reset_test <- resettest(model, power = 2:3, type = "fitted")
cat("Test RESET Ramsey:\n")
cat("  F-statistic =", round(reset_test$statistic, 4), "\n")
cat("  p-value =", round(reset_test$p.value, 4), "\n")
cat("  Concluzie:", ifelse(reset_test$p.value > 0.05, 
                           "Modelul este corect specificat", 
                           "Modelul ar putea avea probleme de specificare"), "\n")

# ============================================================================
# 6. OBSERVAȚII INFLUENTE ȘI VALORI EXTREME
# ============================================================================

cat("\n========================================\n")
cat("ANALIZA OBSERVAȚIILOR INFLUENTE\n")
cat("========================================\n")

# Calcularea măsurilor de influență
influence_measures <- influence.measures(model)
cooks_d <- cooks.distance(model)
leverage <- hatvalues(model)
std_residuals <- rstandard(model)

# Graficul influenței
par(mfrow = c(2, 2))
plot(model)

# Identificarea observațiilor influente
cat("\n--- OBSERVAȚII CU COOK'S DISTANCE MARE ---\n")
influential <- which(cooks_d > 4/nrow(data))
if(length(influential) > 0) {
  cat("Țări cu influență mare:\n")
  print(data[influential, ])
} else {
  cat("Nu există observații cu influență foarte mare.\n")
}

# Observații cu leverage mare
cat("\n--- OBSERVAȚII CU LEVERAGE MARE ---\n")
high_leverage <- which(leverage > 2 * mean(leverage))
if(length(high_leverage) > 0) {
  cat("Țări cu leverage mare:\n")
  print(data[high_leverage, ])
}

# ============================================================================
# 7. PREDICȚII ȘI INTERVALE DE ÎNCREDERE
# ============================================================================

cat("\n========================================\n")
cat("PREDICȚII ȘI INTERVALE DE ÎNCREDERE\n")
cat("========================================\n")

# Predicții pentru câteva valori (adaptate la forma funcțională)
if(model_type == "linlin" || model_type == "loglin") {
  new_data <- data.frame(shadow = c(10, 15, 20, 25, 30))
  predictions <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)
  
  # Dacă modelul este log-lin, trebuie să transformăm predicțiile înapoi
  if(model_type == "loglin") {
    predictions <- exp(predictions) - 0.01  # Inversarea transformării
  }
  
  cat("\nPredicții pentru diferite niveluri ale Shadow Economy:\n")
  result_table <- cbind(new_data, predictions)
  print(round(result_table, 2))
  
} else if(model_type == "linlog" || model_type == "loglog") {
  new_data <- data.frame(
    shadow = c(10, 15, 20, 25, 30),
    log_shadow = log(c(10, 15, 20, 25, 30))
  )
  predictions <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)
  
  # Dacă modelul este log-log, trebuie să transformăm predicțiile înapoi
  if(model_type == "loglog") {
    predictions <- exp(predictions) - 0.01  # Inversarea transformării
  }
  
  cat("\nPredicții pentru diferite niveluri ale Shadow Economy:\n")
  result_table <- cbind(new_data[, "shadow", drop = FALSE], predictions)
  print(round(result_table, 2))
}

# ============================================================================
# 8. CONCLUZII
# ============================================================================

cat("\n========================================\n")
cat("CONCLUZII PRINCIPALE\n")
cat("========================================\n")

cat("\n1. RELAȚIA DINTRE VARIABILE:\n")
cat("   - Corelația:", round(cor(data$vat_gap, data$shadow), 3), "\n")
cat("   - Există o relație", 
    ifelse(cor(data$vat_gap, data$shadow) > 0, "pozitivă", "negativă"), "\n")

cat("\n2. CALITATEA MODELULUI:\n")
cat("   - R²:", round(r_squared, 3), "\n")
cat("   - Semnificația coeficientului:", 
    ifelse(summary(model)$coefficients[2, 4] < 0.05, "Semnificativ", "Nesemnificativ"), "\n")

cat("\n3. VALIDAREA IPOTEZELOR:\n")
cat("   - Normalitate:", ifelse(shapiro_test$p.value > 0.05, "✓ Satisfăcută", "✗ Nesatisfăcută"), "\n")
cat("   - Homoscedasticitate:", ifelse(bp_test$p.value > 0.05, "✓ Satisfăcută", "✗ Nesatisfăcută"), "\n")
cat("   - Specificare:", ifelse(reset_test$p.value > 0.05, "✓ Satisfăcută", "✗ Nesatisfăcută"), "\n")

cat("\n========================================\n")
cat("SFÂRŞITUL ANALIZEI\n")
cat("========================================\n")

# Resetarea layout-ului grafic
par(mfrow = c(1, 1))




# ============================================================================
# CREAREA VARIABILEI DUMMY: SAF-T IMPLEMENTAT
# ============================================================================

# Lista țărilor care aveau SAF-T implementat (Early Adopters)
# Sursa: OECD, "Guidance on the Standard Audit File for Tax" & Comisia Europeană
# Nota: România este 0 deoarece implementarea a început în 2022 (post-date)
saf_t_countries <- c("PT", "AT", "LU", "FR", "PL", "LT")

# Creăm variabila dummy (1 dacă e în listă, 0 altfel)
data$dummy_saft <- ifelse(data$country_code %in% saf_t_countries, 1, 0)

# Verificăm distribuția (câte țări au 1 și câte au 0)
table(data$dummy_saft)

# ============================================================================
# RE-ESTIMAREA MODELULUI CU DUMMY
# ============================================================================

# Modelul nou: Log-Log + Dummy
# log(VAT) = β0 + β1*log(Shadow) + β2*SAF-T
model_saft <- lm(log_vat_gap ~ log_shadow + dummy_saft, data = data)

summary(model_saft)

# Verificăm dacă normalitatea s-a reparat
shapiro.test(residuals(model_saft))