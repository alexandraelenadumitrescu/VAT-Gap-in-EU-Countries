# ============================================================================
# Analiza determinanților VAT Gap cu focus pe Shadow Economy
# Incluează analiza supraestimării SE
# ============================================================================

# 1. INSTALARE ȘI ÎNCĂRCARE PACHETE ========================================
packages <- c("tidyverse", "plm", "lmtest", "sandwich", "car", 
              "stargazer", "ggplot2", "gridExtra", "readxl", "writexl")

install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

invisible(sapply(packages, install_if_missing))

# 2. ÎNCĂRCARE DATE ========================================================
# Adaptează calea către fișierele tale
# Date trebuie să conțină: country, year, VATgap, SE, VATRev, GDPcap, etc.

# Exemplu: citire din Excel
# data <- read_excel("path/to/your/data.xlsx")

# Pentru demonstrație, creez date simulate
set.seed(123)
countries <- rep(c("Romania", "Bulgaria", "Poland", "Germany", "France", 
                   "Italy", "Spain", "Croatia", "Hungary", "Czech Republic",
                   "Slovakia", "Slovenia", "Austria", "Belgium", "Denmark"), 
                 each = 16)
years <- rep(2005:2020, 15)

data <- data.frame(
  country = countries,
  year = years,
  VATgap = rnorm(240, mean = 15, sd = 8) + 
    ifelse(countries %in% c("Romania", "Bulgaria", "Croatia"), 10, 0),
  SE = rnorm(240, mean = 18, sd = 6) + 
    ifelse(countries %in% c("Romania", "Bulgaria", "Croatia"), 10, 0),
  VATRev = rnorm(240, mean = 10.5, sd = 4.5),
  GDPcap = rnorm(240, mean = 36000, sd = 17000) + 
    ifelse(countries %in% c("Germany", "Austria", "Denmark"), 20000, 0),
  PRESS_F = rnorm(240, mean = 89, sd = 8) - 
    ifelse(countries %in% c("Romania", "Bulgaria", "Hungary"), 15, 0),
  FinalConsExp = rnorm(240, mean = 75, sd = 8),
  DIF = rnorm(240, mean = 13, sd = 2.5),
  PS = rnorm(240, mean = 0.74, sd = 0.35) - 
    ifelse(countries %in% c("Romania", "Bulgaria"), 0.3, 0),
  SR = rnorm(240, mean = 21, sd = 2.5)
)

# Asigură valori pozitive
data$VATgap <- abs(data$VATgap)
data$SE <- abs(data$SE)
data$VATRev <- abs(data$VATRev)

# 3. CREARE VARIABILE DERIVATE ============================================

# Transformări logaritmice
data <- data %>%
  mutate(
    LogVATRev = log(VATRev),
    LogGDPcap = log(GDPcap),
    
    # Clasificare țări vechi vs noi UE
    EU_status = ifelse(country %in% c("Germany", "France", "Italy", 
                                      "Spain", "Belgium", "Austria", "Denmark"),
                       "Old", "New"),
    
    # Dummy pentru țări cu SE potențial supraestimat
    # (România, Bulgaria, Croația - valori extreme)
    SE_overestimated = ifelse(country %in% c("Romania", "Bulgaria", "Croatia"), 
                              1, 0)
  )

# 4. STATISTICI DESCRIPTIVE ===============================================

# Summary statistics
summary_stats <- data %>%
  select(VATgap, SE, VATRev, GDPcap, PRESS_F, FinalConsExp, DIF, PS, SR) %>%
  psych::describe() %>%
  as.data.frame() %>%
  select(n, mean, sd, min, max)

print("=== STATISTICI DESCRIPTIVE ===")
print(round(summary_stats, 3))

# Statistici pe grupuri
stats_by_group <- data %>%
  group_by(EU_status) %>%
  summarise(
    n = n(),
    VATgap_mean = mean(VATgap, na.rm = TRUE),
    SE_mean = mean(SE, na.rm = TRUE),
    GDPcap_mean = mean(GDPcap, na.rm = TRUE),
    PRESS_F_mean = mean(PRESS_F, na.rm = TRUE)
  )

print("\n=== STATISTICI PE GRUP (OLD vs NEW EU) ===")
print(stats_by_group)

# 5. MATRICE DE CORELAȚII =================================================

cor_vars <- data %>%
  select(VATgap, SE, VATRev, GDPcap, PRESS_F, FinalConsExp, DIF, PS, SR)

cor_matrix <- cor(cor_vars, use = "complete.obs")

print("\n=== MATRICE CORELAȚII ===")
print(round(cor_matrix, 3))

# Vizualizare matrice corelații
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45)

# 6. REGRESII PANEL DATA ==================================================

# Convertire la panel data
pdata <- pdata.frame(data, index = c("country", "year"))

# Model 1: OLS simplu - doar SE
model1 <- lm(VATgap ~ SE, data = data)

# Model 2: OLS cu VATRev (log)
model2 <- lm(VATgap ~ SE + LogVATRev, data = data)

# Model 3: OLS cu toate variabilele principale
model3 <- lm(VATgap ~ SE + LogVATRev + LogGDPcap + PRESS_F, data = data)

# Model 4: OLS complet
model4 <- lm(VATgap ~ SE + LogVATRev + LogGDPcap + PRESS_F + 
               FinalConsExp + DIF + PS + SR, data = data)

# Model 5: Random Effects Model (REM)
model5 <- plm(VATgap ~ SE + LogVATRev + LogGDPcap + PRESS_F + 
                FinalConsExp + DIF + PS, 
              data = pdata, model = "random")

# Model 6: Fixed Effects Model (FEM)
model6 <- plm(VATgap ~ SE + LogVATRev + LogGDPcap + PRESS_F + 
                FinalConsExp + DIF + PS, 
              data = pdata, model = "within")

# Test Hausman (REM vs FEM)
hausman_test <- phtest(model5, model6)
print("\n=== HAUSMAN TEST (REM vs FEM) ===")
print(hausman_test)

# 7. MODELE CU SUPRAESTIMARE SE ============================================

# Model 7: SE cu dummy pentru supraestimare
model7 <- lm(VATgap ~ SE + SE_overestimated + SE:SE_overestimated + 
               LogVATRev + LogGDPcap + PRESS_F + PS, data = data)

# Model 8: Calcul index de discrepanță SE
# Simulăm o a doua estimare a SE (de ex. din altă sursă)
data <- data %>%
  mutate(
    SE_alt = SE + rnorm(n(), mean = 0, sd = 3),
    SE_discrepancy = abs(SE - SE_alt),
    SE_uncertainty = SE_discrepancy / SE
  )

model8 <- lm(VATgap ~ SE + SE_uncertainty + LogVATRev + LogGDPcap + 
               PRESS_F + PS, data = data)

# Model 9: Analiza rezidualelor pentru identificare supraestimare
# Folosim doar observațiile complete pentru a evita probleme de dimensiune
data_complete <- data[complete.cases(data[, c("VATgap", "SE", "LogVATRev", 
                                              "LogGDPcap", "PRESS_F", 
                                              "FinalConsExp", "DIF", "PS", "SR")]), ]

# Refacem modelul pe datele complete
model4_complete <- lm(VATgap ~ SE + LogVATRev + LogGDPcap + PRESS_F + 
                        FinalConsExp + DIF + PS + SR, data = data_complete)

residuals_df <- data_complete %>%
  mutate(
    predicted = predict(model4_complete),
    residual = residuals(model4_complete),
    abs_residual = abs(residual),
    outlier = ifelse(abs_residual > 1.5 * sd(residual, na.rm = TRUE), 1, 0)
  )

# Identificare țări cu reziduali mari
outlier_countries <- residuals_df %>%
  filter(outlier == 1) %>%
  group_by(country) %>%
  summarise(
    n_outliers = n(),
    mean_SE = mean(SE),
    mean_residual = mean(residual)
  ) %>%
  arrange(desc(n_outliers))

print("\n=== ȚĂRI CU REZIDUALI MARI (posibilă supraestimare SE) ===")
print(outlier_countries)

# 8. REGRESII PE SUBSAMPLES ===============================================

# Old EU countries
data_old <- data %>% filter(EU_status == "Old")
model_old <- lm(VATgap ~ SE + LogVATRev + LogGDPcap + PRESS_F + PS, 
                data = data_old)

# New EU countries
data_new <- data %>% filter(EU_status == "New")
model_new <- lm(VATgap ~ SE + LogVATRev + LogGDPcap + PRESS_F + PS, 
                data = data_new)

# 9. TABEL REZULTATE =======================================================

stargazer(model1, model2, model3, model4, model5, model6,
          type = "text",
          title = "Determinanții VAT Gap - Modele principale",
          column.labels = c("OLS1", "OLS2", "OLS3", "OLS4", "REM", "FEM"),
          dep.var.labels = "VAT Gap",
          covariate.labels = c("Shadow Economy", "Log(VAT Rev)", 
                               "Log(GDP per cap)", "Press Freedom",
                               "Final Consumption", "DIF", "Political Stability",
                               "Standard Rate"),
          omit.stat = c("ser", "f"),
          digits = 3)

stargazer(model4, model7, model8,
          type = "text",
          title = "Analiza supraestimării Shadow Economy",
          column.labels = c("Baseline", "Dummy overest.", "SE uncertainty"),
          dep.var.labels = "VAT Gap",
          digits = 3)

stargazer(model_old, model_new,
          type = "text",
          title = "Comparație Old vs New EU",
          column.labels = c("Old EU", "New EU"),
          dep.var.labels = "VAT Gap",
          digits = 3)

# 10. VIZUALIZĂRI =========================================================

# Plot 1: Scatter SE vs VAT gap
p1 <- ggplot(data, aes(x = SE, y = VATgap, color = EU_status)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Shadow Economy vs VAT Gap",
       x = "Shadow Economy (% GDP)",
       y = "VAT Gap (% VTTL)",
       color = "EU Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Scatter cu supraestimare
p2 <- ggplot(data, aes(x = SE, y = VATgap, 
                       color = factor(SE_overestimated))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "SE vs VAT Gap - Identificare supraestimare",
       x = "Shadow Economy (% GDP)",
       y = "VAT Gap (% VTTL)",
       color = "SE Overestimated") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("Normal", "Possible overestimation")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: Box plot VATgap pe țări
p3 <- data %>%
  group_by(country) %>%
  summarise(mean_VATgap = mean(VATgap, na.rm = TRUE),
            mean_SE = mean(SE, na.rm = TRUE)) %>%
  arrange(desc(mean_VATgap)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(country, mean_VATgap), y = mean_VATgap, fill = mean_SE)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(title = "Top 10 țări cu VAT Gap cel mai mare",
       x = NULL,
       y = "Mean VAT Gap",
       fill = "Mean SE") +
  theme_minimal()

# Plot 4: Evoluție temporală
p4 <- data %>%
  filter(country %in% c("Romania", "Bulgaria", "Germany", "Poland")) %>%
  ggplot(aes(x = year, y = VATgap, color = country)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Evoluția VAT Gap în timp",
       x = "Year",
       y = "VAT Gap",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plots
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Plot 5: Diagnostic plots pentru model4
par(mfrow = c(2, 2))
plot(model4)
par(mfrow = c(1, 1))

# Plot 6: Reziduali pe țări
residuals_plot <- residuals_df %>%
  ggplot(aes(x = reorder(country, abs_residual, FUN = median), 
             y = residual, fill = factor(outlier))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red")) +
  labs(title = "Distribuția rezidualelor pe țări",
       subtitle = "Roșu = Outliers (posibilă supraestimare SE)",
       x = NULL,
       y = "Reziduali",
       fill = "Outlier") +
  theme_minimal()

print(residuals_plot)

# 11. TESTE DIAGNOSTICE ===================================================

# Test multicolinearitate
vif_values <- vif(model4)
print("\n=== TEST VIF (Variance Inflation Factor) ===")
print(vif_values)

# Test heteroskedasticitate
bp_test <- bptest(model4)
print("\n=== BREUSCH-PAGAN TEST (Heteroskedasticitate) ===")
print(bp_test)

# Robust standard errors (dacă există heteroskedasticitate)
robust_se <- coeftest(model4, vcov = vcovHC(model4, type = "HC1"))
print("\n=== COEFICIENȚI CU STANDARD ERRORS ROBUSTI ===")
print(robust_se)

# 12. EXPORT REZULTATE ====================================================

# Export coeficienți
coefficients_df <- data.frame(
  Model = c(rep("Model4", length(coef(model4))),
            rep("Model7", length(coef(model7))),
            rep("Model8", length(coef(model8)))),
  Variable = c(names(coef(model4)), names(coef(model7)), names(coef(model8))),
  Coefficient = c(coef(model4), coef(model7), coef(model8)),
  Std_Error = c(summary(model4)$coefficients[,2],
                summary(model7)$coefficients[,2],
                summary(model8)$coefficients[,2]),
  P_value = c(summary(model4)$coefficients[,4],
              summary(model7)$coefficients[,4],
              summary(model8)$coefficients[,4])
)

# Export la Excel
# write_xlsx(coefficients_df, "rezultate_coeficienti.xlsx")
# write_xlsx(outlier_countries, "tari_outliers.xlsx")
# write_xlsx(stats_by_group, "statistici_grupuri.xlsx")

# 13. RAPORT FINAL ========================================================

cat("\n===============================================\n")
cat("REZUMAT ANALIZĂ\n")
cat("===============================================\n\n")

cat("1. PRINCIPALELE CONSTATĂRI:\n")
cat("   - Coeficient SE în model4:", round(coef(model4)["SE"], 4), "\n")
cat("   - R² model4:", round(summary(model4)$r.squared, 4), "\n")
cat("   - Test Hausman p-value:", round(hausman_test$p.value, 4), "\n")
cat("     (", ifelse(hausman_test$p.value < 0.05, "FEM preferat", "REM preferat"), ")\n\n")

cat("2. SUPRAESTIMARE SHADOW ECONOMY:\n")
cat("   - Coef. SE_overestimated:", round(coef(model7)["SE_overestimated"], 4), "\n")
cat("   - Țări identificate cu posibilă supraestimare:", 
    nrow(outlier_countries), "\n")
cat("   - Top 3 țări outliers:", 
    paste(head(outlier_countries$country, 3), collapse = ", "), "\n\n")

cat("3. DIFERENȚE OLD vs NEW EU:\n")
cat("   - Coef. SE (Old EU):", round(coef(model_old)["SE"], 4), "\n")
cat("   - Coef. SE (New EU):", round(coef(model_new)["SE"], 4), "\n")

cat("\n===============================================\n")
cat("Analiză completă!\n")
cat("Verifică plot-urile și tabelele generate.\n")
cat("===============================================\n")