# Asigura-te ca ai biblioteca incarcata
library(broom) # Pentru un tabel de sinteza frumos (optional, dar util)

# 0. Ne asiguram ca avem datele doar pentru 2022
# (Presupunem ca ai rulat deja pasii anteriori de incarcare)
df_2022 <- df_full %>% 
  filter(Year == 2022)

# -------------------------------------------------------------------------
# PASUL 1: Modelul Simplu (H1)
# Verificam corelatia bruta: Evaziune vs Economie Subterana
# -------------------------------------------------------------------------
cs_model_1 <- lm(Value ~ ShadowEconomy, data = df_2022)

print("--- MODEL 1 (Cross-Section 2022): Doar Shadow Economy ---")
summary(cs_model_1)

#grafic basic

plot(df_2022$ShadowEconomy, df_2022$Value,
     main = "Relatia Economie Subterana vs Decalaj TVA (2022)",
     xlab = "Economie Subterana (% PIB)",
     ylab = "Decalaj TVA",
     pch = 19,       # Forma punctului (cerc plin)
     col = "blue")   # Culoarea punctelor
abline(cs_model_1,col = "red", lwd = 2)

#grafic regresie

library(ggplot2)

ggplot(df_2022, aes(x = ShadowEconomy, y = Value)) +
  # Adaugam punctele
  geom_point(color = "darkblue", size = 3) +
  
  # Adaugam linia de regresie automata
  # method = "lm" inseamna Linear Model (acelasi lucru cu lm() din spate)
  geom_smooth(method = "lm", color = "red", fill = "gray80") +
  
  # Adaugam etichetele tarilor (util pentru analiza outlierilor)
  geom_text(aes(label = Country), vjust = -0.8, size = 3, check_overlap = TRUE) +
  
  # Etichete si tema curata
  labs(title = "Model 1: Impactul Economiei Subterane asupra TVA",
       subtitle = paste("R-squared =", round(summary(cs_model_1)$r.squared, 3)),
       x = "Economie Subterana (Shadow Economy)",
       y = "VAT Gap (Value)") +
  theme_minimal()


#grafic residuals
library(ggplot2)

# 1. Extragem reziduurile in dataframe
df_2022$reziduuri <- residuals(cs_model_1)

# 2. Histograma cu Curba Normala
ggplot(df_2022, aes(x = reziduuri)) +
  # Histograma (barele albastre)
  geom_histogram(aes(y = after_stat(density)), bins = 10, 
                 fill = "skyblue", color = "black", alpha = 0.7) +
  
  # Curba teoretica normala (linia rosie)
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_2022$reziduuri), 
                            sd = sd(df_2022$reziduuri)), 
                color = "red", linewidth = 1) +
  
  # Estetica
  labs(title = "Distribuția Reziduurilor (Model 1)",
       subtitle = "Verificarea ipotezei de normalitate",
       x = "Valoare Reziduu",
       y = "Densitate") +
  theme_minimal()


#grafic residuals vs fitted


# 1. Extragem valorile prezise (Fitted Values)
df_2022$prezise <- fitted(cs_model_1)

# 2. Graficul Scatter
ggplot(df_2022, aes(x = prezise, y = reziduuri)) +
  geom_point(color = "darkblue", size = 3) +
  
  # Linia de zero (unde ar trebui sa fie media erorilor)
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Etichete pentru tarile cu erori mari
  geom_text(aes(label = ifelse(abs(reziduuri) > 0.1, Country, "")), 
            vjust = -0.5, color = "red", size = 3) +
  
  labs(title = "Residuals vs Fitted Values",
       subtitle = "Verificarea homoscedasticității (erori constante)",
       x = "Valoare Prezisa de Model (VAT Gap)",
       y = "Eroare (Reziduu)") +
  theme_minimal()


#observam o palnie dubla
library(lmtest)

# Testul Breusch-Pagan pentru Heteroscedasticitate
bptest(cs_model_1)#p val=0.01<0.05=>heteroscedasticitate=>recalculam erorile standard metoda sandwich - erori robuste la heteroscedasticitate
# Instalare pachete necesare (daca nu le ai)
install.packages(c("sandwich", "lmtest"))

library(sandwich)
library(lmtest)

# 1. Modelul Original (pentru comparatie)
print("--- MODEL STANDARD (GRESIT la t-test) ---")
summary(cs_model_1)

# 2. Modelul Corectat (Robust Standard Errors)
# Folosim vcovHC cu tipul "HC1" (standard in econometrie)
model_robust <- coeftest(cs_model_1, vcov = vcovHC(cs_model_1, type = "HC1"))

print("--- MODEL ROBUST (CORECT la t-test) ---")
print(model_robust)

#alternativ logaritmam

# 1. Cream un model Log-Lin (Logaritmam Y)
# Nota: Value trebuie sa fie > 0. Daca ai 0 sau negativ, nu merge log.
model_log <- lm(log(Value) ~ ShadowEconomy, data = df_2022)

# 2. Vedem rezumatul
summary(model_log)

# 3. Verificam vizual daca a disparut "papionul"
par(mfrow = c(2, 2))
plot(model_log)
par(mfrow = c(1, 1))

# 4. Testul suprem: Breusch-Pagan
library(lmtest)
bptest(model_log)




library(lmtest)
library(ggplot2)

# 1. Modelul Simplu LINIAR (Cel cu probleme)
# Value = b0 + b1 * ShadowEconomy
m1_linear <- lm(Value ~ ShadowEconomy, data = df_2022)

# 2. Modelul Simplu LOGARITMAT (Log-Lin)
# log(Value) = b0 + b1 * ShadowEconomy
# Nota: Folosim log() care in R inseamna logaritm natural (ln)
m1_log <- lm(log(Value) ~ ShadowEconomy, data = df_2022)

# 3. Testam "Papionul" (Heteroscedasticitatea)
# H0: Erori constante (Homoscedasticitate) -> Vrem p-value > 0.05
bp_linear <- bptest(m1_linear)
bp_log    <- bptest(m1_log)

print("--- REZULTATE TEST BREUSCH-PAGAN ---")
print(paste("Model Liniar p-value:", round(bp_linear$p.value, 5)))
print(paste("Model Logaritmat p-value:", round(bp_log$p.value, 5)))

# 4. Vedem Coeficientii noului model
summary(m1_log)

# Desenam graficul Residuals vs Fitted pentru modelul Logaritmat
plot(m1_log, which = 1) 
# which=1 inseamna primul grafic de diagnostic (Residuals vs Fitted)

# -------------------------------------------------------------------------
# PASUL 2: Adăugăm Corupția (CPI_Score)
# Controlam pentru calitatea institutiilor
# -------------------------------------------------------------------------
cs_model_2 <- lm(Value ~ ShadowEconomy + CPI_Score, data = df_2022)

print("--- MODEL 2 (Cross-Section 2022): + CPI Score ---")
summary(cs_model_2)
abline(cs_model_2)

# -------------------------------------------------------------------------
# PASUL 3: Adăugăm Consumul Final
# Verificam baza de impozitare
# -------------------------------------------------------------------------
cs_model_3 <- lm(Value ~ ShadowEconomy + CPI_Score + FinalConsumption, data = df_2022)

print("--- MODEL 3 (Cross-Section 2022): + Consum Final ---")
summary(cs_model_3)

# -------------------------------------------------------------------------
# PASUL 4: Adăugăm Rata Șomajului
# Verificam presiunea pietei muncii
# -------------------------------------------------------------------------
cs_model_4 <- lm(Value ~ ShadowEconomy + CPI_Score + FinalConsumption + Unemployment_rate, data = df_2022)

print("--- MODEL 4 (Cross-Section 2022): + Somaj ---")
summary(cs_model_4)

# -------------------------------------------------------------------------
# COMPARATIE FINALA (Adjusted R-squared)
# -------------------------------------------------------------------------
# Colectam R2 ajustat pentru a vedea care model explica cel mai bine realitatea
# fara sa "triseze" prin adaugarea de variabile inutile
adj_r2_vals <- c(summary(cs_model_1)$adj.r.squared, 
                 summary(cs_model_2)$adj.r.squared, 
                 summary(cs_model_3)$adj.r.squared,
                 summary(cs_model_4)$adj.r.squared)

names(adj_r2_vals) <- c("M1(Shadow)", "M2(+CPI)", "M3(+Consum)", "M4(+Somaj)")

print("Evolutia Adjusted R-squared (2022):")
print(adj_r2_vals)