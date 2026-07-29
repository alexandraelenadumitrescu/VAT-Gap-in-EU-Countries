library(tidyverse)
library(broom)
library(ggrepel) # Pentru etichete frumoase pe grafic

# ---------------------------------------------------------
# 1. ÎNCĂRCAREA DATELOR (Direct din textul tău)
# ---------------------------------------------------------

df <- read.csv("stabil_lag_ambele_se_saft.csv")

# ---------------------------------------------------------
# 2. PREGĂTIREA DATELOR (ENGINEERING)
# ---------------------------------------------------------
# Setăm anul de analiză. Deși VAT Gap-ul pare să fie cel mai recent (2021/2022),
# folosim 2023 ca referință pentru maturitatea SAF-T, conform logicii anterioare.

analysis_year <- 2023
lag_threshold <- 3 # Pragul de maturitate identificat în graficul anterior

df_clean <- df %>%
  mutate(
    # 1. Variabila Dependentă: VAT Gap (numeric)
    VAT_Gap_Num = as.numeric(gsub("%", "", VAT_Gap)),
    
    # 2. Variabila Independentă 1: Shadow Economy (Folosim coloana 'X2022' ca proxy curent)
    # R adaugă un 'X' în fața numerelor când citește coloane.
    Shadow_Economy = X2022, 
    
    # 3. Calculăm Vechimea SAF-T
    SAFT_Year_Clean = ifelse(is.na(SAFT_Year) | SAFT_Year > analysis_year, NA, SAFT_Year),
    Years_Since = ifelse(is.na(SAFT_Year_Clean), 0, analysis_year - SAFT_Year_Clean),
    
    # 4. Variabila Independentă 2: DUMMY MATUR (1 dacă are >= 5 ani, 0 altfel)
    # Acesta este elementul cheie "cu lag"
    SAFT_Mature_Dummy = ifelse(Years_Since >= lag_threshold, 1, 0),
    
    # Etichetă pentru grafic
    Status = ifelse(SAFT_Mature_Dummy == 1, "SAF-T Matur (>5 ani)", "Fără SAF-T / Recent")
  )

# ---------------------------------------------------------
# 3. REGRESIA MULTIVARIATĂ
# ---------------------------------------------------------

# Model: VAT_Gap = Intercept + b1 * Shadow_Economy + b2 * SAFT_Mature_Dummy
model <- lm(VAT_Gap_Num ~ Shadow_Economy + SAFT_Mature_Dummy, data = df_clean)

# Afișare rezultate statistice
print(summary(model))
print(tidy(model))

# ---------------------------------------------------------
# 4. VIZUALIZARE (SCATTER PLOT CU DREPTE PARALELE)
# ---------------------------------------------------------
# Extragem coeficienții pentru a desena liniile manual sau lăsăm ggplot să facă treaba
# Vom folosi metoda simplă cu geom_smooth pe grupuri, forțând pante paralele prin modelul vizualizat

# Creăm predicțiile modelului pentru a desena liniile corecte (pante paralele)
df_clean$Predicted <- predict(model)

ggplot(df_clean, aes(x = Shadow_Economy, y = VAT_Gap_Num, color = Status)) +
  # Punctele (Țările)
  geom_point(size = 3, alpha = 0.8) +
  
  # Adăugăm codurile țărilor
  geom_text_repel(aes(label = Country_Code), size = 3.5, show.legend = FALSE) +
  
  # Liniile de regresie (bazate pe modelul nostru multivariat)
  geom_line(aes(y = Predicted, group = Status), size = 1.2) +
  
  scale_color_manual(values = c("gray50", "darkblue")) +
  
  labs(
    title = "Impactul SAF-T Matur asupra VAT Gap (controlat pt. Economia Subterană)",
    subtitle = "Țările cu SAF-T Matur (>5 ani) colectează mai bine TVA la același nivel de evaziune generală",
    x = "Economie Subterană (% din PIB, 2022)",
    y = "VAT Gap (%)",
    caption = "Sursa date: Comisia Europeană. Model liniar multivariat."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")










library(tidyverse)
library(broom)
library(scales)

# ---------------------------------------------------------
# 1. PREGĂTIREA DATELOR
# ---------------------------------------------------------
# Presupunem că 'df' este deja încărcat din pasul anterior.
# Dacă nu, rulează din nou read.csv cu datele tale.

analysis_year <- 2023

# Definim intervalele pe care le testăm
# 1. Anii pentru Shadow Economy (sau variabila independentă istorică)
shadow_years_to_test <- 2015:2022 

# 2. Lagg-urile pentru SAF-T (ani de maturitate)
saft_lags_to_test <- 0:8

# Creăm un data frame gol pentru rezultate
grid_results <- data.frame()

# ---------------------------------------------------------
# 2. BUCLA DE TESTARE (GRID SEARCH)
# ---------------------------------------------------------

for (s_year in shadow_years_to_test) {
  
  # Selectăm coloana dinamic (ex: X2018, X2019...)
  col_name <- paste0("X", s_year)
  
  # Verificăm dacă coloana există
  if(col_name %in% names(df)) {
    
    for (k_lag in saft_lags_to_test) {
      
      # Construim setul de date temporar pentru această iterație
      df_iter <- df %>%
        mutate(
          VAT_Gap_Target = as.numeric(gsub("%", "", VAT_Gap)), # Variabila Dependentă (2023)
          Shadow_Predictor = get(col_name),                    # Variabila Independentă 1 (Shadow din anul s_year)
          
          # Calculăm dummy-ul SAF-T bazat pe lag-ul k_lag
          SAFT_Year_Clean = ifelse(is.na(SAFT_Year) | SAFT_Year > analysis_year, NA, SAFT_Year),
          Years_Experience = ifelse(is.na(SAFT_Year_Clean), 0, analysis_year - SAFT_Year_Clean),
          SAFT_Dummy = ifelse(Years_Experience >= k_lag & Years_Experience > 0, 1, 0)
        ) %>%
        # Curățăm NA-urile care ar putea apărea
        filter(!is.na(Shadow_Predictor), !is.na(VAT_Gap_Target))
      
      # Verificăm dacă avem suficiente țări cu SAF-T (minim 3) pentru a nu da eroare
      if (sum(df_iter$SAFT_Dummy) >= 3) {
        
        # Rulăm modelul: VAT_Gap ~ Shadow(Anul X) + SAF-T(Vechime K)
        model <- lm(VAT_Gap_Target ~ Shadow_Predictor + SAFT_Dummy, data = df_iter)
        
        # Extragem parametrii
        tidied <- tidy(model)
        glanced <- glance(model) # Pentru R2
        
        saft_row <- tidied %>% filter(term == "SAFT_Dummy")
        shadow_row <- tidied %>% filter(term == "Shadow_Predictor")
        
        if(nrow(saft_row) > 0) {
          grid_results <- rbind(grid_results, data.frame(
            Shadow_Year = s_year,
            SAFT_Lag_Threshold = k_lag,
            Adj_R2 = glanced$adj.r.squared,
            SAFT_Coef = saft_row$estimate,
            SAFT_P_Value = saft_row$p.value,
            Shadow_Coef = shadow_row$estimate
          ))
        }
      }
    }
  }
}

# ---------------------------------------------------------
# 3. IDENTIFICAREA CELUI MAI BUN MODEL
# ---------------------------------------------------------

# Sortăm descrescător după R2 (puterea explicativă)
best_models <- grid_results %>% 
  arrange(desc(Adj_R2)) %>%
  head(10)

print("TOP 10 Combinații (Shadow Year + SAFT Lag):")
print(best_models)

# ---------------------------------------------------------
# 4. VIZUALIZARE HEATMAP (HARTA "CALDĂ")
# ---------------------------------------------------------

ggplot(grid_results, aes(x = factor(Shadow_Year), y = factor(SAFT_Lag_Threshold), fill = Adj_R2)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Adjusted R²") +
  geom_text(aes(label = round(Adj_R2, 2)), color = "white", size = 3) +
  labs(
    title = "Optimizarea Modelului: Shadow Economy vs. SAF-T Lag",
    subtitle = "Culorile mai deschise/calde indică modele mai performante (R² mai mare)",
    x = "Anul datelor de Shadow Economy (Predictor)",
    y = "Pragul minim de vechime SAF-T (Ani)"
  ) +
  theme_minimal()



library(tidyverse)
library(broom)

# ---------------------------------------------------------
# 1. SETUP
# ---------------------------------------------------------
# Ensure 'df' is loaded. If not, re-run your data loading step.

analysis_year <- 2023
lags_to_test <- 0:10 # Testing lags from 0 to 10 years

univar_results <- data.frame()

# ---------------------------------------------------------
# 2. UNIVARIATE LOOP
# ---------------------------------------------------------

for (k in lags_to_test) {
  
  # Prepare data for this specific lag
  df_iter <- df %>%
    mutate(
      # Target Variable
      Y = as.numeric(gsub("%", "", VAT_Gap)),
      
      # Independent Variable: SAFT Dummy with lag k
      SAFT_Clean = ifelse(is.na(SAFT_Year) | SAFT_Year > analysis_year, NA, SAFT_Year),
      Years_Exp  = ifelse(is.na(SAFT_Clean), 0, analysis_year - SAFT_Clean),
      SAFT_Dummy = ifelse(Years_Exp >= k & Years_Exp > 0, 1, 0)
    ) %>%
    filter(!is.na(Y))
  
  # Check for sufficient sample size (at least 3 treated countries)
  if (sum(df_iter$SAFT_Dummy, na.rm = TRUE) >= 3) {
    
    # --- RUN SINGLE VARIABLE REGRESSION ---
    model <- lm(Y ~ SAFT_Dummy, data = df_iter)
    
    # Extract stats
    tidied <- tidy(model)
    glanced <- glance(model)
    
    # Get the SAFT coefficient row
    saft_row <- tidied %>% filter(term == "SAFT_Dummy")
    
    if(nrow(saft_row) > 0) {
      univar_results <- rbind(univar_results, data.frame(
        Lag_Threshold = k,
        Coefficient   = saft_row$estimate,
        P_Value       = saft_row$p.value,
        R_Squared     = glanced$r.squared,
        Conf_Low      = saft_row$estimate - 1.96 * saft_row$std.error,
        Conf_High     = saft_row$estimate + 1.96 * saft_row$std.error
      ))
    }
  }
}

# ---------------------------------------------------------
# 3. VIEW RESULTS
# ---------------------------------------------------------

print("Univariate Regression Results (VAT Gap ~ SAF-T):")
print(univar_results)

# ---------------------------------------------------------
# 4. VISUALIZATION (Coefficient Plot)
# ---------------------------------------------------------

ggplot(univar_results, aes(x = Lag_Threshold, y = Coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  
  # Confidence Intervals (Ribbon)
  geom_ribbon(aes(ymin = Conf_Low, ymax = Conf_High), fill = "blue", alpha = 0.15) +
  
  # The Line and Points
  geom_line(color = "darkblue", size = 1) +
  geom_point(size = 3, color = "darkblue") +
  
  # Labels
  scale_x_continuous(breaks = lags_to_test) +
  labs(
    title = "Univariate Analysis: Effect of SAF-T Maturity on VAT Gap",
    subtitle = "Model: VAT Gap ~ SAFT_Dummy (No controls)",
    y = "Estimated Impact (pp)",
    x = "Minimum Years of SAF-T Experience"
  ) +
  theme_minimal()



























library(tidyverse)
library(car) # Asigură-te că ai pachetul 'car' instalat pentru VIF

# ---------------------------------------------------------
# 1. SETĂM SCENARIUL ȘI GĂSIM NUMELE CORECT AL COLOANEI
# ---------------------------------------------------------
target_shadow_year <- 2022
target_saft_lag    <- 5

# Funcție simplă de detecție a numelui
detect_column_name <- function(df, year) {
  name_with_x <- paste0("X", year)    # Varianta standard R: "X2022"
  name_simple <- as.character(year)   # Varianta check.names=F: "2022"
  
  if (name_with_x %in% names(df)) {
    return(name_with_x)
  } else if (name_simple %in% names(df)) {
    return(name_simple)
  } else {
    stop(paste("Nu găsesc coloana pentru anul", year, "în df! Verifică names(df)."))
  }
}

# Găsim numele corect (X2022 sau 2022)
col_shadow_name <- detect_column_name(df, target_shadow_year)
print(paste("Folosim coloana:", col_shadow_name))

# ---------------------------------------------------------
# 2. PREGĂTIM DATELE PENTRU MODEL
# ---------------------------------------------------------

df_model <- df %>%
  mutate(
    # Variabila Dependentă
    Y = as.numeric(gsub("%", "", VAT_Gap)),
    
    # Variabila Independentă (Shadow Economy) selectată dinamic
    X_Shadow = .data[[col_shadow_name]],
    
    # SAF-T Dummy
    SAFT_Clean = ifelse(is.na(SAFT_Year) | SAFT_Year > 2023, NA, SAFT_Year),
    Years_Exp  = ifelse(is.na(SAFT_Clean), 0, 2023 - SAFT_Clean),
    X_SAFT_Dummy = ifelse(Years_Exp >= target_saft_lag & Years_Exp > 0, 1, 0)
  ) %>%
  # Păstrăm doar rândurile complete
  filter(!is.na(Y), !is.na(X_Shadow)) %>%
  select(Country_Name, Y, X_Shadow, X_SAFT_Dummy)

# ---------------------------------------------------------
# 3. TESTUL 1: MATRICEA DE CORELAȚIE
# ---------------------------------------------------------
print("--- TEST 1: Corelația Pearson (Shadow vs. SAF-T) ---")

cor_val <- cor(df_model$X_Shadow, df_model$X_SAFT_Dummy)
print(paste("Coeficient de corelație (r):", round(cor_val, 4)))

if(abs(cor_val) > 0.7) {
  print("ALERTA: Risc mare de multicoliniaritate (Corelație > 0.7).")
} else {
  print("OK: Corelația este acceptabilă.")
}

# ---------------------------------------------------------
# 4. TESTUL 2: VIF (Variance Inflation Factor)
# ---------------------------------------------------------
print("--- TEST 2: VIF (Variance Inflation Factor) ---")

# Rulăm modelul auxiliar pentru VIF
model_vif <- lm(Y ~ X_Shadow + X_SAFT_Dummy, data = df_model)

# Verificăm VIF
# Notă: Dacă ai doar 2 variabile, VIF-ul va fi identic pentru ambele.
vif_vals <- vif(model_vif)
print(vif_vals)

if(max(vif_vals) > 5) {
  print("ALERTA CRITICĂ: VIF > 5. Rezultatele sunt instabile.")
} else if (max(vif_vals) > 2.5) {
  print("ATENȚIE: VIF moderat (> 2.5). Precizia este redusă.")
} else {
  print("PERFECT: VIF < 2.5. Nu există probleme tehnice de multicoliniaritate.")
}

# ---------------------------------------------------------
# 5. VIZUALIZARE
# ---------------------------------------------------------
ggplot(df_model, aes(x = factor(X_SAFT_Dummy), y = X_Shadow)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, color = "darkblue") +
  labs(
    title = "Verificare Multicoliniaritate",
    subtitle = "Au țările cu SAF-T o economie subterană diferită structural?",
    x = "Status SAF-T (0 = Nu, 1 = Da)",
    y = paste("Shadow Economy", target_shadow_year)
  ) +
  theme_minimal()