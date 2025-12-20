library(tidyverse)
library(broom)
library(ggrepel) # Pentru etichete frumoase pe grafic

# ---------------------------------------------------------
# 1. ÎNCĂRCAREA DATELOR (Direct din textul tău)
# ---------------------------------------------------------
data_text <- "Country_Code,Country_Name,VAT_Gap,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,SAFT_Year,SAFT_2020,SAFT_2021,SAFT_2022,SAFT_2023
AT,Austria,1.0%,10.8,11.0,10.3,9.7,9.4,8.1,8.5,8.2,7.9,7.6,7.5,7.8,8.2,7.8,7.1,6.7,6.1,7.2,6.9,6.6,2009,1,1,1,1
BE,Belgium,12.3%,21.4,20.7,20.1,19.2,18.3,17.5,17.8,17.4,17.1,16.8,16.4,16.1,16.2,16.1,15.6,15.4,15.1,16.2,16.0,16.0,,0,0,0,0
BG,Bulgaria,8.6%,35.9,35.3,34.4,34.0,32.7,32.1,32.5,32.6,32.3,31.9,31.2,31.0,30.6,30.2,29.6,30.8,30.1,32.9,32.4,33.1,2026,0,0,0,0
HR,Croatia,7.7%,32.3,32.3,31.5,31.2,30.4,29.6,30.1,29.8,29.5,29.0,28.4,28.0,27.7,27.1,26.5,27.4,26.4,29.6,29.0,29.7,,0,0,0,0
CY,Cyprus,3.3%,28.7,28.3,28.1,27.9,26.5,26.0,26.5,26.2,26.0,25.6,25.2,25.7,24.8,24.2,23.6,23.2,22.1,24.3,23.7,23.9,,0,0,0,0
CZ,Czech Republic,8.0%,19.5,19.1,18.5,18.1,17.0,16.6,16.9,16.7,16.4,16.0,15.5,15.3,15.1,14.9,14.1,13.6,13.1,14.2,13.9,13.5,,0,0,0,0
DK,Denmark,8.9%,17.4,17.1,16.5,15.4,14.8,13.9,14.3,14.0,13.8,13.4,13.0,12.8,12.0,11.6,10.9,9.3,8.9,9.8,9.6,9.7,2024,0,0,0,0
EE,Estonia,10.3%,30.7,30.8,30.2,29.6,29.5,29.0,29.6,29.3,28.6,28.2,27.6,27.1,26.2,25.4,24.6,23.2,22.1,23.6,23.1,22.7,,0,0,0,0
FI,Finland,3.0%,17.6,17.2,16.6,15.3,14.5,13.8,14.2,14.0,13.7,13.3,13.0,12.9,12.4,12.0,11.5,11.0,10.6,11.4,10.9,10.8,,0,0,0,0
FR,France,5.6%,14.7,14.3,13.8,12.4,11.8,11.1,11.6,11.3,11.0,10.8,9.9,10.8,12.3,12.6,12.8,12.5,12.4,13.6,13.1,14.2,2014,1,1,1,1
DE,Germany,9.7%,16.7,15.7,15.0,14.5,13.9,13.5,14.3,13.5,12.7,12.5,12.1,11.6,11.2,10.8,10.4,9.7,8.5,10.4,10.0,8.8,,0,0,0,0
EL,Greece,11.4%,28.2,28.1,27.6,26.2,25.1,24.3,25.0,25.4,24.3,24.0,23.6,23.3,22.4,22.0,21.5,20.8,19.2,20.9,20.3,20.93,,0,0,0,0
HU,Hungary,7.4%,25.0,24.7,24.5,24.4,23.7,23.0,23.5,23.3,22.8,22.5,22.1,21.6,21.9,22.2,22.4,22.7,23.2,26.0,25.0,25.4,,0,0,0,0
IE,Ireland,8.3%,15.4,15.2,14.8,13.4,12.7,12.2,13.1,13.0,12.8,12.7,12.2,11.8,11.3,10.8,10.4,9.7,8.9,9.9,9.4,10.1,,0,0,0,0
IT,Italy,15.0%,26.1,25.2,24.4,23.2,22.3,21.4,22.0,21.8,21.2,21.6,21.1,20.8,20.6,20.2,19.8,19.5,18.7,20.4,20.2,20.3,,0,0,0,0
LV,Latvia,5.4%,30.4,30.0,29.5,29.0,27.5,26.5,27.1,27.3,26.5,26.1,25.5,24.7,23.6,22.9,21.3,20.2,19.8,20.9,20.2,19.9,,0,0,0,0
LT,Lithuania,15.1%,32.0,31.7,31.1,30.6,29.7,29.1,29.6,29.7,29.0,28.5,28.0,27.1,25.8,24.9,23.8,23.0,21.9,23.1,22.9,22.4,2019,1,1,1,1
LU,Luxembourg,0.18%,9.8,9.8,9.9,10.0,9.4,8.5,8.8,8.4,8.2,8.2,8.0,8.1,8.3,8.4,8.2,7.9,7.4,8.6,8.4,8.3,2011,1,1,1,1
MT,Malta,24.2%,26.7,26.7,26.9,27.2,26.4,25.8,25.9,26.0,25.8,25.3,24.3,24.0,24.3,24.0,23.6,23.2,22.0,23.5,23.1,23.4,,0,0,0,0
NL,Netherlands,7.0%,12.7,12.5,12.0,10.9,10.1,9.6,10.2,10.0,9.8,9.5,9.1,9.2,9.0,8.8,8.4,7.5,7.0,8.1,7.8,8.2,,0,0,0,0
PL,Poland,16.0%,27.7,27.4,27.1,26.8,26.0,25.3,25.9,25.4,25.0,24.4,23.8,23.5,23.3,23.0,22.2,21.7,20.7,22.5,22.0,21.9,2016,1,1,1,1
PT,Portugal,3.6%,22.2,21.7,21.2,20.1,19.2,18.7,19.5,19.2,19.4,19.4,19.0,18.7,17.6,17.2,16.6,16.1,15.4,17.0,16.5,15.7,2009,1,1,1,1
RO,Romania,30.0%,33.6,32.5,32.2,31.4,30.2,29.4,29.4,29.8,29.6,29.1,28.4,28.1,28.0,27.6,26.3,26.7,26.9,29.3,28.9,29.0,2022,0,0,1,1
SI,Slovenia,4.9%,26.7,26.5,26.0,25.8,24.7,24.0,24.6,24.3,24.1,23.6,23.1,23.5,23.3,23.1,22.4,22.2,21.5,23.1,22.5,22.1,,0,0,0,0
ES,Spain,7.6%,22.2,21.9,21.3,20.2,19.3,18.4,19.5,19.4,19.2,19.2,18.6,18.5,18.2,17.9,17.2,16.6,15.4,17.4,16.9,15.8,,0,0,0,0
SK,Slovakia,10.5%,18.4,18.2,17.6,17.3,16.8,16.0,16.8,16.4,16.0,15.5,15.0,14.6,14.1,13.7,13.0,12.8,12.2,14.0,13.7,13.1,,0,0,0,0
SE,Sweden,5.3%,18.6,18.1,17.5,16.2,15.6,14.9,15.4,15.0,14.7,14.3,13.9,13.6,13.2,12.6,12.1,11.6,10.7,11.7,11.0,10.8,,0,0,0,0"

df <- read.csv(text = data_text)

# ---------------------------------------------------------
# 2. PREGĂTIREA DATELOR (ENGINEERING)
# ---------------------------------------------------------
# Setăm anul de analiză. Deși VAT Gap-ul pare să fie cel mai recent (2021/2022),
# folosim 2023 ca referință pentru maturitatea SAF-T, conform logicii anterioare.

analysis_year <- 2023
lag_threshold <- 5 # Pragul de maturitate identificat în graficul anterior

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
# 2. BUCLA DE TESTARE (GRID SEARCH) - CORECTATĂ
# ---------------------------------------------------------

# Reinițializăm grid_results
grid_results <- data.frame()

for (s_year in shadow_years_to_test) {
  
  # CORECTURĂ: Folosim as.character pentru că ai șters "X"-ul din nume
  col_name <- as.character(s_year)
  
  # Verificăm dacă coloana există
  if(col_name %in% names(df)) {
    
    for (k_lag in saft_lags_to_test) {
      
      # Construim setul de date temporar
      # Folosim backticks `` pentru numele de coloane numerice (ex: `2022`)
      df_iter <- df %>%
        mutate(
          VAT_Gap_Target = as.numeric(gsub("%", "", VAT_Gap)),
          Shadow_Predictor = .data[[col_name]], # Accesăm sigur coloana numerică
          
          # Calculăm dummy-ul SAF-T
          SAFT_Year_Clean = ifelse(is.na(SAFT_Year) | SAFT_Year > analysis_year, NA, SAFT_Year),
          Years_Experience = ifelse(is.na(SAFT_Year_Clean), 0, analysis_year - SAFT_Year_Clean),
          SAFT_Dummy = ifelse(Years_Experience >= k_lag & Years_Experience > 0, 1, 0)
        ) %>%
        filter(!is.na(Shadow_Predictor), !is.na(VAT_Gap_Target))
      
      # Verificăm dacă avem suficiente țări (minim 3)
      if (sum(df_iter$SAFT_Dummy) >= 3) {
        
        model <- lm(VAT_Gap_Target ~ Shadow_Predictor + SAFT_Dummy, data = df_iter)
        
        tidied <- tidy(model)
        glanced <- glance(model)
        
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

# Verificăm întâi dacă avem rezultate
if(nrow(grid_results) > 0) {
  best_models <- grid_results %>% 
    arrange(desc(Adj_R2)) %>%
    head(10)
  
  print("TOP 10 Combinații (Shadow Year + SAFT Lag):")
  print(best_models)
} else {
  print("Nu s-au generat rezultate. Verifică numele coloanelor din df.")
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