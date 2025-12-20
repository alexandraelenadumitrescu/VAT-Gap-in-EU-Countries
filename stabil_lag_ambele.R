library(tidyverse)
library(broom)
library(ggplot2)

# ---------------------------------------------------------
# 1. ÎNCARCĂ DATELE
# ---------------------------------------------------------
# Înlocuiește cu calea reală către fișierul tău CSV
file_path <- "stabil_lag_ambele_se_saft.csv" 
df <- read.csv(file_path)

# ---------------------------------------------------------
# 2. PREGĂTIREA DATELOR (pentru anul 2023)
# ---------------------------------------------------------

df_analysis <- df %>%
  mutate(
    # 1. Curățăm VAT Gap (scoatem %)
    VAT_Gap_2023 = as.numeric(gsub("%", "", VAT_Gap)),
    
    # 2. Gestionăm anii de implementare
    # Dacă anul e gol sau în viitor (ex: 2024, 2026), considerăm că NU au SAF-T în 2023
    SAFT_Year_Clean = ifelse(is.na(SAFT_Year) | SAFT_Year > 2023, NA, SAFT_Year),
    
    # 3. Calculăm "Lag-ul" (Vechimea în ani până în 2023)
    # Ex: 2023 - 2022 = 1 an vechime
    Years_Since_SAFT = ifelse(is.na(SAFT_Year_Clean), 0, 2023 - SAFT_Year_Clean)
  )

# Verificare rapidă a datelor calculate
print(df_analysis %>% select(Country_Name, SAFT_Year, Years_Since_SAFT, VAT_Gap_2023))

# ---------------------------------------------------------
# 3. BUCLA DE TESTARE A STABILITĂȚII (Robustness Loop)
# ---------------------------------------------------------

results_store <- data.frame()

# Testăm ipoteza pentru praguri de la 0 ani (efect imediat) până la 10 ani (efect pe termen lung)
max_years_test <- 10 

for (k in 0:max_years_test) {
  
  # Definim grupul de tratament: 
  # Țările care au SAF-T de CEL PUȚIN k ani
  df_analysis$Treated_Dummy <- ifelse(df_analysis$Years_Since_SAFT >= k & df_analysis$Years_Since_SAFT > 0, 1, 0)
  
  # Verificăm dacă avem suficiente țări în grupul tratat pentru a rula regresia
  # (Trebuie să fie măcar 2-3 țări pentru a avea sens statistic)
  if (sum(df_analysis$Treated_Dummy) >= 3) {
    
    # Rulăm Regresia Liniară Simplă
    # Model: VAT_Gap_2023 ~ Avem_SAFT_de_k_ani
    model <- lm(VAT_Gap_2023 ~ Treated_Dummy, data = df_analysis)
    
    # Extragem rezultatele
    tidied <- tidy(model)
    coef_row <- tidied %>% filter(term == "Treated_Dummy")
    
    if(nrow(coef_row) > 0) {
      results_store <- rbind(results_store, data.frame(
        Min_Years_Experience = k,
        Coefficient = coef_row$estimate,
        P_Value = coef_row$p.value,
        Low_CI = coef_row$estimate - 1.96 * coef_row$std.error,
        High_CI = coef_row$estimate + 1.96 * coef_row$std.error
      ))
    }
  }
}

# ---------------------------------------------------------
# 4. VIZUALIZARE (Coefficient Plot)
# ---------------------------------------------------------

print(results_store)

ggplot(results_store, aes(x = Min_Years_Experience, y = Coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Linia de nul
  geom_line(color = "darkblue", size = 1) +
  geom_point(size = 3, color = "darkblue") +
  geom_ribbon(aes(ymin = Low_CI, ymax = High_CI), alpha = 0.2, fill = "blue") +
  scale_x_continuous(breaks = 0:10, name = "Vechime minimă a SAF-T (Ani)") +
  labs(
    title = "Stabilitatea Efectului SAF-T asupra VAT Gap (2023)",
    subtitle = "Coeficientul estimat în funcție de vechimea sistemului",
    y = "Diferența estimată de VAT Gap (pp)",
    caption = "Nota: Banda umbrită reprezintă intervalul de încredere 95%."
  ) +
  theme_minimal()