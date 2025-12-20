library(tidyverse)
library(plm)      # For Panel Data analysis
library(lmtest)   # For robust standard errors
library(sandwich) # For robust covariance matrix

# ---------------------------------------------------------
# 1. DATA PREPARATION
# ---------------------------------------------------------
# I will reconstruct the dataframe based on the merged CSV logic we discussed.
# In your real script, just load your actual CSV using: df <- read.csv("your_file.csv")

data_csv <- "Country_Code,Country_Name,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,SAFT_Start_Year
AT,Austria,10.8,11,10.3,9.7,9.4,8.1,8.5,8.2,7.9,7.6,7.5,7.8,8.2,7.8,7.1,6.7,6.1,7.2,6.9,6.6,2009
BE,Belgium,21.4,20.7,20.1,19.2,18.3,17.5,17.8,17.4,17.1,16.8,16.4,16.1,16.2,16.1,15.6,15.4,15.1,16.2,16,16,NA
BG,Bulgaria,35.9,35.3,34.4,34,32.7,32.1,32.5,32.6,32.3,31.9,31.2,31,30.6,30.2,29.6,30.8,30.1,32.9,32.4,33.1,2026
HR,Croatia,32.3,32.3,31.5,31.2,30.4,29.6,30.1,29.8,29.5,29,28.4,28,27.7,27.1,26.5,27.4,26.4,29.6,29,29.7,NA
CY,Cyprus,28.7,28.3,28.1,27.9,26.5,26,26.5,26.2,26,25.6,25.2,25.7,24.8,24.2,23.6,23.2,22.1,24.3,23.7,23.9,NA
CZ,Czech Republic,19.5,19.1,18.5,18.1,17,16.6,16.9,16.7,16.4,16,15.5,15.3,15.1,14.9,14.1,13.6,13.1,14.2,13.9,13.5,NA
DK,Denmark,17.4,17.1,16.5,15.4,14.8,13.9,14.3,14,13.8,13.4,13,12.8,12,11.6,10.9,9.3,8.9,9.8,9.6,9.7,2024
EE,Estonia,30.7,30.8,30.2,29.6,29.5,29,29.6,29.3,28.6,28.2,27.6,27.1,26.2,25.4,24.6,23.2,22.1,23.6,23.1,22.7,NA
FI,Finland,17.6,17.2,16.6,15.3,14.5,13.8,14.2,14,13.7,13.3,13,12.9,12.4,12,11.5,11,10.6,11.4,10.9,10.8,NA
FR,France,14.7,14.3,13.8,12.4,11.8,11.1,11.6,11.3,11,10.8,9.9,10.8,12.3,12.6,12.8,12.5,12.4,13.6,13.1,14.2,2014
DE,Germany,16.7,15.7,15,14.5,13.9,13.5,14.3,13.5,12.7,12.5,12.1,11.6,11.2,10.8,10.4,9.7,8.5,10.4,10,8.8,NA
EL,Greece,28.2,28.1,27.6,26.2,25.1,24.3,25,25.4,24.3,24,23.6,23.3,22.4,22,21.5,20.8,19.2,20.9,20.3,20.93,NA
HU,Hungary,25,24.7,24.5,24.4,23.7,23,23.5,23.3,22.8,22.5,22.1,21.6,21.9,22.2,22.4,22.7,23.2,26,25,25.4,NA
IE,Ireland,15.4,15.2,14.8,13.4,12.7,12.2,13.1,13,12.8,12.7,12.2,11.8,11.3,10.8,10.4,9.7,8.9,9.9,9.4,10.1,NA
IT,Italy,26.1,25.2,24.4,23.2,22.3,21.4,22,21.8,21.2,21.6,21.1,20.8,20.6,20.2,19.8,19.5,18.7,20.4,20.2,20.3,NA
LV,Latvia,30.4,30,29.5,29,27.5,26.5,27.1,27.3,26.5,26.1,25.5,24.7,23.6,22.9,21.3,20.2,19.8,20.9,20.2,19.9,NA
LT,Lithuania,32,31.7,31.1,30.6,29.7,29.1,29.6,29.7,29,28.5,28,27.1,25.8,24.9,23.8,23,21.9,23.1,22.9,22.4,2019
LU,Luxembourg,9.8,9.8,9.9,10,9.4,8.5,8.8,8.4,8.2,8.2,8,8.1,8.3,8.4,8.2,7.9,7.4,8.6,8.4,8.3,2011
MT,Malta,26.7,26.7,26.9,27.2,26.4,25.8,25.9,26,25.8,25.3,24.3,24,24.3,24,23.6,23.2,22,23.5,23.1,23.4,NA
NL,Netherlands,12.7,12.5,12,10.9,10.1,9.6,10.2,10,9.8,9.5,9.1,9.2,9,8.8,8.4,7.5,7,8.1,7.8,8.2,NA
PL,Poland,27.7,27.4,27.1,26.8,26,25.3,25.9,25.4,25,24.4,23.8,23.5,23.3,23,22.2,21.7,20.7,22.5,22,21.9,2016
PT,Portugal,22.2,21.7,21.2,20.1,19.2,18.7,19.5,19.2,19.4,19.4,19,18.7,17.6,17.2,16.6,16.1,15.4,17,16.5,15.7,2009
RO,Romania,33.6,32.5,32.2,31.4,30.2,29.4,29.4,29.8,29.6,29.1,28.4,28.1,28,27.6,26.3,26.7,26.9,29.3,28.9,29,2022
SI,Slovenia,26.7,26.5,26,25.8,24.7,24,24.6,24.3,24.1,23.6,23.1,23.5,23.3,23.1,22.4,22.2,21.5,23.1,22.5,22.1,NA
ES,Spain,22.2,21.9,21.3,20.2,19.3,18.4,19.5,19.4,19.2,19.2,18.6,18.5,18.2,17.9,17.2,16.6,15.4,17.4,16.9,15.8,NA
SK,Slovakia,18.4,18.2,17.6,17.3,16.8,16,16.8,16.4,16,15.5,15,14.6,14.1,13.7,13,12.8,12.2,14,13.7,13.1,NA
SE,Sweden,18.6,18.1,17.5,16.2,15.6,14.9,15.4,15,14.7,14.3,13.9,13.6,13.2,12.6,12.1,11.6,10.7,11.7,11,10.8,NA"

df <- read.csv("stabil_lag_ambele_se_saft.csv")

# Reshape to LONG format (Panel Data Standard)
df_long <- df %>%
  pivot_longer(
    cols = starts_with("X") | matches("^\\d{4}$"), # Matches years (sometimes R adds X before numbers)
    names_to = "Year",
    values_to = "Shadow_Economy"
  ) %>%
  mutate(
    Year = as.numeric(gsub("X", "", Year)),
    # Create the SAFT Dummy Logic
    # 1 if current Year >= SAFT_Start_Year, else 0. 
    # If SAFT_Start_Year is NA, it remains 0.
    SAFT_Dummy = ifelse(!is.na(SAFT_Start_Year) & Year >= SAFT_Start_Year, 1, 0)
  ) %>%
  filter(Year <= 2022) # Shadow Economy data ends in 2022

# Convert to pdata.frame for Panel Analysis
pdf <- pdata.frame(df_long, index = c("Country_Code", "Year"))

# ---------------------------------------------------------
# 2. ROBUSTNESS CHECK: LAG STABILITY LOOP
# ---------------------------------------------------------

# We will test:
# Lag 0 (Immediate effect)
# Lag 1 (Effect after 1 year)
# Lag 2 (Effect after 2 years)
# Note: Since your data ends in 2022 and Romania starts in 2022, 
# Lag 1 and Lag 2 will drop Romania's treatment effect (it becomes NA).
# But this is useful to see if earlier adopters drive the results.

lags_to_test <- c(0, 1, 2)
results_store <- data.frame()

for (k in lags_to_test) {
  
  # Create the specific lag variable dynamically
  # We use 'lag(variable, k)' provided by plm
  formula_str <- paste0("Shadow_Economy ~ lag(SAFT_Dummy, ", k, ")")
  
  # Run Two-Way Fixed Effects Model (Entity + Time Fixed Effects)
  # effect = "twoways" controls for Country specific invariant traits and Time specific shocks
  model <- plm(as.formula(formula_str), 
               data = pdf, 
               model = "within", 
               effect = "twoways")
  
  # Get Robust Standard Errors (Clustered by Group/Country)
  # This is crucial for panel data serial correlation
  cov_robust <- vcovHC(model, type = "HC1", cluster = "group")
  robust_test <- coeftest(model, vcov = cov_robust)
  
  # Extract Data for Plotting
  term_name <- paste0("lag(SAFT_Dummy, ", k, ")")
  
  coef_val <- robust_test[term_name, "Estimate"]
  se_val   <- robust_test[term_name, "Std. Error"]
  p_val    <- robust_test[term_name, "Pr(>|t|)"]
  
  # Store results
  results_store <- rbind(results_store, data.frame(
    Lag = paste0("Lag ", k),
    Coefficient = coef_val,
    SE = se_val,
    Lower_CI = coef_val - 1.96 * se_val,
    Upper_CI = coef_val + 1.96 * se_val,
    P_Value = p_val
  ))
}

# ---------------------------------------------------------
# 3. VISUALIZATION (Coefficient Plot)
# ---------------------------------------------------------

print(results_store)

ggplot(results_store, aes(x = Lag, y = Coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_pointrange(aes(ymin = Lower_CI, ymax = Upper_CI), size = 1, color = "blue") +
  theme_minimal() +
  labs(
    title = "SAFT Impact Stability Check (2003-2022)",
    subtitle = "Coefficients of SAFT dummy across different lags (95% CI)",
    y = "Impact on Shadow Economy (% GDP)",
    x = "Model Specification",
    caption = "Model: Two-Way Fixed Effects (Country + Year). SE clustered by Country."
  ) +
  geom_text(aes(label = round(Coefficient, 3)), vjust = -1.5)