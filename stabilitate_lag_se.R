# =========================
# 1. Load data
# =========================
df <- read.csv("se_crossgap2023.csv", stringsAsFactors = FALSE)

# Curățare nume coloane (ani ca numeric)
names(df) <- gsub("X", "", names(df))

# Convertim coloanele de ani la numeric
years <- as.character(2003:2022)
df[years] <- lapply(df[years], as.numeric)

# VAT gap 2023 (coloana a 3-a în exemplul tău)
df$VAT_gap_2023 <- as.numeric(gsub("%", "", df$VAT_Gap))

# =========================
# 2. Shadow economy lags
# =========================
df$shadow_2022 <- df$`2022`
df$shadow_2021 <- df$`2021`
df$shadow_2020 <- df$`2020`


# =========================
# 3. Regressions
# =========================
m_lag1 <- lm(VAT_gap_2023 ~ shadow_2022, data = df)
m_lag2 <- lm(VAT_gap_2023 ~ shadow_2021, data = df)
m_lag3 <- lm(VAT_gap_2023 ~ shadow_2020, data = df)


# =========================
# 4. Stability table
# =========================
# =========================
# 4. Stability table (Updated)
# =========================
extract_results <- function(model, lag){
  c(
    Lag = lag,
    Beta = coef(model)[2],
    Std_Error = summary(model)$coefficients[2,2],
    t_value = summary(model)$coefficients[2,3],  # This is the t-value
    p_value = summary(model)$coefficients[2,4],  # This is the p-value (Pr(>|t|))
    R2 = summary(model)$r.squared
  )
}

results <- rbind(
  extract_results(m_lag1, "Lag 1 (2022)"),
  extract_results(m_lag2, "Lag 2 (2021)"),
  extract_results(m_lag3, "Lag 3 (2020)")
)

results <- as.data.frame(results)

# Convert numerical columns from character/factor to numeric
# Columns 2 to 6 are now numerical (Beta, Std_Error, t_value, p_value, R2)
results[,2:6] <- lapply(results[,2:6], as.numeric)

# Print the final table with t-value and p-value
print(results)
# =========================
# 5. Coefficient stability plot
# =========================
library(ggplot2)

ggplot(results, aes(x = Lag, y = Beta.shadow_2022, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Stability of Shadow Economy Effect on VAT Gap (2023)",
    y = "Coefficient (β)",
    x = "Lag definition"
  ) +
  theme_minimal()







