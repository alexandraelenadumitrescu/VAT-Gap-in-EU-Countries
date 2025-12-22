# Load libraries
if (!require(car)) install.packages("car")
if (!require(ggplot2)) install.packages("ggplot2")
library(car)
library(ggplot2)
library(stats)

# 1. LOAD DATA

raw_data <- read.csv("multe.csv")
names(raw_data) <- make.names(names(raw_data))

# Select Variables
target_var <- "VAT_Comp"
indep_vars <- setdiff(names(raw_data), c("Country", "Year", target_var))
df <- raw_data[, c("Country", target_var, indep_vars)] # Keep Country for plotting

# 2. ITERATE AND RANK MODELS
model_results <- data.frame()
max_vars <- 3 

# Loop through all combinations of 2 and 3 variables
for (k in 2:max_vars) {
  combos <- combn(indep_vars, k, simplify = FALSE)
  for (vars in combos) {
    # We use '*' for interactions
    f_string <- paste(target_var, "~", paste(vars, collapse = " * "))
    model <- try(lm(as.formula(f_string), data = df), silent = TRUE)
    
    if (!inherits(model, "try-error")) {
      s <- summary(model)
      adj_r2 <- s$adj.r.squared
      if (!is.null(adj_r2) && !is.na(adj_r2)) {
        model_results <- rbind(model_results, data.frame(
          Variables = paste(vars, collapse = ", "),
          Formula = f_string,
          Adj_R2 = adj_r2
        ))
      }
    }
  }
}

# 3. GET THE BEST CANDIDATE AND OPTIMIZE IT
top_model_info <- model_results[order(-model_results$Adj_R2), ][1, ]
cat("--- 1. BEST RAW MODEL (Full Interactions) ---\n")
print(top_model_info$Formula)

best_raw_model <- lm(as.formula(top_model_info$Formula), data = df)

cat("\n--- 2. STEPWISE OPTIMIZATION (Removing useless terms) ---\n")
final_model <- step(best_raw_model, direction = "both", trace = 0)
print(summary(final_model))

cat("\n--- 3. MULTICOLLINEARITY CHECK (VIF) ---\n")
# If VIF fails, it prints a message
tryCatch(print(vif(final_model)), error=function(e) print("VIF high due to interaction terms (Expected)"))

# 4. VISUALIZATION (Actual vs Predicted)
# We add predictions to the dataframe
df$Predicted_VAT_Gap <- predict(final_model, df)
df$Residuals <- abs(df$VAT_Comp - df$Predicted_VAT_Gap)

# Create Plot
p <- ggplot(df, aes(x = Predicted_VAT_Gap, y = VAT_Comp, label = Country)) +
  geom_point(color = "blue", size = 2) +
  geom_text(vjust = -0.5, size = 3, check_overlap = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "VAT Gap: Actual vs Predicted (Optimized Model)",
       subtitle = paste("Adj R2:", round(summary(final_model)$adj.r.squared, 3)),
       x = "Predicted VAT Gap (%)",
       y = "Actual VAT Gap (%)") +
  theme_minimal()

print(p)

cat("\n--- 5. TOP 3 COUNTRIES WITH WORST PREDICTION ERRORS (OUTLIERS) ---\n")
# Print countries where the model fails the most
print(df[order(-df$Residuals), c("Country", "VAT_Comp", "Predicted_VAT_Gap")][1:3, ])















# Încărcare pachete necesare pentru diagnostic
if (!require(lmtest)) install.packages("lmtest")
library(lmtest)
library(car)

cat("\n--- 1. REZOLVAREA VIF PRIN CENTRARE ---\n")
# Creăm versiuni centrate ale variabilelor (Valoare - Medie)
# Asta elimină multicoliniaritatea structurală
df$VAT_Rever_c <- scale(df$VAT_Rever, scale = FALSE) # Doar centrare, nu scalare
df$Governme_c  <- scale(df$Governme, scale = FALSE)
df$Regulatory_c <- scale(df$Regulatory, scale = FALSE)

# Refacem modelul cu variabilele centrate
# Observă cum VIF scade drastic, dar R-squared rămâne IDENTIC
centered_model <- lm(VAT_Comp ~ VAT_Rever_c * Governme_c + VAT_Rever_c * Regulatory_c, data = df)

# Verificăm noul VIF
cat("VIF pentru modelul centrat (ar trebui să fie mult mai mic):\n")
print(vif(centered_model))

cat("\n--- 2. TEST DE NORMALITATE A REZIDUURILOR (Shapiro-Wilk) ---\n")
# H0: Reziduurile sunt normale (Asta vrem)
# H1: Reziduurile NU sunt normale
shapiro_res <- shapiro.test(residuals(final_model))
print(shapiro_res)
if(shapiro_res$p.value > 0.05) {
  cat(">> CONCLUZIE: Reziduurile sunt normale (OK).\n")
} else {
  cat(">> ATENȚIE: Reziduurile NU sunt normale. Modelul poate fi instabil.\n")
}

cat("\n--- 3. TEST DE HOMOSCEDASTICITATE (Breusch-Pagan) ---\n")
# H0: Varianța este constantă (Asta vrem)
# H1: Avem heteroscedasticitate (Varianța erorii se schimbă)
bp_res <- bptest(final_model)
print(bp_res)
if(bp_res$p.value > 0.05) {
  cat(">> CONCLUZIE: Varianța este constantă (Homoscedasticitate OK).\n")
} else {
  cat(">> ATENȚIE: Ai Heteroscedasticitate. Încearcă log(y) sau erori robuste.\n")
}

cat("\n--- 4. DETECTAREA ȚĂRILOR CARE STRICĂ MODELUL (Outliers - Cook's Distance) ---\n")
# Distanța Cook măsoară cât de mult se schimbă modelul dacă ștergi o țară
cooksD <- cooks.distance(final_model)
influential <- cooksD[(cooksD > (4 / nrow(df)))] # Prag standard: 4/n
cat("Țări cu influență exagerată asupra coeficienților:\n")
if(length(influential) > 0) {
  print(influential)
} else {
  cat("Nicio țară nu are o influență exagerată.\n")
}

# 5. GRAFICE DE DIAGNOSTIC
par(mfrow = c(2, 2)) # Împarte ecranul în 4
plot(final_model)
par(mfrow = c(1, 1)) # Reset


































