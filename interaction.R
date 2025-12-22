# Load necessary library
library(stats)

# 1. READ THE DATA
# I am using the text= parameter to read the data you pasted directly.
# To read from your local file, uncomment the line below:
raw_data <- read.csv("multe.csv")



# 2. PREPROCESSING
# Clean column names (replaces spaces with dots, removes brackets)
names(raw_data) <- make.names(names(raw_data))

# Identify Target Variable: "VAT_Comp" (VAT Gap)
target_var <- "VAT_Comp"

# Identify Independent Variables
# We exclude "Country" (ID), "Year" (Constant), and the Target itself
indep_vars <- setdiff(names(raw_data), c("Country", "Year", target_var))

# Remove the Country column for analysis
df <- raw_data[, c(target_var, indep_vars)]

# 3. GENERATE ALL COMBINATIONS (1, 2, and 3 variables)
model_results <- data.frame()
max_vars <- 3 # As requested, max 2-3 variables

for (k in 1:max_vars) {
  # Get all combinations of size k
  combos <- combn(indep_vars, k, simplify = FALSE)
  
  for (vars in combos) {
    # Create formula with interactions using '*'
    # e.g., y ~ x1 * x2 becomes y ~ x1 + x2 + x1:x2
    f_string <- paste(target_var, "~", paste(vars, collapse = " * "))
    
    # Fit the linear model
    model <- try(lm(as.formula(f_string), data = df), silent = TRUE)
    
    if (!inherits(model, "try-error")) {
      # Extract summary statistics
      s <- summary(model)
      adj_r2 <- s$adj.r.squared
      
      # If R2 is valid (not NaN), store it
      if (!is.null(adj_r2) && !is.na(adj_r2)) {
        model_results <- rbind(model_results, data.frame(
          Variables = paste(vars, collapse = ", "),
          Formula = f_string,
          Adj_R2 = adj_r2,
          Num_Vars = k
        ))
      }
    }
  }
}

# 4. SORT AND DISPLAY
# Sort by Adjusted R-squared (descending)
top_models <- model_results[order(-model_results$Adj_R2), ]

# Display top 10 models
cat("--- TOP 10 MODELS FOR PREDICTING VAT GAP (WITH INTERACTIONS) ---\n")
print(head(top_models, 10), row.names = FALSE)

# Optional: Print detailed summary of the #1 best model
cat("\n\n--- DETAILED SUMMARY OF THE BEST MODEL ---\n")
best_formula <- top_models$Formula[1]
best_model <- lm(as.formula(best_formula), data = df)
print(summary(best_model))





















# ... (run your previous code first to get 'best_model') ...

library(car) # You might need to install.packages("car")

cat("\n--- DIAGNOSTICS ---\n")

# 1. Check Variance Inflation Factor (VIF)
# Note: VIF is often naturally high for interaction terms, 
# but this confirms the severity.
vif_val <- try(vif(best_model), silent=TRUE)
if(!inherits(vif_val, "try-error")) {
  print(vif_val)
} else {
  cat("Could not calculate standard VIF (likely due to aliasing or perfect correlation).\n")
}

# 2. SUGGESTION: Refit a simpler model (Remove the 3-way interaction)
# The output showed the 3-way interaction was not significant (p=0.46)
cat("\n--- SIMPLIFIED MODEL (No 3-way interaction) ---\n")
simple_formula <- "VAT_Comp ~ (VAT_Rever + Governme + Regulatory)^2" # ^2 means only up to 2-way interactions
simple_model <- lm(as.formula(simple_formula), data = df)
print(summary(simple_model))

# 3. SUGGESTION: Stepwise reduction (AIC)
# Let R automatically remove the useless variables to fix overfitting
cat("\n--- OPTIMIZED MODEL (Stepwise Reduction) ---\n")
optimized_model <- step(best_model, direction="both", trace=0)
print(summary(optimized_model))











# Asigură-te că ai rulat codul anterior și ai obiectul 'optimized_model'

# 1. Calculăm PRESS (Predicted Residual Error Sum of Squares)
# Asta simulează scoaterea fiecărei țări pe rând și prezicerea ei
pr <- residuals(optimized_model) / (1 - lm.influence(optimized_model)$hat)
PRESS <- sum(pr^2)

# 2. Calculăm Total Sum of Squares (TSS)
TSS <- sum((df$VAT_Comp - mean(df$VAT_Comp))^2)

# 3. Calculăm Predicted R-squared
pred_r2 <- 1 - (PRESS / TSS)

# 4. AFIȘĂM VERDICTUL
adj_r2 <- summary(optimized_model)$adj.r.squared

cat("\n--- DETECTOR DE OVERFITTING ---\n")
cat("Adjusted R-squared (Pe datele cunoscute):", round(adj_r2, 4), "\n")
cat("Predicted R-squared (Pe date 'noi'):     ", round(pred_r2, 4), "\n")

diff <- adj_r2 - pred_r2
cat("Diferența (Drop-off):                    ", round(diff, 4), "\n\n")

if (diff > 0.2) {
  cat("VERDICT: 🚩 OVERFITTING PROBABIL!\n")
  cat("Modelul pierde multă putere când prezice date necunoscute.\n")
  cat("Soluție: Întoarce-te la un model mai simplu (fără interacțiuni sau maxim 2 variabile).\n")
} else if (diff > 0.1) {
  cat("VERDICT: ⚠️ ATENȚIE (Zona Gri).\n")
  cat("Modelul este acceptabil, dar puțin instabil.\n")
} else {
  cat("VERDICT: ✅ MODEL ROBUST.\n")
  cat("Modelul generalizează bine și nu a memorat doar zgomotul.\n")
  
  
  
  
  
  
  
  
  
  
  
  library(ggplot2)
  
  # 1. Definim 3 scenarii pentru "Venituri din TVA" (VAT_Rever)
  # Low (Minim), Mean (Medie), High (Maxim)
  vat_levels <- quantile(df$VAT_Rever, probs = c(0.1, 0.5, 0.9))
  vat_labels <- c("Venituri TVA Mici (Low)", "Venituri TVA Medii", "Venituri TVA Mari (High)")
  
  # 2. Creăm un set de date sintetic pentru predicție
  # Variem 'Governme' de la minim la maxim, ținem 'Regulatory' constant la medie
  pred_data <- expand.grid(
    Governme = seq(min(df$Governme), max(df$Governme), length.out = 100),
    VAT_Rever = vat_levels,
    Regulatory = mean(df$Regulatory) # Fixăm a 3-a variabilă
  )
  
  # 3. Facem predicțiile folosind modelul optimizat
  pred_data$Predicted_Gap <- predict(optimized_model, newdata = pred_data)
  
  # Transformăm VAT_Rever în factor pentru legendă
  pred_data$Scenario <- factor(pred_data$VAT_Rever, levels = vat_levels, labels = vat_labels)
  
  # 4. Generăm Graficul de Interacțiune
  p_interact <- ggplot(pred_data, aes(x = Governme, y = Predicted_Gap, color = Scenario)) +
    geom_line(size = 1.2) +
    geom_point(data = df, aes(x = Governme, y = VAT_Comp, color = NULL), alpha = 0.3, color = "black") + # Punctele reale
    labs(
      title = "Efectul Moderator: Cum influențează Guvernarea Gap-ul de TVA",
      subtitle = "Interacțiunea dintre Calitatea Guvernării și Dependența de Veniturile din TVA",
      x = "Scor Eficiență Guvernamentală (Governme)",
      y = "Predicție VAT Gap (%)",
      color = "Nivel Venituri TVA"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p_interact)
}