# ==============================================================================
# SCRIPT GENERARE TOP MODELE DE REGRESIE CU INTERACȚIUNI
# ==============================================================================

# 1. ÎNCĂRCAREA DATELOR
# Datele sunt citite direct din formatul text furnizat

# Citim datele și curățăm numele coloanelor
df <- read.csv("multe.csv")
names(df) <- make.names(names(df)) # Înlocuiește spațiile cu puncte (ex: Rule.of.Law)

# 2. DEFINIRE VARIABILE
# Variabila Dependentă (Y)
target <- "VAT_Comp"

# Variabile Independente (X) - excludem Country, Year și Target-ul
predictors <- setdiff(names(df), c("Country", "Year", target))

# 3. GENERARE COMBINAȚII ȘI RULARE MODELE
results <- data.frame(
  Formula = character(),
  Adj_R2 = numeric(),
  AIC_Score = numeric(),
  Nr_Vars = integer(),
  stringsAsFactors = FALSE
)

# Iterăm prin numărul de variabile dorite (2 și 3)
for (k in 2:3) {
  # Generează toate combinațiile posibile de 'k' variabile
  combos <- combn(predictors, k, simplify = FALSE)
  
  cat(paste("Analyzing", length(combos), "combinations for", k, "variables...\n"))
  
  for (vars in combos) {
    # Construim formula cu interacțiuni (operatorul *)
    # Ex: Y ~ X1 * X2 devine Y ~ X1 + X2 + X1:X2
    formula_str <- paste(target, "~", paste(vars, collapse = " * "))
    
    # Rulăm modelul (folosim try pentru a evita erori la multicoliniaritate perfectă)
    model <- try(lm(as.formula(formula_str), data = df), silent = TRUE)
    
    if (!inherits(model, "try-error")) {
      s <- summary(model)
      # Inside your loop, after s <- summary(model)
      
      # 1. Calculate HAT Matrix diagonals (leverage)
      hat_values <- hatvalues(model)
      
      # 2. Calculate PRESS residuals (Prediction Error Sum of Squares)
      # residuals(model) are the ordinary residuals
      press_residuals <- residuals(model) / (1 - hat_values)
      
      # 3. Calculate PRESS Statistic
      PRESS <- sum(press_residuals^2)
      
      # 4. Calculate Total Sum of Squares (TSS)
      TSS <- sum((df[[target]] - mean(df[[target]]))^2)
      
      # 5. Calculate Predicted R-Squared
      Pred_R2 <- 1 - (PRESS / TSS)
      
      # Add Pred_R2 to your results dataframe
      
      # Salvăm doar dacă avem un R-squared valid
      if (!is.na(s$adj.r.squared)) {
        results <- rbind(results, data.frame(
          Formula = formula_str,
          Adj_R2 = s$adj.r.squared,
          AIC_Score = AIC(model),
          Nr_Vars = k,Pred_R2=Pred_R2
        ))
      }
    }
  }
}

# 4. SORTARE ȘI AFIȘARE TOP
# Ordonăm descrescător după Adjusted R-squared
top_results <- results[order(-results$Adj_R2), ]

cat("\n========================================================\n")
cat(" TOP 20 MODELE CU INTERACȚIUNI (MAX 2-3 VARIABILE) \n")
cat(" Ordonat după Adjusted R-Squared \n")
cat("========================================================\n\n")

# Afișăm primele 20
print(head(top_results[, c("Adj_R2", "Formula")], 20), row.names = FALSE)

# 5. BONUS: SALVARE ÎN CSV (Opțional)
# write.csv(top_results, "top_modele_interactiune.csv", row.names = FALSE)