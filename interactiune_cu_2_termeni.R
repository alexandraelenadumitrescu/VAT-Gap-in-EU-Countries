library(interactions) # Pachet excelent pentru vizualizare și testare
library(car)          # Pentru VIF

# 1. PRE-PROCESARE: Centrarea variabilelor numerice
# (Nu centram Y-ul, doar predictorii)
predictors_vars <- setdiff(names(df), c("Country", "Year", target))
df_centered <- df # Facem o copie

for(var in predictors_vars){
  # scale(..., scale=FALSE) face doar scăderea mediei, nu împarte la deviația standard
  if(is.numeric(df[[var]])) {
    df_centered[[var]] <- as.numeric(scale(df[[var]], center = TRUE, scale = FALSE))
  }
}

# 2. RULARE MODEL (Exemplu pentru o pereche specifică)
# Să zicem VAT_Standard_Rate * Government_Effectiveness
model_safe <- lm(VAT_Comp ~ VAT_Stand * Governme, data = df_centered)

# 3. DIAGNOSTIC
summary(model_safe)

# Verificare Multicoliniaritate (VIF trebuie să fie < 5, ideal < 2.5)
vif(model_safe) 

# 4. VIZUALIZARE (Esențial pentru interpretare!)
# Aceasta arată cum panta lui "VAT Rate" se schimbă când "Guvernul" devine mai eficient
interact_plot(model_safe, pred = VAT_Stand, modx = Governme, 
              plot.points = TRUE, # Arată punctele reale (cele 27 țări)
              interval = TRUE)    # Arată intervalul de încredere































# ==============================================================================
# SCRIPT FINAL: TOP MODELE ROBUSTE (Date Centrate, Max 2-way)
# ==============================================================================

library(interactions)
library(car)

# 1. PREGĂTIRE DATE (CENTRARE)
# Recitim datele curate pentru a fi siguri
df_final <- df # Folosim df-ul tău original curat
predictors_vars <- setdiff(names(df_final), c("Country", "Year", target))

# Centrăm toți predictorii numerici
for(var in predictors_vars){
  if(is.numeric(df_final[[var]])) {
    df_final[[var]] <- as.numeric(scale(df_final[[var]], center = TRUE, scale = FALSE))
  }
}

# 2. GENERARE COMBINAȚII (DOAR PERECHI)
results_safe <- data.frame(
  Formula = character(),
  Adj_R2 = numeric(),
  Interaction_P_Value = numeric(), # Salvăm și p-value la interacțiune
  VIF_Max = numeric(),             # Salvăm VIF-ul maxim
  stringsAsFactors = FALSE
)

# Generăm doar combinații de 2 variabile (cele mai sigure pe N=27)
combos <- combn(predictors_vars, 2, simplify = FALSE)

cat(paste("Testăm", length(combos), "modele cu interacțiuni...\n"))

for (vars in combos) {
  # Formula: Y ~ A * B
  f_str <- paste(target, "~", paste(vars, collapse = " * "))
  
  model <- try(lm(as.formula(f_str), data = df_final), silent = TRUE)
  
  if (!inherits(model, "try-error")) {
    s <- summary(model)
    
    # Extragem p-value-ul interacțiunii (ultimul rând din coeficienți)
    p_val_inter <- try(tail(s$coefficients[,4], 1), silent=TRUE)
    
    # Calculăm VIF (doar dacă nu dă eroare)
    vif_val <- try(max(vif(model)), silent=TRUE)
    if(inherits(vif_val, "try-error")) vif_val <- 999
    
    if (!is.na(s$adj.r.squared)) {
      results_safe <- rbind(results_safe, data.frame(
        Formula = f_str,
        Adj_R2 = s$adj.r.squared,
        Interaction_P_Value = as.numeric(p_val_inter),
        VIF_Max = as.numeric(vif_val)
      ))
    }
  }
}

# 3. FILTRARE ȘI SORTARE
# Vrem modele cu R2 mare DAR și cu interacțiune semnificativă (p < 0.05 sau măcar < 0.1)
top_safe <- results_safe[results_safe$Interaction_P_Value < 0.10, ] # Filtru lejer p<0.10
top_safe <- top_safe[order(-top_safe$Adj_R2), ]

cat("\n--- TOP 10 MODELE 'SAFE' (Centrate, 2-way, Interacțiune Semnificativă) ---\n")
print(head(top_safe, 10), row.names = FALSE)

# 4. AFISARE CÂȘTIGĂTOR
best_formula <- top_safe$Formula[1]
cat("\n--- Cel mai bun model robust este: ---\n")
print(best_formula)

# Rulăm detaliile pentru câștigător
best_model_final <- lm(as.formula(best_formula), data = df_final)
print(summary(best_model_final))

# Facem graficul pentru câștigător
# (Extragem numele variabilelor din formulă pentru plot)
terms <- all.vars(as.formula(best_formula))
# terms[1] e Y, terms[2] e Var1, terms[3] e Var2
interact_plot(best_model_final, pred = !!sym(terms[2]), modx = !!sym(terms[3]), 
              plot.points = TRUE, interval = TRUE,
              main.title = "Interacțiunea Câștigătoare")
















# Run this on your best_model_final
# Cook's Distance Plot
plot(best_model_final, which = 4)