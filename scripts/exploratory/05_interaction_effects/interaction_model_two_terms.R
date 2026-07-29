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
















library(MASS) # Pachetul standard pentru statistici robuste
library(lmtest)
library(sandwich)

# 1. Identificare Outliers (pentru raportare în paper)
cat("Outlierii identificați prin Cook's Distance > 0.5 sunt:\n")
cooks_d <- cooks.distance(best_model_final)
influential_obs <- which(cooks_d > 0.5)
print(df_final[influential_obs, "Country"])

# 2. Rulare REGRESIE ROBUSTĂ (RLM)
# Folosim metoda M-estimation cu ponderi Huber sau Bisquare
# Asta va reduce automat influența obs 4 și 14 fără să le șteargă
robust_model <- rlm(as.formula(best_formula), data = df_final, method = "MM")

cat("\n--- REZULTATE REGRESIE ROBUSTĂ (RLM) ---\n")
summary(robust_model)

# 3. Testare semnificație coeficienți (Robust t-test)
# RLM nu dă p-values standard, trebuie calculate așa:
coeftest(robust_model, vcov = vcovHC(robust_model, type = "HC3"))


# Vizualizare bazată pe modelul ROBUST
# interact_plot știe să gestioneze obiecte de tip rlm, 
# dar e bine să specificăm datele originale

plot_robust <- interact_plot(robust_model, 
                             pred = VAT_Rever, 
                             modx = Governme,
                             data = df_final,
                             plot.points = TRUE,   # Arată punctele
                             interval = TRUE,      # Intervalele de încredere vor fi mai largi (realiste)
                             x.label = "VAT Revenues (Centered)",
                             y.label = "VAT Compliance Gap",
                             main.title = "Efectul Moderator Robust (RLM)")

# Marcăm vizual outlierii (Opțional, dar de efect)
# Adăugăm etichete doar pentru Irlanda și Croația pe grafic
library(ggplot2)
# Soluția este argumentul inherit.aes = FALSE
plot_robust + 
  geom_text(data = df_final[influential_obs, ], 
            aes(x = VAT_Rever, y = VAT_Comp, label = Country), 
            nudge_y = 2, 
            color = "red", 
            fontface = "bold",
            inherit.aes = FALSE) # <--- ASTA REZOLVĂ EROAREA










library(boot)

# Define function to return the t-statistic of the interaction term
interaction_boot <- function(data, indices) {
  d <- data[indices,] # Allow resampling with replacement
  fit <- lm(VAT_Comp ~ VAT_Rever * Governme, data = d)
  return(coef(summary(fit))[4, 3]) # Return t-value of interaction
}

# Run 1000 bootstraps
results <- boot(data = df_final, statistic = interaction_boot, R = 1000)

# If the confidence interval includes 0, the interaction is spurious
boot.ci(results, type = "bca")




# INSTALL IF MISSING
if(!require(spdep)) install.packages("spdep")
library(spdep)

# 1. PREPARE COORDINATES
# Ideally, you load a shapefile, but for N=27 we can use capital coordinates approx.
# Since we don't have lat/long in your df, we will use a neighbor matrix based on distance implies similarity
# OR, a simpler method: The "1/Distance" weighting if you have coordinates.

# -- SIMPLIFIED APPROACH FOR PEER REVIEW WITHOUT SHAPEFILES --
# We check if the residuals are random. If we lack coordinates, 
# we can visualize residuals by region (West, East, South, North).

# Assuming you DON'T have lat/long columns, we will extract residuals 
# and look at them sorted by Country Name (weak check) or Region.

# BUT, to do this professionally, I need to know:
# Do you have a column for "Region" (e.g., CEE, Western Europe, Southern Europe)?

# -- IF YOU WANT TO RUN THE REAL TEST, ADD LAT/LONG --
# Let's create a dummy spatial weight matrix to demonstrate. 
# PLEASE UPDATE the lat/long for your actual data if possible.

# Example: Extract residuals from the Robust Model
df_final$residuals_robust <- residuals(robust_model)

# Quick Visual Check: Are residuals clustered by magnitude?
# Sort by magnitude
df_sorted <- df_final[order(df_final$residuals_robust), ]
barplot(df_sorted$residuals_robust, names.arg=df_sorted$Country, las=2, 
        main="Residuals of Robust Model (Check for Regional Clusters)",
        ylab="Residual Value", cex.names=0.7)

# INTERPRETATION:
# Look at the bar plot. 
# Do you see all Eastern European countries on one side? 
# Do you see all Nordic countries on the other?
# If the residuals look "mixed" (e.g., Romania next to France), you are SAFE.
# If you see blocks of countries (e.g. RO, BG, HU all negative), you have Spatial Autocorrelation.