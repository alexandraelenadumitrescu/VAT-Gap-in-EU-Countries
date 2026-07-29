# --- PASUL 8: TESTAREA CURBEI LAFFER (Forma Neliniară) ---
# Cerința 4a din PDF

# Creăm variabila pătratică (TVA la pătrat)
train_set$VAT_Rate_Sq <- train_set$VAT_Rate ^ 2

# Rulăm modelul pătratic
laffer_model <- lm(VAT_Gap ~ Internet_Access + CPI + VAT_Rate + VAT_Rate_Sq, 
                   data = train_set)

cat("\n=== Testarea Curbei Laffer (Termen Pătratic) ===\n")
print(summary(laffer_model))

# Verificăm dacă termenul pătratic (VAT_Rate_Sq) este semnificativ
p_val_sq <- summary(laffer_model)$coefficients["VAT_Rate_Sq", "Pr(>|t|)"]

if(p_val_sq < 0.10) {
  cat("\nCONCLUZIE: Există o relație neliniară (Curba Laffer se confirmă parțial).\n")
} else {
  cat("\nCONCLUZIE: Nu există dovezi statistice pentru Curba Laffer în acest eșantion.\n")
  cat("Relația rămâne liniară.\n")
}

# Vizualizare simplă a relației TVA - GAP
ggplot(data, aes(x=VAT_Rate, y=VAT_Gap)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +
  labs(title = "Testarea Vizuală a Curbei Laffer",
       subtitle = "Relația dintre Cota TVA și Gap",
       x = "Cota Standard TVA (%)", y = "VAT Gap (%)") +
  theme_minimal()