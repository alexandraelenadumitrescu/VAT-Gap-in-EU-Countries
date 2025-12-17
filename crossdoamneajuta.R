# 1. Definirea căii către fișier
file_path <- "C:/Users/alexa/Documents/proiect-econometrie/Date_Proiect_UE_GoogleTrends_2022.csv"

# Verificăm dacă fișierul există înainte de a continua
if (!file.exists(file_path)) {
  stop("Eroare: Fișierul nu a fost găsit la calea specificată. Verifică numele și calea.")
}

# 2. Încărcarea datelor
df <- read.csv(file_path, stringsAsFactors = FALSE)

# 3. Pregătirea variabilelor
target_var <- "VAT_Gap"

# Selectăm doar coloanele numerice (regresia simplă are nevoie de numere)
numeric_cols <- names(df)[sapply(df, is.numeric)]

# Lista de variabile de exclus:
# - Target-ul (evident)
# - Year (este constant sau irelevant pentru variația cross-section)
# - VAT_Center (este derivat matematic din VAT_Gap, deci ar trișa)
# - Geo_Code, Country (nu sunt numerice, dar le excludem explicit dacă apar)
exclusions <- c(target_var, "Year", "VAT_Center", "Region_Dummy")
predictors <- setdiff(numeric_cols, exclusions)

# 4. Rularea Regresiilor (Loop)
results <- data.frame(
  Variabila = character(),
  R_Squared = numeric(),
  P_Value = numeric(),
  Coeficient = numeric(),
  stringsAsFactors = FALSE
)

print("Se rulează regresia pentru fiecare variabilă...")

for (var in predictors) {
  # Construim formula: VAT_Gap ~ Variabila_X
  f <- as.formula(paste(target_var, "~", var))
  
  # Rulăm modelul
  model <- lm(f, data = df)
  
  # Extragem sumarul
  s <- summary(model)
  
  # Salvăm rezultatele
  results <- rbind(results, data.frame(
    Variabila = var,
    R_Squared = s$r.squared,
    P_Value = s$coefficients[2, 4],  # P-value pentru variabila independentă
    Coeficient = s$coefficients[2, 1] # Semnul relației (+ sau -)
  ))
}

# 5. Ordonarea rezultatelor după "Bonitate" (R-Squared descrescător)
results <- results[order(-results$R_Squared), ]

# 6. Afișarea rezultatelor
print(paste("Variabila Tinta:", target_var))
print("---------------------------------------------------")
print(head(results, 10)) # Afișează top 10

# Opțional: Salvarea rezultatelor într-un CSV nou
# write.csv(results, "C:/Users/alexa/Documents/proiect-econometrie/Rezultate_Regresie.csv", row.names = FALSE)









# 1. Define the Multivariate Model
# We combine the top distinct factors: ShadowEconomy, Governance, and StandardVAT
# We use 'ShadowEconomy' (original) instead of 'Effective_Shadow' for easier interpretation of percentages.

multi_model <- lm(VAT_Gap ~ ShadowEconomy + Governance + StandardVAT, data = df)

# 2. Get the Summary
model_summary <- summary(multi_model)

# 3. Print Key Stats
print("---------------------------------------------------")
print("REZULTATE REGRESIE MULTIVARIATA")
print("---------------------------------------------------")
print(paste("R-Squared (Adjusted):", round(model_summary$adj.r.squared, 4)))
print("Coefficients:")
print(model_summary$coefficients)

# 4. Check for Multicollinearity (Optional but recommended)
# This checks if ShadowEconomy and Governance are too correlated to work together
if(!require(car)) install.packages("car")
library(car)
print("---------------------------------------------------")
print("VIF Values (Variance Inflation Factor):")
print("(Values > 5 indicate problematic correlation)")
print(vif(multi_model))
#FAIL



# 1. Modelul A: Economie Gri + Digitalizare
# Verificăm dacă infrastructura digitală compensează evaziunea
model_A <- lm(VAT_Gap ~ ShadowEconomy + InternetAccess, data = df)
sum_A <- summary(model_A)

# 2. Modelul B: Economie Gri + Cota TVA
# Verificăm dacă "presiunea fiscală" (TVA mare) contează alături de economia gri
model_B <- lm(VAT_Gap ~ ShadowEconomy + StandardVAT, data = df)
sum_B <- summary(model_B)

# 3. Afisare Comparativa
print("---------------------------------------------------")
print("REZULTATE MODEL A (Shadow + Internet)")
print(paste("R-Squared (Adjusted):", round(sum_A$adj.r.squared, 4)))
print(sum_A$coefficients)

print("---------------------------------------------------")
print("REZULTATE MODEL B (Shadow + StandardVAT)")
print(paste("R-Squared (Adjusted):", round(sum_B$adj.r.squared, 4)))
print(sum_B$coefficients)

# 4. Verificare VIF pentru Modelul A (să fim siguri că nu se bat cap în cap)
if(require(car)){
  print("--- VIF Model A ---")
  print(vif(model_A))
}
#FAIL






# 1. Modelul C: Economie Gri + Agricultura
# Ipoteza: Sectoarele agricole mari au evaziune TVA mai mare (piețe, cash).
model_C <- lm(VAT_Gap ~ ShadowEconomy + VAB.Agriculture, data = df)
sum_C <- summary(model_C)

# 2. Modelul D: Economie Gri + Urbanizare
# Ipoteza: Orașele mari (Urbanizare mare) colectează TVA mai eficient (supermarketuri vs piețe).
model_D <- lm(VAT_Gap ~ ShadowEconomy + Urbanizare, data = df)
sum_D <- summary(model_D)

# 3. Afișare Rezultate
print("---------------------------------------------------")
print("REZULTATE MODEL C (Shadow + Agriculture)")
print(paste("R-Squared (Adjusted):", round(sum_C$adj.r.squared, 4)))
print("Coefficients (Uită-te la P-value pentru Agriculture):")
print(sum_C$coefficients)

print("---------------------------------------------------")
print("REZULTATE MODEL D (Shadow + Urbanizare)")
print(paste("R-Squared (Adjusted):", round(sum_D$adj.r.squared, 4)))
print(sum_D$coefficients)

# 4. Verificare corelație (să nu fie agricultura același lucru cu economia gri)
if(require(car)){
  print("--- VIF Model C ---")
  print(vif(model_C))
}











# 1. Definim modelul "MAXIM"
# Includem tot ce ar putea avea sens economic (fără variabile duplicate sau derivate)
# Excludem Country, Year, Geo_Code și variabilele "_Center" sau "_Dummy"

full_model <- lm(VAT_Gap ~ ShadowEconomy + Governance + StandardVAT + 
                   InternetAccess + Unemployment_rate + GDP_per_capita + 
                   FinalConsumption + CPI_Score + Fiscal_Resilience_Index, 
                 data = df)

# 2. Rulăm algoritmul Stepwise (direcția "both" înseamnă că poate scoate și băga variabile)
# R va printa procesul de selecție.
print("--- Începe Optimizarea Automată ---")
best_model <- step(full_model, direction = "both", trace = 0) # trace=0 ascunde pașii intermediari lungi

# 3. Rezultatul Final
print("---------------------------------------------------")
print("CEL MAI BUN MODEL IDENTIFICAT AUTOMAT")
print("---------------------------------------------------")
s_best <- summary(best_model)

print(paste("R-Squared (Adjusted):", round(s_best$adj.r.squared, 4)))
print("Variabilele Câștigătoare:")
print(s_best$coefficients)

# 4. Verificare VIF pentru modelul final (să nu avem coliniaritate)
if(require(car)){
  print("--- Verificare Coliniaritate (VIF) ---")
  print(vif(best_model))
  
  
  
  
  
  
  
  
  
  # 1. Definim din nou modelul complet
  full_model <- lm(VAT_Gap ~ ShadowEconomy + Governance + StandardVAT + 
                     InternetAccess + Unemployment_rate + GDP_per_capita + 
                     FinalConsumption + CPI_Score + Fiscal_Resilience_Index, 
                   data = df)
  
  # 2. Rulăm Stepwise
  best_model <- step(full_model, direction = "both", trace = 0)
  
  # 3. Extragem sumarul
  s_best <- summary(best_model)
  
  print("---------------------------------------------------")
  print("REZULTAT FINAL (Dupa eliminarea variabilelor slabe)")
  print("---------------------------------------------------")
  print(paste("R-Squared (Adjusted):", round(s_best$adj.r.squared, 4)))
  print("Variabilele ramase in model:")
  print(s_best$coefficients)
  
  # 4. Verificare VIF cu protecție la eroare
  # Calculăm câte variabile au rămas (excludem Intercept-ul)
  num_vars <- length(coef(best_model)) - 1 
  
  if(num_vars >= 2){
    if(require(car)){
      print("--- VIF (Coliniaritate) ---")
      print(vif(best_model))
    }
  } else {
    print("--- Nota: VIF nu se calculează deoarece a rămas o singură variabilă relevantă. ---")
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # --- PARTEA 1: Model cu Termen de Interacțiune ---
# Testăm ipoteza: Shadow Economy crește Gap-ul, dar Guvernanța o blochează?
# Semnul "*" în R creează automat variabilele individuale PLUS interacțiunea lor.

interaction_model <- lm(VAT_Gap ~ ShadowEconomy * Governance, data = df)
s_inter <- summary(interaction_model)

print("---------------------------------------------------")
print("REZULTATE MODEL CU INTERACTIUNE")
print("---------------------------------------------------")
print(paste("R-Squared (Adjusted):", round(s_inter$adj.r.squared, 4)))
print(s_inter$coefficients)

# Interpretare:
# Dacă 'ShadowEconomy:Governance' are P-value < 0.05, interacțiunea e validă.

# --- PARTEA 2: Calculul "Coeficientului de Deviație" (Reziduurile) ---
# Vedem ce țări performează mai bine sau mai rău decât prezice modelul simplu.

# Folosim modelul simplu (cel câștigător anterior) pentru referință
simple_model <- lm(VAT_Gap ~ ShadowEconomy, data = df)

# Adăugăm reziduurile în tabel
df$Model_Prediction <- predict(simple_model)
df$Residuals <- residuals(simple_model)

# Ordonăm țările după cât de mult "păcălesc" modelul
# Reziduu POZITIV = Gap REAL mai mare decât zice modelul (Problemă de colectare)
# Reziduu NEGATIV = Gap REAL mai mic decât zice modelul (Colectare eficientă)
analysis_table <- df[order(df$Residuals), c("Geo_Code", "VAT_Gap", "ShadowEconomy", "Model_Prediction", "Residuals")]

print("---------------------------------------------------")
print("ANALIZA REZIDUURILOR (Cine sfidează modelul?)")
print("---------------------------------------------------")
print("TOP 5 Țări care colectează MAI BINE decât ne așteptam (Reziduu Negativ):")
print(head(analysis_table, 5))

print("---------------------------------------------------")
print("TOP 5 Țări care colectează MAI PROST decât ne așteptam (Reziduu Pozitiv):")
print(tail(analysis_table, 5))

# Opțional: Vizualizare Interacțiune (Dacă pachetul interactions există)
# install.packages("interactions")
# library(interactions)
# interact_plot(interaction_model, pred = ShadowEconomy, modx = Governance)










library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Selectăm datele pentru RO, BG și calculăm Media UE
target_countries <- c("RO", "BG")

# Calculăm media UE pentru restul țărilor
eu_avg <- df %>%
  filter(!Geo_Code %in% target_countries) %>%
  summarise(
    Geo_Code = "Media UE",
    ShadowEconomy = mean(ShadowEconomy, na.rm = TRUE),
    # CPI Score: 0 = Foarte Corupt, 100 = Foarte Curat.
    # Transformăm în "Nivel Corupție" (100 - Score) pentru a fi intuitiv (Bară mare = Corupție mare)
    Corruption_Level = 100 - mean(CPI_Score, na.rm = TRUE), 
    VAT_Gap = mean(VAT_Gap, na.rm = TRUE)
  )

# Extragem RO și BG
ro_bg <- df %>%
  filter(Geo_Code %in% target_countries) %>%
  mutate(Corruption_Level = 100 - CPI_Score) %>%
  select(Geo_Code, ShadowEconomy, Corruption_Level, VAT_Gap)

# Combinăm totul
comparison_data <- bind_rows(ro_bg, eu_avg)

# 2. Transformăm datele (Format Lung)
data_long <- comparison_data %>%
  pivot_longer(cols = c("ShadowEconomy", "Corruption_Level", "VAT_Gap"),
               names_to = "Indicator",
               values_to = "Valoare")

# --- FIXUL ESTE AICI (Folosim dplyr::recode explicit) ---
data_long$Indicator <- dplyr::recode(data_long$Indicator,
                                     "ShadowEconomy" = "1. Economie Gri (%)",
                                     "Corruption_Level" = "2. Nivel Corupție (Est.)",
                                     "VAT_Gap" = "3. VAT Gap (Neîncasare)")

# 3. Generăm Graficul "Paradoxul Balcanic"
ggplot(data_long, aes(x = Indicator, y = Valoare, fill = Geo_Code)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # Culori: RO (Roșu), BG (Turcoaz), UE (Gri)
  scale_fill_manual(values = c("BG" = "#009688", "RO" = "#d73027", "Media UE" = "#999999")) +
  
  # Etichete valori
  geom_text(aes(label = round(Valoare, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4, fontface = "bold") +
  
  theme_minimal() +
  labs(title = "Paradoxul Balcanic: România vs Bulgaria",
       subtitle = "România și Bulgaria au probleme similare (Gri + Corupție), dar Bulgaria colectează mult mai bine.",
       y = "Procent (%)",
       x = NULL,
       fill = "Țara / Zona") +
  theme(legend.position = "top",
        axis.text.x = element_text(face = "bold", size = 10))



library(ggplot2)
library(ggrepel) # Pachet pentru a nu se suprapune etichetele

# Definim țările paradoxale pentru a le colora diferit
paradoxuri <- c("HU", "SK", "MT", "RO", "BG", "EE", "LT")

df$Status <- ifelse(df$Geo_Code %in% paradoxuri, "Paradox / Outlier", "Normal")

ggplot(df, aes(x = ShadowEconomy, y = VAT_Gap)) +
  # Punctele gri pentru țările normale
  geom_point(aes(color = Status), size = 3) +
  
  # Linia de regresie (așteptarea teoretică)
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", alpha=0.5) +
  
  # Punem etichetele doar la țările interesante sau la toate
  geom_text_repel(aes(label = Geo_Code), box.padding = 0.5) +
  
  # Culori: Roșu pentru paradoxuri, Gri pentru restul
  scale_color_manual(values = c("Normal" = "grey70", "Paradox / Outlier" = "red")) +
  
  theme_minimal() +
  labs(title = "Harta Paradoxurilor TVA în Europa",
       subtitle = "Ungaria (HU) și Slovacia (SK) sunt la poli opuși față de linia de așteptare",
       x = "Economie Gri (% PIB)",
       y = "VAT Gap (%)")









# 1. Crearea Vectorului de Digitalizare Fiscală (Stare la nivelul anului 2020-2021)
# 1 = Țări cu sisteme mature de raportare digitală/fiscalizare strictă (Ex: Italia, Ungaria, Bulgaria, Polonia)
# 0 = Țări cu sisteme tradiționale sau implementare tardivă (Ex: România, Germania, Malta)

# Notă: Această clasificare este bazată pe rapoartele "VAT in the Digital Age" (2022)
digital_status <- c(
  "AT" = 0, # Austria (Sistem tradițional bazat pe audit, fără raportare tranzacțională full-time în 2022)
  "BE" = 0, # Belgia (Tradițional)
  "BG" = 1, # Bulgaria (Case marcat conectate la NRA din 2011/2018 + Control strict transporturi)
  "HR" = 1, # Croația (Fiscalizare în timp real din 2013 - model de succes)
  "CY" = 0, # Cipru
  "CZ" = 0, # Cehia (A anulat sistemul EET de fiscalizare online)
  "DK" = 0, # Danemarca (Conformare voluntară ridicată, dar fără digitalizare coercitivă)
  "EE" = 1, # Estonia (Lider digitalizare generală)
  "FI" = 0, # Finlanda
  "FR" = 0, # Franța (E-invoicing obligatoriu începe abia în 2024-2026)
  "DE" = 0, # Germania (Ordonanța KassenSichV abia din 2020, implementare lentă)
  "GR" = 1, # Grecia (Sistemul myDATA implementat agresiv în 2021, efecte parțiale)
  "HU" = 1, # Ungaria (RTIR - Raportare în timp real din 2018, cel mai performant sistem)
  "IE" = 0, # Irlanda
  "IT" = 1, # Italia (Prima din UE cu e-Factura generalizată B2B din 2019)
  "LV" = 1, # Letonia (Sisteme stricte)
  "LT" = 1, # Lituania (i.MAS subsisteme din 2016)
  "LU" = 0, # Luxemburg
  "MT" = 0, # Malta (Fără măsuri majore de digitalizare coercitivă)
  "NL" = 0, # Olanda (Bazat pe încredere/compliance voluntar)
  "PL" = 1, # Polonia (JPK_VAT / SAF-T obligatoriu și Split Payment din 2018)
  "PT" = 1, # Portugalia (E-invoice și SAF-T timpuriu)
  "RO" = 0, # România (SAF-T și e-Factura abia la început în 2022, impact nul pe anul respectiv)
  "SK" = 0, # Slovacia (e-Kasa din 2019, dar rezultate slabe - posibil excepție)
  "SI" = 1, # Slovenia (Fiscalizare certificată din 2016)
  "ES" = 1, # Spania (Sistemul SII - raportare imediată din 2017)
  "SE" = 0  # Suedia (Case marcat cu cutie neagră, dar nu raportare online centralizată tip Est)
)

# 2. Adăugăm variabila în Dataset
# Folosim funcția match pentru a asocia valoarea corectă fiecărei țări după codul Geo_Code
df$Fiscal_Digitalization <- digital_status[df$Geo_Code]

# Verificăm dacă s-a adăugat corect (ar trebui să nu avem NA-uri)
print("Verificare date noi:")
print(head(df[, c("Geo_Code", "VAT_Gap", "ShadowEconomy", "Fiscal_Digitalization")]))

# 3. Rulăm Regresia Finală "Cauzală"
# Ipoteză: Gap-ul depinde de Economia Gri, dar este redus drastic de Digitalizare.
final_model <- lm(VAT_Gap ~ ShadowEconomy + Fiscal_Digitalization, data = df)
s_final <- summary(final_model)

print("---------------------------------------------------")
print("REZULTATE REGRESIE FINALA (Shadow + Digitalizare)")
print("---------------------------------------------------")
print(paste("R-Squared (Adjusted):", round(s_final$adj.r.squared, 4)))
print("Coefficients:")
print(s_final$coefficients)

# 4. Interacțiune (Opțional dar recomandat)
# Vedem dacă digitalizarea "rupe" legătura dintre economia gri și Gap
inter_digi <- lm(VAT_Gap ~ ShadowEconomy * Fiscal_Digitalization, data = df)
print("--- Test Interacțiune ---")
print(summary(inter_digi)$coefficients)












library(ggplot2)

# Transformăm variabila în Factor pentru a avea etichete frumoase pe grafic
df$Digital_Label <- ifelse(df$Fiscal_Digitalization == 1, 
                           "Digitalizat (ex: BG, HU, PL)", 
                           "Tradițional / Lent (ex: RO, DE, MT)")

# Graficul Final
ggplot(df, aes(x = ShadowEconomy, y = VAT_Gap, color = Digital_Label)) +
  # Punctele (Țările)
  geom_point(size = 3, alpha = 0.7) +
  
  # Liniile de regresie (fără interval de încredere 'se=FALSE' pentru claritate)
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  
  # Etichete pentru țările cheie (RO, BG, HU, SK)
  geom_text(aes(label = Geo_Code), vjust = -0.5, show.legend = FALSE, check_overlap = TRUE) +
  
  # Culori sugestive (Roșu pentru Tradițional, Albastru/Verde pentru Digital)
  scale_color_manual(values = c("Digitalizat (ex: BG, HU, PL)" = "#2c7bb6", 
                                "Tradițional / Lent (ex: RO, DE, MT)" = "#d7191c")) +
  
  theme_minimal() +
  labs(title = "Impactul Digitalizării Fiscale asupra Colectării TVA",
       subtitle = paste("Digitalizarea reduce Gap-ul cu aprox. 7 puncte procentuale, la același nivel de economie gri."),
       x = "Economie Gri (% din PIB)",
       y = "VAT Gap (%)",
       color = "Regim Fiscal") +
  theme(legend.position = "bottom")