# ==============================================================================
# PASUL 0: IMPORTAREA BIBLIOTECILOR
# ==============================================================================
library(readr)      # Pentru citirea rapida a CSV-ului
library(dplyr)      # Pentru manipularea datelor
library(plm)        # Pentru modele de date PANEL (Efecte Fixe)
library(corrplot)   # Pentru vizualizarea corelatiilor
library(lmtest)     # Pentru testare statistica avansata
library(sandwich)   # Pentru erori standard robuste (Clustered Standard Errors)

# ==============================================================================
# PASUL 1: ÎNCĂRCAREA ȘI PREGĂTIREA DATELOR
# ==============================================================================
# Inlocuieste cu calea catre fisierul tau. Daca e in folderul proiectului, doar numele:
df <- read_csv("panel.csv") 

# Ne asiguram ca nu avem valori lipsa (NA)
df <- na.omit(df)

# Definim structura de PANEL (Tara si Anul sunt index)
pdata <- pdata.frame(df, index = c("Country", "Year"))

# ==============================================================================
# PASUL 2: DIAGNOSTICUL COLINIARITĂȚII (De ce facem PCA?)
# ==============================================================================
# Selectam variabilele suspecte de coliniaritate
vars_pca <- df %>% 
  select(GDP_per_capita, InternetAccess, Urbanizare, FinalConsumption)

# Calculam matricea de corelatie
cor_matrix <- cor(vars_pca)
print(cor_matrix)

# Vizualizam corelatia (Optional)
corrplot(cor_matrix, method = "number", type = "upper")
# Daca vezi valori peste 0.7 sau sub -0.7, PCA este justificat!

# ==============================================================================
# PASUL 3: APLICAREA PCA (Analiza Componentelor Principale)
# ==============================================================================
# Rulam PCA. Argumentul 'scale. = TRUE' este OBLIGATORIU (standardizare)
pca_result <- prcomp(vars_pca, scale. = TRUE)

# Vedem cat explica fiecare componenta (Summary)
summary(pca_result)
# Uita-te la linia "Cumulative Proportion" pentru PC1. Ar trebui sa fie > 60-70%.

# Extragem prima componenta (PC1) care va fi "Indexul nostru de Dezvoltare"
# ATENTIE: Semnul PC1 este arbitrar in matematica. 
# Trebuie sa verificam corelatia cu GDP ca sa stim directia.
df$Index_Dezvoltare_Raw <- pca_result$x[, 1]

# Verificam directia:
cor_check <- cor(df$Index_Dezvoltare_Raw, df$GDP_per_capita)
print(paste("Corelatia dintre Index si GDP este:", round(cor_check, 2)))

# LOGICA DE SEMN: 
# Daca corelatia e negativa (ex: -0.9), inseamna ca valori mici ale PC1 = Dezvoltare Mare.
# Ca sa fie intuitiv (Index Mare = Dezvoltare Mare), il inmultim cu -1 daca e nevoie.
if(cor_check < 0) {
  df$Index_Dezvoltare <- df$Index_Dezvoltare_Raw * -1
} else {
  df$Index_Dezvoltare <- df$Index_Dezvoltare_Raw
}

# ==============================================================================
# PASUL 4: ESTIMAREA MODELULUI PANEL (FIXED EFFECTS)
# ==============================================================================
# Actualizam obiectul panel cu noua variabila creata
pdata <- pdata.frame(df, index = c("Country", "Year"))

# Formula modelului
# Value (VAT Gap) este variabila dependenta
formula_model <- Value ~ Index_Dezvoltare + ShadowEconomy + StandardVAT + 
  VAB.Agriculture + CPI_Score + Unemployment_rate

# Rulam modelul cu Efecte Fixe ("within")
fe_model <- plm(formula_model, data = pdata, model = "within")

# Afisam rezultatele standard
summary(fe_model)

# ==============================================================================
# PASUL 5: TESTE ȘI ERORI ROBUSTE (Pentru nota 10 la metodologie)
# ==============================================================================
# Testam daca Efectele Fixe sunt necesare vs OLS simplu (Testul F pentru efecte individuale)
# Daca p-value < 0.05, EFECTELE FIXE SUNT OBLIGATORII (ceea ce e bine pentru tine)
pFtest(fe_model, plm(formula_model, data = pdata, model = "pooling"))

# CORECTIA FINALĂ: Erori Standard Clustered (Robuste la heteroscedasticitate si autocorelare)
# Aceasta este iesirea pe care o pui in lucrare!
robust_results <- coeftest(fe_model, vcov = vcovHC(fe_model, type = "HC1", cluster = "group"))

print("REZULTATELE FINALE CU ERORI ROBUSTE:")
print(robust_results)

# Interpretare rapida: 
# Pr(>|t|) < 0.05 inseamna ca variabila este semnificativa statistic (are stelute ***)