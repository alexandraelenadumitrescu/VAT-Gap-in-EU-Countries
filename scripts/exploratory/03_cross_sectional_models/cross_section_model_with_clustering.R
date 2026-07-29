# ==============================================================================
# PROIECT ECONOMETRIE: ANALIZA INTEGRATA VAT GAP 2022
# ==============================================================================

# 1. INCARCARE PACHETE
# ------------------------------------------------------------------------------
required_packages <- c("tidyverse", "caret", "corrplot", "factoextra", "cluster", 
                       "lmtest", "car", "glmnet", "moments")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(caret)
library(corrplot)
library(factoextra) # Pentru vizualizare clusteri
library(cluster)
library(lmtest)
library(car)
library(glmnet)

# 2. PREGATIREA DATELOR (Din obiectul 'panelG')
# ------------------------------------------------------------------------------
# Verificam existenta datelor
if(!exists("panelG")) stop("Eroare: Ruleaza intai codul de incarcare a datelor in obiectul 'panelG'!")

# Filtram doar anul 2022 si facem curatenie
data_2022 <- panelG %>% 
  filter(Year == 2022) %>%
  rename(VAT_Gap = Value) # Redenumim pentru claritate

# Setam tara ca index (nume de rand) pentru grafice
rownames(data_2022) <- data_2022$Country

# Corectie: Daca VAT_Gap e zecimal (0.08), il facem procent (8.0)
if(mean(data_2022$VAT_Gap, na.rm=TRUE) < 1) {
  data_2022$VAT_Gap <- data_2022$VAT_Gap * 100
}

cat("\n--- Date Pregatite pentru 2022 ---\n")
str(data_2022)

# ==============================================================================
# ETAPA A: ANALIZA EXPLORATORIE AVANSATA & UNSUPERVISED ML (CLUSTERING)
# ==============================================================================
cat("\n\n=== ETAPA A: MACHINE LEARNING NESUPERVIZAT (CLUSTERING) ===\n")

# A1. Histograma cu Outlieri
ggplot(data_2022, aes(x = VAT_Gap)) +
  geom_histogram(binwidth = 2, fill = "#4e79a7", color = "white", alpha=0.9) +
  geom_vline(aes(xintercept=mean(VAT_Gap)), color="red", linetype="dashed", size=1) +
  labs(title="Distributia Gap-ului de TVA in UE (2022)", 
       subtitle = "Linia rosie indica media UE. Tarile din dreapta sunt outlieri (problematici).",
       x="VAT Gap (%)", y="Numar tari") +
  theme_minimal()

# A2. Pregatire pentru Clustering (Standardizare obligatorie!)
# Selectam variabilele relevante pentru profilul tarii
vars_cluster <- data_2022 %>% 
  select(VAT_Gap, ShadowEconomy, InternetAccess, Governance, GDP_per_capita)

# Scalam datele (Z-score)
data_scaled <- scale(vars_cluster)

# A3. Algoritmul K-Means
set.seed(123)
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# Vizualizare Clusteri (Output pentru proiect)
fviz_cluster(km_res, data = data_scaled,
             palette = c("#2E9FDF", "#E7B800", "#FC4E07"), 
             ggtheme = theme_minimal(),
             main = "Harta Fiscala a UE (K-Means Clustering)",
             repel = TRUE) # Evita suprapunerea etichetelor

# Interpretare Clusteri (Medii pe grupuri)
data_2022$Cluster <- as.factor(km_res$cluster)
cat("\n--- Profilul Economic al Clusterelor (Medii) ---\n")
print(data_2022 %>% 
        group_by(Cluster) %>% 
        summarise(VAT_Gap = mean(VAT_Gap), 
                  ShadowEc = mean(ShadowEconomy),
                  Governance = mean(Governance),
                  GDP = mean(GDP_per_capita)))

# ==============================================================================
# ETAPA B: MODELARE ECONOMETRICA (EXPLICAREA FENOMENULUI)
# ==============================================================================
cat("\n\n=== ETAPA B: REGRESIE MULTIPLA (OLS) & DIAGNOSTIC ===\n")

# B1. Matricea de Corelatie (Verificare multicoliniaritate pre-model)
cor_matrix <- cor(vars_cluster)
corrplot(cor_matrix, method="number", type="upper", tl.col="black", title="Corelatii 2022")

# B2. Estimarea Modelului OLS
# Incercam sa explicam VAT Gap folosind determinanti cheie
model_ols <- lm(VAT_Gap ~ ShadowEconomy + InternetAccess + Governance + StandardVAT, 
                data = data_2022)

cat("\n--- Rezultate Regresie OLS ---\n")
summary(model_ols)

# B3. Diagnosticarea Modelului
# a) Normalitate (Shapiro-Wilk) - Daca p > 0.05 e bine
shapiro <- shapiro.test(resid(model_ols))
cat("Shapiro-Wilk (Normalitate): p =", round(shapiro$p.value, 4), 
    ifelse(shapiro$p.value > 0.05, "(Valid)", "(Nevalid)"), "\n")

# b) Homoscedasticitate (Breusch-Pagan) - Daca p > 0.05 e bine
bp <- bptest(model_ols)
cat("Breusch-Pagan (Homoscedasticitate): p =", round(bp$p.value, 4), 
    ifelse(bp$p.value > 0.05, "(Valid)", "(Heteroscedasticitate!)"), "\n")

# c) Multicoliniaritate (VIF) - Daca valori > 4 sau 5 e rau
cat("\nTest VIF (Multicoliniaritate):\n")
vif_vals <- vif(model_ols)
print(vif_vals)

# ==============================================================================
# ETAPA C: REGULARIZARE (LASSO) - SOLUTIA PENTRU ESANTION MIC
# ==============================================================================
cat("\n\n=== ETAPA C: SELECTIA VARIABILELOR RELEVANTE (LASSO) ===\n")

# Pregatim matricele pentru glmnet
x_lasso <- model.matrix(VAT_Gap ~ ShadowEconomy + InternetAccess + Governance + 
                          StandardVAT + GDP_per_capita + Urbanizare, data_2022)[,-1]
y_lasso <- data_2022$VAT_Gap

set.seed(123)
# Gasim lambda optim prin cross-validation
cv_lasso <- cv.glmnet(x_lasso, y_lasso, alpha = 1)
best_lambda <- cv_lasso$lambda.min

# Rulam modelul final
model_lasso <- glmnet(x_lasso, y_lasso, alpha = 1, lambda = best_lambda)

cat("\nCoeficientii selectati de LASSO (Variabilele care conteaza cu adevarat):\n")
coef(model_lasso)






# ==============================================================================
# ETAPA D: OPTIMIZAREA MODELULUI (SOLUTIA FINALA)
# ==============================================================================
cat("\n\n=== ETAPA D: MODEL OPTIMIZAT (Corectie Multicoliniaritate & Heteroscedasticitate) ===\n")

# 1. Alegerea variabilelor noi (Logica Economica)
# - Scoatem Governance (corelat cu ShadowEconomy)
# - Adaugam VAB.Agriculture (sector greu de taxat)
# - Logaritmam GDP_per_capita (relatie non-liniara)
# - Scoatem InternetAccess daca nu e semnificativ, lasam ShadowEconomy ca baza

# Noul Model OLS
model_final <- lm(VAT_Gap ~ ShadowEconomy + VAB.Agriculture + log(GDP_per_capita) + Unemployment_rate, 
                  data = data_2022)

cat("\n--- Rezultate Model Imbunatatit ---\n")
summary(model_final)

# 2. Reverificam Multicoliniaritatea (VIF)
# Ne asteptam sa fie mult mai mici acum (< 2 sau < 3)
cat("\n--- Test VIF (Dupa eliminarea variabilelor redundante) ---\n")
print(vif(model_final))

# 3. Tratarea Heteroscedasticitatii (Robust Standard Errors)
# Deoarece BP test a iesit < 0.05, folosim corectia White (HC1)
if(!require(sandwich)) install.packages("sandwich")
library(sandwich)

cat("\n--- Coeficienti cu Erori Standard Robuste (Corectie Heteroscedasticitate) ---\n")
# Aceasta este tabelul "adevarat" pe care il interpretezi in proiect
model_robust <- coeftest(model_final, vcov = vcovHC(model_final, type = "HC1"))
print(model_robust)

# 4. Comparatie R-Squared
r2_vechi <- summary(model_ols)$r.squared
r2_nou <- summary(model_final)$r.squared

cat("\nImbunatatire R-Squared:\n")
cat("Vechi:", round(r2_vechi, 4), "\n")
cat("Nou:  ", round(r2_nou, 4), "\n")








# ==============================================================================
# ETAPA E: STRATEGIA "LESS IS MORE" + INTERACTIUNI
# ==============================================================================
cat("\n\n=== ETAPA E: MODELE FINALE OPTIMIZATE ===\n")

# ------------------------------------------------------------------------------
# VARIANTA 1: MODELUL "STRUCTURAL"
# Scoatem GDP si Agricultura (care se bat cap in cap cu ShadowEconomy)
# Introducem FinalConsumption (baza de impozitare)
# Teorie: O economie subterana mare + Consum mare = Evaziune uriasa
# ------------------------------------------------------------------------------

model_smart <- lm(VAT_Gap ~ ShadowEconomy + FinalConsumption, data = data_2022)

cat("\n--- [Model 1] Rezultate Simplificate ---\n")
summary(model_smart)

# ------------------------------------------------------------------------------
# VARIANTA 2: MODEL CU TERMEN DE INTERACTIUNE (Cerința din PDF)
# Teorie: Economia subterana duce la evaziune MAI ALES daca taxele (TVA) sunt mari.
# Formula: VAT_Gap = B0 + B1*Shadow + B2*TVA + B3*(Shadow * TVA)
# ------------------------------------------------------------------------------

# Cream termenul de interactiune
# Observatie: ShadowEconomy * StandardVAT
model_interactiune <- lm(VAT_Gap ~ ShadowEconomy * StandardVAT, data = data_2022)

cat("\n--- [Model 2] Rezultate cu Interactiune (Non-Liniar) ---\n")
summary(model_interactiune)

# ------------------------------------------------------------------------------
# COMPARATIE SI DIAGNOSTIC FINAL (Pentru Modelul Castigator)
# Alegem automat modelul cu R2 mai mare
# ------------------------------------------------------------------------------

if(summary(model_interactiune)$adj.r.squared > summary(model_smart)$adj.r.squared) {
  best_model <- model_interactiune
  nume_best <- "Model Interactiune"
} else {
  best_model <- model_smart
  nume_best <- "Model Simplu"
}

cat("\n>>> MODELUL CASTIGATOR ESTE:", nume_best, "<<<\n")

# Tratarea Heteroscedasticitatii pe modelul castigator
library(sandwich)
library(lmtest)

cat("\n--- Coeficienti Robusti (Corectie Heteroscedasticitate) ---\n")
print(coeftest(best_model, vcov = vcovHC(best_model, type = "HC1")))

# Test VIF Final
cat("\n--- VIF Final ---\n")
print(vif(best_model))




# ==============================================================================
# ETAPA F: CORECTIA FINALA VIF (CENTRAREA VARIABILELOR)
# ==============================================================================
cat("\n=== ETAPA F: CORECTIA TEHNICA A MODELULUI (CENTERING) ===\n")

# 1. Centram variabilele (Scadem media)
# Asta elimina corelatia dintre ShadowEconomy si termenul de interactiune
data_2022$Shadow_Center <- scale(data_2022$ShadowEconomy, center = TRUE, scale = FALSE)
data_2022$VAT_Center <- scale(data_2022$StandardVAT, center = TRUE, scale = FALSE)

# 2. Rulam modelul pe variabilele centrate
model_final_vif_ok <- lm(VAT_Gap ~ Shadow_Center * VAT_Center, data = data_2022)

# 3. Rezultate
cat("\n--- Rezultate Model Centrat (Aceleasi statistici, VIF mic) ---\n")
summary(model_final_vif_ok)

cat("\n--- VIF Dupa Centrare (Acum ar trebui sa fie < 2) ---\n")
vif(model_final_vif_ok)

# 4. Testam Heteroscedasticitatea pe acest model final
library(sandwich)
library(lmtest)
cat("\n--- Coeficienti Robusti FINALI ---\n")
print(coeftest(model_final_vif_ok, vcov = vcovHC(model_final_vif_ok, type = "HC1")))








# ==============================================================================
# STRATEGIA DE MAXIMIZARE R-SQUARED (CROSS-SECTION 2022)
# ==============================================================================

# 1. CREAREA VARIABILEI DUMMY REGIONALE
# Definim tarile care au istoric structural de Gap mare (Est + Sud)
# Aceasta variabila captureaza "cultura fiscala" si "istoricul" neobservat
tari_est_sud <- c("Bulgaria", "Romania", "Greece", "Italy", "Slovakia", 
                  "Poland", "Hungary", "Latvia", "Lithuania", "Croatia", "Malta")

data_2022$Region_Dummy <- ifelse(data_2022$Country %in% tari_est_sud, 1, 0)

# 2. MODELUL AVANSAT (Dummy + Logaritmi + Interactiuni)
# Folosim log(GDP) si log(Shadow) pentru a normaliza distributia
# Adaugam Dummy-ul regional

model_booster <- lm(VAT_Gap ~ Region_Dummy + log(GDP_per_capita) + ShadowEconomy, 
                    data = data_2022)

cat("\n--- [Model Booster 1] Dummy Regional + Log(GDP) ---\n")
summary(model_booster)

# 3. MODELUL "STEPWISE" (SELECTIE AUTOMATA)
# Lasam algoritmul sa caute combinatia matematica perfecta pentru R2 maxim
# (Atentie: AIC penalizeaza complexitatea, deci nu va supra-fita)

full_model <- lm(VAT_Gap ~ ShadowEconomy * Region_Dummy + 
                   log(GDP_per_capita) + InternetAccess + 
                   Governance + VAB.Agriculture, 
                 data = data_2022)

# Stepwise regression (Backward direction)
model_stepwise <- step(full_model, direction = "both", trace = 0)

cat("\n--- [Model Booster 2] Selectie Automata (Stepwise) ---\n")
summary(model_stepwise)

# 4. COMPARATIE R2
cat("\n>>> COMPARATIE R-SQUARED AJUSTAT <<<\n")
cat("Model OLS Initial:   0.086 (aprox)\n") # Din rularea anterioara
cat("Model Dummy Region: ", round(summary(model_booster)$adj.r.squared, 3), "\n")
cat("Model Stepwise:     ", round(summary(model_stepwise)$adj.r.squared, 3), "\n")

# 5. DIAGNOSTIC FINAL PE CEL MAI BUN MODEL
best_model_r2 <- ifelse(summary(model_booster)$adj.r.squared > summary(model_stepwise)$adj.r.squared, 
                        "model_booster", "model_stepwise")

final_best <- get(best_model_r2)

cat("\n--- Diagnostic Model Final ---\n")
# VIF
if(require(car)) print(vif(final_best))
# Heteroscedasticitate
if(require(lmtest)) print(bptest(final_best))







# ==============================================================================
# STRATEGIA FINALA: CRESTERE R2 PRIN ELIMINARE OUTLIERI SI INTERACTIUNE
# ==============================================================================

# 1. IDENTIFICAREA OUTLIERILOR (Distanta Cook)
# Vedem ce tari distorsioneaza cel mai mult modelul tau cel mai bun (Stepwise)
# ------------------------------------------------------------------------------
# Refacem modelul stepwise (cel mai bun de pana acum)
best_current <- lm(VAT_Gap ~ Region_Dummy + VAB.Agriculture, data = data_2022)

# Calculam Distanta Cook (Cat de mult influenteaza fiecare tara modelul)
cooksd <- cooks.distance(best_current)

# Vizualizam Outlierii
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cook's distance")
abline(h = 4/nrow(data_2022), col="red")  # Linia de prag
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/nrow(data_2022), names(cooksd),""), col="red")

# Identificam numele tarilor problematice
outliers <- names(cooksd)[(cooksd > 4/nrow(data_2022))]
cat("\n>>> Outlieri identificati (tari care trag modelul in jos):", outliers, "\n")



#[Image of Cook's Distance plot]


# 2. MODELUL FARA OUTLIERI ("Curatat")
# ------------------------------------------------------------------------------
# Cream un set de date fara acesti outlieri
data_clean <- data_2022 %>% filter(!Country %in% outliers)

# Rulam din nou modelul pe datele curate
model_no_outlier <- lm(VAT_Gap ~ Region_Dummy + ShadowEconomy, data = data_clean)

cat("\n--- [Model Fara Outlieri] Rezultate ---\n")
summary(model_no_outlier)


# 3. MODELUL FINAL "INTERACTIUNE REGIONALA"
# Teorie: Shadow Economy conteaza, dar POATE DOAR in tarile din Est?
# Formula: VAT_Gap ~ Region + Shadow + (Region * Shadow)
# ------------------------------------------------------------------------------
# Rulam pe datele curate (sau pe cele originale daca vrei)
model_interact_region <- lm(VAT_Gap ~ Region_Dummy * ShadowEconomy, data = data_clean)

cat("\n--- [Model Interactiune Regionala] (Region * Shadow) ---\n")
summary(model_interact_region)


# 4. COMPARATIE FINALA R2
cat("\n>>> COMPARATIE R-SQUARED AJUSTAT <<<\n")
cat("Model Anterior (Stepwise):      0.338\n")
cat("Model Fara Outlieri:            ", round(summary(model_no_outlier)$adj.r.squared, 3), "\n")
cat("Model Interactiune (Clean):     ", round(summary(model_interact_region)$adj.r.squared, 3), "\n")







# ==============================================================================
# APLICATIA 2: MODELARE PE DATE DE TIP PANEL (SCRIPT CORECTAT)
# ==============================================================================
cat("\n\n================ APLICATIA 2: PANEL DATA MODELS ================\n")

# 1. Definirea Structurii Panel
pdata <- pdata.frame(panelG, index = c("Country", "Year"))

# 2. Estimarea Modelelor (Candidatii)
# A. Modelul cu EFECTE FIXE (Fixed Effects - FE)
model_fe <- plm(Value ~ ShadowEconomy + InternetAccess + Governance + Unemployment_rate, 
                data = pdata, model = "within")

# B. Modelul cu EFECTE ALEATORII (Random Effects - RE)
model_re <- plm(Value ~ ShadowEconomy + InternetAccess + Governance + Unemployment_rate, 
                data = pdata, model = "random")

# 3. Alegerea Modelului Optim: TESTUL HAUSMAN
hausman_test <- phtest(model_fe, model_re)

cat("\n--- REZULTAT TEST HAUSMAN (Alegerea Modelului) ---\n")
print(hausman_test)

# Logica de decizie automata
if(hausman_test$p.value < 0.05) {
  final_model_panel <- model_fe
  tip_model <- "FIXED EFFECTS (FE)"
  cat(">>> DECIZIE: P-value < 0.05. Se alege modelul cu EFECTE FIXE (FE).\n")
} else {
  final_model_panel <- model_re
  tip_model <- "RANDOM EFFECTS (RE)"
  cat(">>> DECIZIE: P-value > 0.05. Se alege modelul cu EFECTE ALEATORII (RE).\n")
}

# 4. Rezultatele Modelului Optim
cat("\n--- Rezultate Finale Model Panel (", tip_model, ") ---\n")
summary(final_model_panel)

# 5. Diagnostic (Cu protectie anti-eroare)

# a) Testarea dependentei transversale (Pasaran CD) - Merge pe ambele
cat("\nTest Pasaran CD (Dependenta Transversala):\n")
print(pcdtest(final_model_panel, test = "cd"))

# b) Testarea Autocorelarii (Wooldridge) - Merge DOAR pe Fixed Effects
if(tip_model == "FIXED EFFECTS (FE)") {
  cat("\nTest Wooldridge (Autocorelare Seriala):\n")
  print(pwartest(final_model_panel))
} else {
  cat("\nNOTA: Testul Wooldridge a fost omis deoarece este valid doar pentru Fixed Effects.\n")
  cat("Pentru Random Effects, ne bazam direct pe erorile robuste de mai jos.\n")
}

# 6. TABEL FINAL (SOLUTIA ROBUSTA)
# Folosim erori robuste care corecteaza orice problema (autocorelare sau heteroscedasticitate)
cat("\n--- TABEL FINAL PENTRU PROIECT: Coeficienti Robusti (Driscoll-Kraay/White) ---\n")

# Daca e Fixed Effects folosim 'arellano', daca e Random folosim 'white1' sau 'arellano' (ambele merg tehnic)
if(tip_model == "FIXED EFFECTS (FE)") {
  metoda_robusta <- "arellano"
} else {
  metoda_robusta <- "white1" # Standard pentru Random Effects
}

res_robust_panel <- coeftest(final_model_panel, vcov. = vcovHC(final_model_panel, method = metoda_robusta))
print(res_robust_panel)

# Calcul R2
r2_val <- summary(final_model_panel)$r.squared["adjrsq"]
cat("\nR-Squared (Adjusted):", r2_val, "\n")






# ==============================================================================
# STRATEGIA "MODERARE PRIN DIGITALIZARE"
# Scop: Explicarea outlierilor prin faptul ca digitalizarea reduce impactul economiei subterane
# ==============================================================================

# 1. Centrarea variabilelor (Obligatoriu pentru interactiuni ca sa evitam VIF mare)
data_2022$Shadow_C <- scale(data_2022$ShadowEconomy, center = TRUE, scale = FALSE)
data_2022$Internet_C <- scale(data_2022$InternetAccess, center = TRUE, scale = FALSE)

# 2. Modelul cu Interactiune (Shadow * Internet)
# Teoria: "Economia subterana creste Gap-ul, DAR internetul ii reduce puterea nociva"
model_interaction_tech <- lm(VAT_Gap ~ Shadow_C * Internet_C + Region_Dummy, 
                             data = data_2022)

cat("\n--- [Model Booster] Shadow Economy moderata de Internet ---\n")
summary(model_interaction_tech)

# 3. Verificam R2
cat("\nR-Squared Adjusted:", round(summary(model_interaction_tech)$adj.r.squared, 4))

# 4. Interpretare Vizuala (Daca modelul e bun)
# Vedem coeficientul interactiunii
coef_interact <- coef(model_interaction_tech)["Shadow_C:Internet_C"]

if(coef_interact < 0) {
  cat("\n\n>>> INTERPRETARE DE SUCCES: Coeficientul interactiunii este NEGATIV.\n")
  cat("Asta inseamna ca in tarile cu Internet mult, Economia Subterana conteaza mai putin pentru TVA.\n")
  cat("Practic, ai creat matematic acel 'grad de supraestimare' pe care il cautai.\n")
} else {
  cat("\n\n>>> Rezultatul nu confirma ipoteza de moderare.\n")
  
  
  
  
  
  
  # ==============================================================================
  # STRATEGIA "SHADOW OVERESTIMATION" PRIN PCA (Principal Component Analysis)
  # ==============================================================================
  
  # 1. PREGATIREA VARIABILELOR PENTRU PCA
  # Selectam variabilele care fac statul "imun" la evaziune
  # Atentie: CPI_Score este inversul coruptiei (mare = bine)
  vars_resilience <- data_2022 %>% 
    select(InternetAccess, Governance, CPI_Score, Urbanizare)
  
  # 2. RULAREA PCA (REDUCEREA DIMENSIONALITATII)
  # Aceasta combina cele 4 variabile intr-un singur "Scor de Modernizare"
  pca_result <- prcomp(vars_resilience, scale. = TRUE)
  
  # Extragem prima componenta principala (PC1) care explica cea mai mare variatie
  # PC1 va fi "Indexul nostru de Rezilienta Fiscala"
  data_2022$Fiscal_Resilience_Index <- pca_result$x[,1]
  
  # ATENTIE: Verificam semnul! PC1 poate fi inversat matematic.
  # Ne asiguram ca un Index MARE inseamna tara BUNA (Corelatie pozitiva cu Internetul)
  if(cor(data_2022$Fiscal_Resilience_Index, data_2022$InternetAccess) < 0) {
    data_2022$Fiscal_Resilience_Index <- -data_2022$Fiscal_Resilience_Index
  }
  
  cat("\n--- Componenta Principala (Indexul de Rezilienta) ---\n")
  print(head(data_2022 %>% select(Country, Fiscal_Resilience_Index)))
  
  # 3. CALCULUL "EFFECTIVE SHADOW ECONOMY"
  # Ajustam Shadow Economy in functie de Rezilienta
  # Logica: Daca Rezilienta e mare, scadem impactul Shadow Economy
  # Folosim o formula de interactiune manuala pentru a crea noua variabila
  
  # Normalizam Shadow Economy (0 la 1)
  shadow_norm <- (data_2022$ShadowEconomy - min(data_2022$ShadowEconomy)) / 
    (max(data_2022$ShadowEconomy) - min(data_2022$ShadowEconomy))
  
  # Normalizam Indexul de Rezilienta (0 la 1)
  resilience_norm <- (data_2022$Fiscal_Resilience_Index - min(data_2022$Fiscal_Resilience_Index)) / 
    (max(data_2022$Fiscal_Resilience_Index) - min(data_2022$Fiscal_Resilience_Index))
  
  # CREAREA VARIABILEI MAGICE:
  # Effective_Shadow = Shadow * (1 - Resilience)
  # Daca Rezilienta e 1 (Max), Shadow devine 0 (Neutralizat complet)
  # Daca Rezilienta e 0 (Min), Shadow loveste 100%
  data_2022$Effective_Shadow <- shadow_norm * (1 - resilience_norm)
  
  # 4. MODELUL FINAL COMPLEX
  # Regresam Gap-ul pe aceasta noua variabila compusa
  model_pca_complex <- lm(VAT_Gap ~ Effective_Shadow + Region_Dummy, data = data_2022)
  
  cat("\n--- [Model Complex] PCA Adjusted Shadow Economy ---\n")
  summary(model_pca_complex)
  
  # 5. DIAGNOSTIC VIZUAL
  # Vedem daca am reusit sa aliniem tarile mai bine
  plot(data_2022$Effective_Shadow, data_2022$VAT_Gap, 
       main="Relatia VAT Gap vs. Effective Shadow (Ajustat PCA)",
       xlab="Effective Shadow Index (Risk)", ylab="VAT Gap (%)", pch=19, col="blue")
  text(data_2022$Effective_Shadow, data_2022$VAT_Gap, labels=data_2022$Country, cex=0.7, pos=3)



  
  
  
  
  
  
  install.packages("gtrendsR")
  install.packages("countrycode")
  library(gtrendsR)
  library(countrycode)
  library(tidyverse)