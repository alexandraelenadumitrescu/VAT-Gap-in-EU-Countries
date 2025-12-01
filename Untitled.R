install.packages(c("eurostat", "dplyr", "tidyr", "readxl"))

library(eurostat)
library(dplyr)
library(tidyr)

# Curățăm memoria
clean_eurostat_cache()

# Funcție sigură de procesare
process_eurostat_data <- function(dataset_id) {
  data <- get_eurostat(dataset_id)
  if ("TIME_PERIOD" %in% names(data)) {
    data <- data %>% rename(time = TIME_PERIOD)
  } else if ("obsTime" %in% names(data)) {
    data <- data %>% rename(time = obsTime)
  }
  return(data)
}

TARGET_YEAR <- 2021
eu_countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", 
                  "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                  "NL", "PL", "PT", "RO", "SE", "SI", "SK")

cat(">>> REPARARE DATE PENTRU ANUL", TARGET_YEAR, "<<<\n")

# --- 1. PIB per Capita (Folosim nama_10_pc - Main aggregates per capita) ---
cat("1. Descarc GDP (Sursa Alternativa nama_10_pc)...\n")
# Folosim Prețuri Curente (CP_EUR_HAB) care sunt mereu disponibile
gdp_data <- process_eurostat_data("nama_10_pc") %>%
  mutate(year = as.numeric(substr(as.character(time), 1, 4))) %>%
  filter(unit == "CP_EUR_HAB", # Prețuri curente în Euro pe cap de locuitor
         na_item == "B1GQ",    # Produs Intern Brut
         year == TARGET_YEAR) %>%
  select(geo, values) %>%
  rename(GDP_per_Capita = values)

# --- 2. Rata Șomajului (A mers, păstrăm codul) ---
cat("2. Descarc Somaj...\n")
unemployment_data <- process_eurostat_data("une_rt_a") %>%
  mutate(year = as.numeric(substr(as.character(time), 1, 4))) %>%
  filter(age == "Y15-74", unit == "PC_ACT", sex == "T", year == TARGET_YEAR) %>%
  select(geo, values) %>%
  rename(Unemployment_Rate = values)

# --- 3. Acces Internet (A mers, păstrăm codul) ---
cat("3. Descarc Internet...\n")
internet_data <- process_eurostat_data("isoc_ci_in_h") %>%
  mutate(year = as.numeric(substr(as.character(time), 1, 4))) %>%
  filter(unit == "PC_HH", hhtyp == "TOTAL", year == TARGET_YEAR) %>%
  select(geo, values) %>%
  rename(Internet_Access = values)

# --- 4. Ponderea Agriculturii (Folosim nama_10_a10 - Agregate pe 10 ramuri) ---
cat("4. Descarc Agricultura (Sursa Alternativa nama_10_a10)...\n")
# Folosim CP_MEUR (Milioane Euro Prețuri Curente)
agri_raw <- process_eurostat_data("nama_10_a10") %>%
  mutate(year = as.numeric(substr(as.character(time), 1, 4))) %>%
  filter(unit == "CP_MEUR", 
         year == TARGET_YEAR)

# A. Agricultura ("A")
data_agriculture <- agri_raw %>%
  filter(nace_r2 == "A") %>%
  group_by(geo) %>% summarise(Val_Agri = sum(values, na.rm=TRUE)) 

# B. Total Economie - Valoare Adăugată Brută ("TOTAL" sau sumă ramuri)
# Mai sigur: luăm GDP-ul total din nama_10_gdp pentru a fi siguri de numitor
gdp_total_raw <- process_eurostat_data("nama_10_gdp") %>%
  mutate(year = as.numeric(substr(as.character(time), 1, 4))) %>%
  filter(unit == "CP_MEUR", na_item == "B1GQ", year == TARGET_YEAR) %>%
  select(geo, values) %>%
  rename(Val_Total = values)

# C. Unim și calculăm
agri_share_final <- full_join(data_agriculture, gdp_total_raw, by = "geo") %>%
  mutate(Agriculture_Share = (Val_Agri / Val_Total) * 100) %>%
  select(geo, Agriculture_Share)

# --- 5. UNIREA FINALĂ ---
cat("5. Unire finală...\n")
dataset_2021 <- gdp_data %>%
  full_join(unemployment_data, by = "geo") %>%
  full_join(internet_data, by = "geo") %>%
  full_join(agri_share_final, by = "geo") %>%
  filter(geo %in% eu_countries) %>%
  mutate(Year = TARGET_YEAR) %>%
  select(geo, Year, everything()) %>%
  arrange(geo)

# Verificare - Ar trebui să nu mai ai NA-uri
print(head(dataset_2021))

# Salvare
write.csv(dataset_2021, "Date_Proiect_UE_2021_Final_V2.csv", row.names = FALSE)
cat("Gata! Verifică fișierul 'Date_Proiect_UE_2021_Final_V2.csv'.")





# --- PAS DE REPARARE (Rulează asta acum) ---

# 1. Transformăm virgula în punct (în caz că Excelul a pus virgule)
data$VAT_Gap <- as.numeric(gsub(",", ".", as.character(data$VAT_Gap)))
data$CPI <- as.numeric(gsub(",", ".", as.character(data$CPI)))
data$VAT_Rate <- as.numeric(gsub(",", ".", as.character(data$VAT_Rate)))

# 2. Verificăm din nou structura
str(data) 
# ACUM ar trebui să vezi "num" (numeric) în dreptul la VAT_Gap, nu "chr" (character).

# 3. Recalculăm setul curat pentru analiză
data_clean <- data %>% 
  select(GDP_per_Capita, Unemployment_Rate, Internet_Access, 
         Agriculture_Share, VAT_Gap, CPI, VAT_Rate)

cat("Datele au fost reparate! Acum sunt numerice.\n")



# --- PASUL 1: INSTALARE PACHETE (Dacă lipsesc) ---
packages <- c("ggplot2", "corrplot", "factoextra", "cluster", "psych", "dplyr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(ggplot2)
library(corrplot)
library(factoextra) # Pentru vizualizare clusteri
library(cluster)
library(dplyr)

# --- PASUL 2: ÎNCĂRCAREA DATELOR COMPLETE ---
# Citim fișierul pe care l-ai pregătit (cel din screenshot)
data <- read.csv("Date_Proiect_UE_2021_Final.csv")

# Verificăm structura să fim siguri că R vede numerele ca numere
str(data)

# Setăm codul țării ca nume de rând (ca să apară pe grafice)
rownames(data) <- data$geo

# Creăm un set de date strict numeric pentru analiză (scoatem geo și Year)
# Selectăm coloanele relevante: GDP, Unemployment, Internet, Agriculture, VAT_Gap, CPI, VAT_Rate
data_clean <- data %>% 
  select(GDP_per_Capita, Unemployment_Rate, Internet_Access, 
         Agriculture_Share, VAT_Gap, CPI, VAT_Rate)

# --- PASUL 3: STATISTICI DESCRIPTIVE (Cerința 2a, 2b) ---
cat("\n=== Statistici Descriptive ===\n")
print(summary(data_clean))

# Histograma Variabilei Dependente (VAT GAP)
hist_plot <- ggplot(data, aes(x=VAT_Gap)) + 
  geom_histogram(binwidth=3, fill="#4e79a7", color="white", alpha=0.8) +
  geom_vline(aes(xintercept=mean(VAT_Gap)), color="red", linetype="dashed", size=1) +
  labs(title="Distribuția Gap-ului de TVA în țările UE (2021)", 
       subtitle = paste("Media UE:", round(mean(data$VAT_Gap), 2), "%"),
       x="VAT Gap (%)", y="Număr țări") +
  theme_minimal()
print(hist_plot)

# --- PASUL 4: MATRICEA DE CORELAȚIE (Cerința 2b) ---
# Vedem legăturile dintre variabile
M <- cor(data_clean)
corrplot(M, method="color", type="upper", 
         addCoef.col = "black", # Adaugă cifrele pe grafic
         tl.col="black", tl.srt=45, # Culoarea textului
         number.cex = 0.7,
         title="Matricea de Corelație a Variabilelor", mar=c(0,0,1,0))

# --- PASUL 5: MACHINE LEARNING - CLUSTERING (Cerința 2e) ---

# A. Standardizarea datelor (OBLIGATORIU pentru K-Means)
# Altfel PIB-ul (40.000) ar domina complet Gap-ul (5)
data_scaled <- scale(data_clean)

# B. Determinarea numărului optim de clustere (Elbow Method)
elbow_plot <- fviz_nbclust(data_scaled, kmeans, method = "wss") +
  labs(title = "Numărul optim de clustere (Elbow Method)")
print(elbow_plot)

# C. Rularea algoritmului K-Means
# Vom alege k=3 clustere (Tipic: Performeri, Medii, Problematici)
set.seed(123) # Pentru rezultate reproductibile
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# D. Vizualizarea Clusterelor (Harta țărilor)
cluster_plot <- fviz_cluster(km_res, data = data_scaled,
                             palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Culori distincte
                             ggtheme = theme_minimal(),
                             main = "Harta Fiscală a UE - Gruparea țărilor (K-Means)",
                             repel = TRUE) # Evită suprapunerea textului
print(cluster_plot)

# E. Vedem cine face parte din fiecare cluster
cat("\n=== Componența Clusterelor ===\n")
print(km_res$cluster)

# F. Analiza caracteristicilor fiecărui cluster (Media pe grupuri)
# Asta te ajută să scrii concluziile ("Clusterul 1 are PIB mare și Gap mic")
aggregate(data_clean, by=list(cluster=km_res$cluster), mean)


# --- PASUL 1: Reîncărcăm și curățăm din nou datele ---
data <- read.csv("Date_Proiect_UE_2021_Final.csv")
rownames(data) <- data$geo

# Reparăm formatele (Text -> Număr)
data$VAT_Gap <- as.numeric(gsub(",", ".", as.character(data$VAT_Gap)))
data$CPI <- as.numeric(gsub(",", ".", as.character(data$CPI)))
data$VAT_Rate <- as.numeric(gsub(",", ".", as.character(data$VAT_Rate)))

# Selectăm doar coloanele numerice
library(dplyr)
data_clean <- data %>% 
  select(GDP_per_Capita, Unemployment_Rate, Internet_Access, 
         Agriculture_Share, VAT_Gap, CPI, VAT_Rate)

# --- PASUL CRITIC: Eliminarea valorilor NA ---
# Verificăm câte NA-uri avem
cat("Număr de valori lipsă (NA) găsite:", sum(is.na(data_clean)), "\n")

# Eliminăm rândurile care conțin NA (K-Means nu merge altfel)
data_clean <- na.omit(data_clean)

# Verificăm din nou
cat("Număr de țări rămase după curățare:", nrow(data_clean), "\n")

# --- PASUL 2: Standardizare și K-Means (Acum va merge sigur) ---

# A. Standardizare
data_scaled <- scale(data_clean)

# B. Rulare K-Means
library(factoextra)
set.seed(123)
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# C. Vizualizare
fviz_cluster(km_res, data = data_scaled,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Harta Fiscală a UE - Gruparea țărilor",
             repel = TRUE)













# --- PASUL A: PREGĂTIRE ---
library(ggplot2)
library(corrplot)
library(factoextra)
library(cluster)
library(dplyr)

# Încărcăm datele corectate
data <- read.csv("Date_Proiect_UE_2021_Final.csv")
rownames(data) <- data$geo

# Selectăm variabilele numerice pentru analiză
# Observă că am scos 'Year' și 'geo' din selecție
data_clean <- data %>% 
  select(GDP_per_Capita, Unemployment_Rate, Internet_Access, 
         Agriculture_Share, VAT_Gap, CPI, VAT_Rate)

# Verificăm că totul e numeric (Trebuie să vezi 'num' sau 'int' peste tot)
str(data_clean)

# --- PASUL B: STATISTICI DESCRIPTIVE (Cerința 2 din PDF) ---
cat("\n=== Rezumat Statistic ===\n")
print(summary(data_clean))

# 1. Histograma pentru VAT GAP (Variabila Dependentă)
ggplot(data, aes(x=VAT_Gap)) + 
  geom_histogram(binwidth=2, fill="#4e79a7", color="white", alpha=0.9) +
  geom_vline(aes(xintercept=mean(VAT_Gap)), color="red", linetype="dashed", linewidth=1) +
  labs(title="Distribuția Gap-ului de TVA în UE (2021)", 
       subtitle = "Majoritatea țărilor sunt sub 5%, dar există outliers (RO, MT, EL, LT)",
       x="VAT Gap (%)", y="Număr țări") +
  theme_minimal()

# --- PASUL C: CORELAȚII ---
# 2. Matricea de corelație
M <- cor(data_clean)
corrplot(M, method="color", type="upper", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45, 
         number.cex = 0.7,
         title="Matricea de Corelație", mar=c(0,0,1,0))

# --- PASUL D: K-MEANS CLUSTERING (Cerința de ML Exploratoriu) ---
# Standardizăm datele (Z-score)
data_scaled <- scale(data_clean)

# Setăm seed pentru reproductibilitate
set.seed(123)

# Rulăm K-Means cu 3 clustere
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# 3. Vizualizarea Clusterelor
fviz_cluster(km_res, data = data_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             ggtheme = theme_minimal(),
             main = "Clustere Fiscale în UE-27",
             repel = TRUE) +
  labs(subtitle = "Gruparea țărilor pe baza indicatorilor economici și fiscali")

# Vedem mediile pe fiecare cluster ca să le putem interpreta în proiect
cat("\n=== Interpretarea Clusterelor (Medii) ===\n")
print(aggregate(data_clean, by=list(cluster=km_res$cluster), mean))









# 1. Citim fișierul și verificăm primele rânduri
data <- read.csv("Date_Proiect_UE_2021_Final.csv")
cat("Verificare separator virgulă:\n")
print(head(data))

# Dacă vezi totul îngrămădit, încercăm cu punct și virgulă
if (ncol(data) <= 1) {
  cat("\n⚠️ Se pare că separatorul e greșit. Încercăm cu punct și virgulă...\n")
  data <- read.csv("Date_Complete_Analiza.csv", sep = ";")
}

# 2. Verificăm numele coloanelor
cat("\nColoanele găsite sunt:\n")
print(colnames(data))

# 3. Dacă tot nu merge, curățăm numele coloanelor (eliminăm spații sau caractere ciudate)
names(data) <- trimws(names(data)) # Eliminăm spații
# Redenumim forțat pentru a fi siguri
if(ncol(data) >= 7) { # Doar dacă avem destule coloane
  # Încercăm să mapăm coloanele principale
  # Atenție: ordinea trebuie să fie cea din fișier
  colnames(data)[1] <- "geo"
  # Căutăm coloana cu VAT Gap
  gap_idx <- grep("Gap", colnames(data), ignore.case = TRUE)
  if(length(gap_idx) > 0) colnames(data)[gap_idx] <- "VAT_Gap"
  
  cpi_idx <- grep("CPI", colnames(data), ignore.case = TRUE)
  if(length(cpi_idx) > 0) colnames(data)[cpi_idx] <- "CPI"
  
  rate_idx <- grep("Rate", colnames(data), ignore.case = TRUE)
  # Atenție, Unemployment_Rate e prima, VAT_Rate e ultima
  if(length(rate_idx) > 1) colnames(data)[tail(rate_idx, 1)] <- "VAT_Rate"
}

# 4. Acum reîncercăm selecția
library(dplyr)
# Verificăm dacă VAT_Gap există acum
if ("VAT_Gap" %in% names(data)) {
  cat("\n✅ Coloana VAT_Gap a fost găsită! Continuăm analiza.\n")
  
  data_clean <- data %>% 
    select(GDP_per_Capita, Unemployment_Rate, Internet_Access, 
           Agriculture_Share, VAT_Gap, CPI, VAT_Rate)
  
  # Afișăm sumarul pentru confirmare
  print(summary(data_clean))
  
} else {
  stop("❌ EROARE CRITICĂ: Tot nu găsesc coloana 'VAT_Gap'. Te rog deschide fișierul CSV în Notepad și verifică dacă are virgule între valori.")
}



















