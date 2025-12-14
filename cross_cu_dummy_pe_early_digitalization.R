# ==============================================================================
# PROIECT ECONOMETRIE: ANALIZA VAT GAP ÎN UE (2022)
# ==============================================================================

# 1. PREGĂTIREA MEDIULUI ȘI A PACHETELOR
# ------------------------------------------------------------------------------
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr") # Pentru transformarea datelor
if(!require(ggrepel)) install.packages("ggrepel") # Pentru etichete care nu se suprapun

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

# 2. IMPORTUL DATELOR
# ------------------------------------------------------------------------------
# Calea specificată de tine
file_path <- "C:/Users/alexa/Documents/proiect-econometrie/Date_Proiect_UE_GoogleTrends_2022.csv"

if (!file.exists(file_path)) {
  stop("Eroare: Fișierul nu a fost găsit. Verifică dacă calea este corectă.")
}

df <- read.csv(file_path, stringsAsFactors = FALSE)
print("-> Datele au fost încărcate cu succes.")

# 3. CONSTRUCȚIA VARIABILEI DUMMY: 'FISCAL_DIGITALIZATION'
# ------------------------------------------------------------------------------
# 1 = Sisteme digitale mature înainte de 2021 (ex: BG, HU, IT, PL, ES)
# 0 = Sisteme tradiționale sau implementare tardivă în 2022 (ex: RO, DE, MT)

digital_status <- c(
  "AT" = 0, "BE" = 0, "BG" = 1, "HR" = 1, "CY" = 0, "CZ" = 0, "DK" = 0, 
  "EE" = 1, "FI" = 0, "FR" = 0, "DE" = 0, "GR" = 1, "HU" = 1, "IE" = 0, 
  "IT" = 1, "LV" = 1, "LT" = 1, "LU" = 0, "MT" = 0, "NL" = 0, "PL" = 1, 
  "PT" = 1, "RO" = 0, "SK" = 0, "SI" = 1, "ES" = 1, "SE" = 0
)

# Adăugăm variabila în dataset
df$Fiscal_Digitalization <- digital_status[df$Geo_Code]

# Creăm o etichetă text pentru grafice (ca să arate frumos în legendă)
df$Regim_Fiscal <- ifelse(df$Fiscal_Digitalization == 1, 
                          "Digitalizat / Strict (ex: BG, HU, IT)", 
                          "Traditional / Lent (ex: RO, DE)")

# 4. MODELUL ECONOMETRIC FINAL
# ------------------------------------------------------------------------------
# Modelăm VAT Gap în funcție de Economia Gri și existența Digitalizării
final_model <- lm(VAT_Gap ~ ShadowEconomy + Fiscal_Digitalization, data = df)
s_final <- summary(final_model)

print("===================================================")
print("REZULTATE REGRESIE FINALA")
print("===================================================")
print(paste("R-Squared (Adjusted):", round(s_final$adj.r.squared, 4)))
print("Coeficienți:")
print(s_final$coefficients)
print("---------------------------------------------------")


# 5. GRAFICUL 1: PARADOXUL BALCANIC (RO vs BG vs UE)
# ------------------------------------------------------------------------------
# Acest grafic demonstrează că structura economiei (Gri/Corupție) nu dictează rezultatul final.

# Pregătim datele
target_countries <- c("RO", "BG")

# Calculăm media UE (excluzând RO și BG pentru comparație corectă)
eu_avg <- df %>%
  filter(!Geo_Code %in% target_countries) %>%
  summarise(
    Geo_Code = "Media UE",
    ShadowEconomy = mean(ShadowEconomy, na.rm = TRUE),
    Corruption_Level = 100 - mean(CPI_Score, na.rm = TRUE), # CPI mic = Corupție mare
    VAT_Gap = mean(VAT_Gap, na.rm = TRUE)
  )

# Combinăm datele
plot_data <- df %>%
  filter(Geo_Code %in% target_countries) %>%
  mutate(Corruption_Level = 100 - CPI_Score) %>%
  select(Geo_Code, ShadowEconomy, Corruption_Level, VAT_Gap) %>%
  bind_rows(eu_avg) %>%
  pivot_longer(cols = c("ShadowEconomy", "Corruption_Level", "VAT_Gap"),
               names_to = "Indicator", values_to = "Valoare")

# Redenumim indicatorii
plot_data$Indicator <- dplyr::recode(plot_data$Indicator,
                                     "ShadowEconomy" = "1. Economie Gri (%)",
                                     "Corruption_Level" = "2. Nivel Corupție (Est.)",
                                     "VAT_Gap" = "3. VAT Gap (Neîncasare)")

# Desenăm Graficul
g1 <- ggplot(plot_data, aes(x = Indicator, y = Valoare, fill = Geo_Code)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("BG" = "#009688", "RO" = "#d73027", "Media UE" = "#999999")) +
  geom_text(aes(label = round(Valoare, 1)), 
            position = position_dodge(width = 0.8), vjust = -0.5, fontface = "bold", size = 3.5) +
  theme_minimal() +
  labs(title = "Paradoxul Balcanic: România vs Bulgaria",
       subtitle = "Bulgaria are probleme structurale similare, dar colectează TVA la nivel european.",
       y = "Procent (%)", x = NULL, fill = "Tara / Zona") +
  theme(legend.position = "top", axis.text.x = element_text(face="bold"))

print(g1)


# 6. GRAFICUL 2: IMPACTUL DIGITALIZĂRII (CELE DOUĂ VITEZE)
# ------------------------------------------------------------------------------
# Arată cele două linii de regresie paralele

g2 <- ggplot(df, aes(x = ShadowEconomy, y = VAT_Gap, color = Regim_Fiscal)) +
  # Punctele
  geom_point(size = 3, alpha = 0.7) +
  
  # Liniile de regresie paralele
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  
  # Etichete țări
  geom_text_repel(aes(label = Geo_Code), show.legend = FALSE, box.padding = 0.3) +
  
  # Culori
  scale_color_manual(values = c("Digitalizat / Strict (ex: BG, HU, IT)" = "#2c7bb6", 
                                "Traditional / Lent (ex: RO, DE)" = "#d7191c")) +
  
  theme_minimal() +
  labs(title = "Digitalizarea reduce Gap-ul de TVA",
       subtitle = paste("Țările digitalizate (Albastru) au un Gap cu aprox.", 
                        round(abs(s_final$coefficients[3,1]), 1), "pp mai mic, la aceeași economie gri."),
       x = "Economie Gri (% din PIB)",
       y = "VAT Gap (%)",
       color = "Tip Administrare Fiscală") +
  theme(legend.position = "bottom")

print(g2)