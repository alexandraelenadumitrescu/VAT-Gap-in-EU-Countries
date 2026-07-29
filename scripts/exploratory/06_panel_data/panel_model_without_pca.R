# --- STEP 0: Libraries & Data Loading ---
# Install packages if you haven't: install.packages(c("readr", "dplyr", "ggplot2", "corrplot", "psych"))
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(psych)

# 1. Load the data 
# Make sure the file "Project Econometrie 2025-2026.csv" is in your specific working directory
data_full <- read_csv("../../../data/raw/panel_data_variables.csv")

# --- STEP 1: Filter for Cross-Sectional Analysis (Year 2022) ---
# Requirement: Analyze a phenomenon at a specific moment (Application 1)
data_2022 <- data_full %>% 
  filter(Year == 2022)

# Check the structure
str(data_2022)

# --- STEP 2: Descriptive Statistics ---
# Requirement 2a/2b: Describe variables and statistics
describe(data_2022 %>% select(where(is.numeric)))

# --- STEP 3: Distribution of the Dependent Variable (Value) ---
# We need to see if "Value" (VAT Gap) is roughly normal
ggplot(data_2022, aes(x = Value)) +
  geom_histogram(binwidth = 0.02, fill = "steelblue", color = "white") +
  labs(title = "Distribution of VAT Gap (Value) in 2022",
       x = "VAT Gap (Value)",
       y = "Frequency") +
  theme_minimal()

# --- STEP 4: Correlation Matrix ---
# Requirement 2b: Analyze correlations between variables
# We exclude "Country" (text) and "Year" (constant)
cor_matrix <- cor(data_2022 %>% select(where(is.numeric), -Year))

# Plot the heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", # Add coefficient numbers
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         title = "Correlation Matrix (2022)", mar = c(0,0,1,0))
