################################################################################
# PROIECT ECONOMETRIE - ANALIZĂ STATISTICĂ DESCRIPTIVĂ ȘI GRAFICĂ
# Dataset: EU27 - Shadow Economy, VAT Compliance Gap, VAT Revenue
# Autor: Bianca
# Data: Decembrie 2024
################################################################################

# Curățare environment
rm(list = ls())

setwd("/Users/biancabaias/an3/sem1/econometrie/proiect/FINAL") 

# Forțează instalarea pachetelor binare pe macOS
options(pkgType = "both")

# Încărcare și instalare pachete necesare
# Pachete ESENȚIALE - instalare rapidă
PackageNames <- c("ggplot2", "moments", "car", "corrplot", "psych")

for(i in PackageNames){
  if(!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    require(i, character.only = TRUE)
  }
}

cat("\n")
cat("================================================================================\n")
cat("          ANALIZA STATISTICĂ DESCRIPTIVĂ - DATASET EU27                        \n")
cat("================================================================================\n\n")

################################################################################
# 1. ÎNCĂRCARE ȘI EXPLORARE INIȚIALĂ A DATELOR
################################################################################

# Citire dataset
dataset <- read.csv("/Users/biancabaias/an3/sem1/econometrie/proiect/FINAL/EU27_Dataset_2023_Final_Complete.csv")

cat(">>> 1. STRUCTURA DATASET-ULUI <<<\n")
cat("================================================================================\n")
str(dataset)
cat("\n")

cat(">>> Primele 10 observații <<<\n")
print(head(dataset, 10))
cat("\n")

cat(">>> Ultimele 10 observații <<<\n")
print(tail(dataset, 10))
cat("\n")

cat(">>> Dimensiuni dataset <<<\n")
cat("Număr de observații:", nrow(dataset), "\n")
cat("Număr de variabile:", ncol(dataset), "\n")
cat("Număr total de valori:", nrow(dataset) * ncol(dataset), "\n\n")

# Verificare valori lipsă
cat(">>> Valori lipsă (NA) <<<\n")
missing_values <- colSums(is.na(dataset))
print(missing_values)
cat("Total valori lipsă:", sum(is.na(dataset)), "\n\n")

################################################################################
# 2. STATISTICI DESCRIPTIVE DETALIATE
################################################################################

cat("================================================================================\n")
cat(">>> 2. STATISTICI DESCRIPTIVE <<<\n")
cat("================================================================================\n\n")

# Variabile numerice
numeric_vars <- c("ShadowEconomy", "VAT_Compliance_Gap", "VAT_Revenue_Perc_GDP")

cat(">>> 2.1. SUMAR STATISTIC COMPLET <<<\n")
cat("--------------------------------------------------------------------------------\n")
summary_stats <- summary(dataset[, numeric_vars])
print(summary_stats)
cat("\n")

# Statistici descriptive detaliate pentru fiecare variabilă
cat(">>> 2.2. STATISTICI DESCRIPTIVE DETALIATE <<<\n")
cat("--------------------------------------------------------------------------------\n")

for(var in numeric_vars){
  cat("\n", toupper(var), "\n")
  cat(rep("-", 80), "\n", sep="")
  cat("Mean (Media):                  ", round(mean(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Median (Mediana):              ", round(median(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Standard Deviation (Abaterea): ", round(sd(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Variance (Varianța):           ", round(var(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Minimum:                       ", round(min(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Maximum:                       ", round(max(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Range (Amplitudine):           ", round(max(dataset[[var]], na.rm=TRUE) - 
                                                 min(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Q1 (Quartila 1):               ", round(quantile(dataset[[var]], 0.25, na.rm=TRUE), 4), "\n")
  cat("Q3 (Quartila 3):               ", round(quantile(dataset[[var]], 0.75, na.rm=TRUE), 4), "\n")
  cat("IQR (Interval intercuartilic): ", round(IQR(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Skewness (Asimetrie):          ", round(skewness(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Kurtosis (Boltire):            ", round(kurtosis(dataset[[var]], na.rm=TRUE), 4), "\n")
  cat("Coef. Variație (CV%):          ", round(sd(dataset[[var]], na.rm=TRUE) / 
                                                 mean(dataset[[var]], na.rm=TRUE) * 100, 2), "%\n")
}
cat("\n")

# Statistici descriptive cu psych::describe
cat(">>> 2.3. STATISTICI PSYCH::DESCRIBE <<<\n")
cat("--------------------------------------------------------------------------------\n")
describe_stats <- describe(dataset[, numeric_vars])
print(describe_stats)
cat("\n")

################################################################################
# 3. TESTE DE NORMALITATE
################################################################################

cat("================================================================================\n")
cat(">>> 3. TESTE DE NORMALITATE <<<\n")
cat("================================================================================\n\n")

for(var in numeric_vars){
  cat(">>> ", toupper(var), " <<<\n")
  cat("--------------------------------------------------------------------------------\n")
  
  # Jarque-Bera Test
  jb_test <- jarque.bera.test(dataset[[var]])
  cat("Jarque-Bera Test:\n")
  cat("  Statistica:  ", round(jb_test$statistic, 4), "\n")
  cat("  P-value:     ", format(jb_test$p.value, scientific=TRUE), "\n")
  cat("  Concluzie:   ", ifelse(jb_test$p.value > 0.05, 
                                "Distribuție NORMALĂ (p > 0.05)", 
                                "Distribuție NON-NORMALĂ (p < 0.05)"), "\n\n")
  
  # Shapiro-Wilk Test
  sw_test <- shapiro.test(dataset[[var]])
  cat("Shapiro-Wilk Test:\n")
  cat("  Statistica:  ", round(sw_test$statistic, 4), "\n")
  cat("  P-value:     ", format(sw_test$p.value, scientific=TRUE), "\n")
  cat("  Concluzie:   ", ifelse(sw_test$p.value > 0.05, 
                                "Distribuție NORMALĂ (p > 0.05)", 
                                "Distribuție NON-NORMALĂ (p < 0.05)"), "\n\n")
}

################################################################################
# 4. MATRICE DE CORELAȚIE
################################################################################

cat("================================================================================\n")
cat(">>> 4. ANALIZA CORELAȚIILOR <<<\n")
cat("================================================================================\n\n")

# Matrice de corelație Pearson
cat(">>> 4.1. MATRICE DE CORELAȚIE PEARSON <<<\n")
cat("--------------------------------------------------------------------------------\n")
cor_matrix <- cor(dataset[, numeric_vars], use="complete.obs")
print(round(cor_matrix, 4))
cat("\n")

# Testare semnificație corelații
cat(">>> 4.2. TESTE DE SEMNIFICAȚIE CORELAȚII <<<\n")
cat("--------------------------------------------------------------------------------\n")

# ShadowEconomy vs VAT_Compliance_Gap
cor_test1 <- cor.test(dataset$ShadowEconomy, dataset$VAT_Compliance_Gap)
cat("ShadowEconomy vs VAT_Compliance_Gap:\n")
cat("  Corelație:   ", round(cor_test1$estimate, 4), "\n")
cat("  P-value:     ", format(cor_test1$p.value, scientific=TRUE), "\n")
cat("  Interval:    [", round(cor_test1$conf.int[1], 4), ", ", 
    round(cor_test1$conf.int[2], 4), "]\n")
cat("  Semnificativ:", ifelse(cor_test1$p.value < 0.05, "DA (p < 0.05)", "NU (p > 0.05)"), "\n\n")

# ShadowEconomy vs VAT_Revenue_Perc_GDP
cor_test2 <- cor.test(dataset$ShadowEconomy, dataset$VAT_Revenue_Perc_GDP)
cat("ShadowEconomy vs VAT_Revenue_Perc_GDP:\n")
cat("  Corelație:   ", round(cor_test2$estimate, 4), "\n")
cat("  P-value:     ", format(cor_test2$p.value, scientific=TRUE), "\n")
cat("  Interval:    [", round(cor_test2$conf.int[1], 4), ", ", 
    round(cor_test2$conf.int[2], 4), "]\n")
cat("  Semnificativ:", ifelse(cor_test2$p.value < 0.05, "DA (p < 0.05)", "NU (p > 0.05)"), "\n\n")

# VAT_Compliance_Gap vs VAT_Revenue_Perc_GDP
cor_test3 <- cor.test(dataset$VAT_Compliance_Gap, dataset$VAT_Revenue_Perc_GDP)
cat("VAT_Compliance_Gap vs VAT_Revenue_Perc_GDP:\n")
cat("  Corelație:   ", round(cor_test3$estimate, 4), "\n")
cat("  P-value:     ", format(cor_test3$p.value, scientific=TRUE), "\n")
cat("  Interval:    [", round(cor_test3$conf.int[1], 4), ", ", 
    round(cor_test3$conf.int[2], 4), "]\n")
cat("  Semnificativ:", ifelse(cor_test3$p.value < 0.05, "DA (p < 0.05)", "NU (p > 0.05)"), "\n\n")

# Corelație Spearman (non-parametric)
cat(">>> 4.3. MATRICE DE CORELAȚIE SPEARMAN (non-parametric) <<<\n")
cat("--------------------------------------------------------------------------------\n")
cor_spearman <- cor(dataset[, numeric_vars], method="spearman", use="complete.obs")
print(round(cor_spearman, 4))
cat("\n")

################################################################################
# 5. ÎMPĂRȚIRE DATASET ÎN TRAIN ȘI TEST
################################################################################

cat("================================================================================\n")
cat(">>> 5. ÎMPĂRȚIRE DATASET: TRAIN (75%) și TEST (25%) <<<\n")
cat("================================================================================\n\n")

# Setare seed pentru reproducibilitate
set.seed(123)

# Creare index pentru train (75%) - folosim sampling direct din R base
n_total <- nrow(dataset)
n_train <- floor(0.75 * n_total)

# Selectare random index pentru train
train_index <- sample(1:n_total, n_train, replace = FALSE)

# Împărțire date
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

cat(">>> Dimensiuni seturi de date <<<\n")
cat("--------------------------------------------------------------------------------\n")
cat("Dataset complet:     ", nrow(dataset), "observații\n")
cat("Train set (75%):     ", nrow(train_data), "observații\n")
cat("Test set (25%):      ", nrow(test_data), "observații\n")
cat("Verificare:          ", nrow(train_data) + nrow(test_data), "= ", nrow(dataset), "\n\n")

# Statistici descriptive pentru Train Set
cat(">>> STATISTICI DESCRIPTIVE - TRAIN SET <<<\n")
cat("--------------------------------------------------------------------------------\n")
print(summary(train_data[, numeric_vars]))
cat("\n")

# Statistici descriptive pentru Test Set
cat(">>> STATISTICI DESCRIPTIVE - TEST SET <<<\n")
cat("--------------------------------------------------------------------------------\n")
print(summary(test_data[, numeric_vars]))
cat("\n")

# Comparație distribuții Train vs Test
cat(">>> COMPARAȚIE MEDII: TRAIN vs TEST <<<\n")
cat("--------------------------------------------------------------------------------\n")
for(var in numeric_vars){
  cat(var, ":\n")
  cat("  Mean Train:  ", round(mean(train_data[[var]], na.rm=TRUE), 4), "\n")
  cat("  Mean Test:   ", round(mean(test_data[[var]], na.rm=TRUE), 4), "\n")
  cat("  Diferență:   ", round(abs(mean(train_data[[var]], na.rm=TRUE) - 
                                     mean(test_data[[var]], na.rm=TRUE)), 4), "\n\n")
}

# Test t pentru a verifica dacă distribuțiile sunt similare
cat(">>> TESTE T: Comparare Train vs Test <<<\n")
cat("--------------------------------------------------------------------------------\n")
for(var in numeric_vars){
  t_test_result <- t.test(train_data[[var]], test_data[[var]])
  cat(var, ":\n")
  cat("  P-value:     ", format(t_test_result$p.value, scientific=TRUE), "\n")
  cat("  Concluzie:   ", ifelse(t_test_result$p.value > 0.05, 
                                "Distribuții SIMILARE (p > 0.05)", 
                                "Distribuții DIFERITE (p < 0.05)"), "\n\n")
}


################################################################################
# 5. ÎMPĂRȚIRE ȘI VALIDARE: 5-FOLD CROSS-VALIDATION
################################################################################

cat("================================================================================\n")
cat(">>> 5. VALIDARE MODEL: 5-FOLD CROSS-VALIDATION <<<\n")
cat("================================================================================\n\n")

# Setare seed pentru reproducibilitate
set.seed(123)

# Parametri CV
k <- 5  # număr de fold-uri
n <- nrow(dataset)
fold_size <- floor(n / k)
indices <- sample(1:n)

# Inițializare rezultate
cv_rmse <- numeric(k)
cv_r2 <- numeric(k)
cv_mae <- numeric(k)

cat("Rulare", k, "fold-uri de cross-validation...\n\n")

# Loop prin fiecare fold
for(i in 1:k){
  # Index-uri pentru test fold
  start_idx <- (i - 1) * fold_size + 1
  end_idx <- ifelse(i == k, n, i * fold_size)
  test_idx <- indices[start_idx:end_idx]
  
  # Split train/test
  train_fold <- dataset[-test_idx, ]
  test_fold <- dataset[test_idx, ]
  
  # Antrenare model
  model_fold <- lm(VAT_Compliance_Gap ~ ShadowEconomy + VAT_Revenue_Perc_GDP, 
                   data = train_fold)
  
  # Predicții
  predictions <- predict(model_fold, newdata = test_fold)
  actual <- test_fold$VAT_Compliance_Gap
  
  # Metrici
  cv_rmse[i] <- sqrt(mean((actual - predictions)^2))
  cv_r2[i] <- 1 - (sum((actual - predictions)^2) / sum((actual - mean(actual))^2))
  cv_mae[i] <- mean(abs(actual - predictions))
  
  # Afișare
  cat("Fold", i, ":\n")
  cat("  Train:", nrow(train_fold), "| Test:", nrow(test_fold), "\n")
  cat("  RMSE:", round(cv_rmse[i], 4), "| R²:", round(cv_r2[i], 4), "\n\n")
}

# Rezultate finale
cat("================================================================================\n")
cat(">>> REZULTATE CROSS-VALIDATION <<<\n")
cat("================================================================================\n\n")

cat("RMSE:  ", round(mean(cv_rmse), 4), "±", round(sd(cv_rmse), 4), "\n")
cat("R²:    ", round(mean(cv_r2), 4), "±", round(sd(cv_r2), 4), "\n")
cat("MAE:   ", round(mean(cv_mae), 4), "±", round(sd(cv_mae), 4), "\n\n")



##Strategie Hibridă (CV + Holdout)

# Dezinstalați pachetul
remove.packages("caret")

# Reinstalați (inclusiv dependențele)
install.packages("caret", dependencies = TRUE)

# Încercați să încărcați din nou
library(caret)

set.seed(123)

# PASUL 1: Separă 20% pentru final test (5 țări)
holdout_index <- sample(1:nrow(dataset), size = 5)
final_test <- dataset[holdout_index, ]
cv_data <- dataset[-holdout_index, ]  # 22 țări pentru CV

cat("Final Test Set:", nrow(final_test), "țări\n")
cat("CV Data:", nrow(cv_data), "țări\n")

# PASUL 2: 5-Fold CV pe cele 22 țări
train_control <- trainControl(method = "cv", number = 5)

model_cv <- train(
  VAT_Compliance_Gap ~ ShadowEconomy + VAT_Revenue_Perc_GDP,
  data = cv_data,
  method = "lm",
  trControl = train_control
)

cat("\nCV Results (22 țări):\n")
print(model_cv)

# PASUL 3: Final test pe cele 5 țări neatinse
final_model <- lm(VAT_Compliance_Gap ~ ShadowEconomy + VAT_Revenue_Perc_GDP, 
                  data = cv_data)

final_predictions <- predict(final_model, newdata = final_test)
final_rmse <- sqrt(mean((final_test$VAT_Compliance_Gap - final_predictions)^2))

cat("\nFinal Test Set Results (5 țări):\n")
cat("RMSE:", final_rmse, "\n")
cat("R-squared:", cor(final_test$VAT_Compliance_Gap, final_predictions)^2, "\n")


################################################################################
# 6. ANALIZE GRAFICE
################################################################################

cat("================================================================================\n")
cat(">>> 6. GENERARE GRAFICE (salvate în working directory) <<<\n")
cat("================================================================================\n\n")

# 6.1. Histograme pentru fiecare variabilă
cat("Generare: Histograme pentru distribuții...\n")
png("histograme_distributii.png", width=1200, height=800, res=120)
par(mfrow=c(2,2))

for(var in numeric_vars){
  hist(dataset[[var]], 
       main=paste("Histogramă -", var),
       xlab=var,
       ylab="Frecvență",
       col="lightblue",
       border="darkblue",
       breaks=15)
  abline(v=mean(dataset[[var]], na.rm=TRUE), col="red", lwd=2, lty=2)
  abline(v=median(dataset[[var]], na.rm=TRUE), col="green", lwd=2, lty=2)
  legend("topright", legend=c("Mean", "Median"), 
         col=c("red", "green"), lty=2, lwd=2, cex=0.8)
}
dev.off()
cat("  ✓ Salvat: histograme_distributii.png\n\n")

# 6.2. Boxplots pentru fiecare variabilă
cat("Generare: Boxplots pentru identificare outlieri...\n")
png("boxplots_variabile.png", width=1200, height=600, res=120)
par(mfrow=c(1,3))

for(var in numeric_vars){
  boxplot(dataset[[var]], 
          main=paste("Boxplot -", var),
          ylab=var,
          col="lightgreen",
          border="darkgreen",
          outline=TRUE)
  points(1, mean(dataset[[var]], na.rm=TRUE), pch=19, col="red", cex=1.5)
  legend("topright", legend="Mean", pch=19, col="red", cex=0.8)
}
dev.off()
cat("  ✓ Salvat: boxplots_variabile.png\n\n")

# 6.3. Q-Q Plots pentru normalitate
cat("Generare: Q-Q Plots pentru teste de normalitate...\n")
png("qq_plots_normalitate.png", width=1200, height=800, res=120)
par(mfrow=c(2,2))

for(var in numeric_vars){
  qqnorm(dataset[[var]], 
         main=paste("Q-Q Plot -", var),
         col="blue",
         pch=19)
  qqline(dataset[[var]], col="red", lwd=2)
}
dev.off()
cat("  ✓ Salvat: qq_plots_normalitate.png\n\n")

# 6.4. Scatter plots - relații între variabile
cat("Generare: Scatter plots pentru corelații...\n")
png("scatter_plots_corelatii.png", width=1200, height=1200, res=120)
par(mfrow=c(2,2))

# Shadow Economy vs VAT Compliance Gap
plot(dataset$ShadowEconomy, dataset$VAT_Compliance_Gap,
     main="Shadow Economy vs VAT Compliance Gap",
     xlab="Shadow Economy (%)",
     ylab="VAT Compliance Gap (%)",
     pch=19, col="darkblue")
abline(lm(VAT_Compliance_Gap ~ ShadowEconomy, data=dataset), col="red", lwd=2)
text(min(dataset$ShadowEconomy), max(dataset$VAT_Compliance_Gap), 
     paste("r =", round(cor(dataset$ShadowEconomy, dataset$VAT_Compliance_Gap), 3)),
     pos=4, col="red")

# Shadow Economy vs VAT Revenue
plot(dataset$ShadowEconomy, dataset$VAT_Revenue_Perc_GDP,
     main="Shadow Economy vs VAT Revenue (% GDP)",
     xlab="Shadow Economy (%)",
     ylab="VAT Revenue (% GDP)",
     pch=19, col="darkgreen")
abline(lm(VAT_Revenue_Perc_GDP ~ ShadowEconomy, data=dataset), col="red", lwd=2)
text(min(dataset$ShadowEconomy), max(dataset$VAT_Revenue_Perc_GDP), 
     paste("r =", round(cor(dataset$ShadowEconomy, dataset$VAT_Revenue_Perc_GDP), 3)),
     pos=4, col="red")

# VAT Compliance Gap vs VAT Revenue
plot(dataset$VAT_Compliance_Gap, dataset$VAT_Revenue_Perc_GDP,
     main="VAT Compliance Gap vs VAT Revenue (% GDP)",
     xlab="VAT Compliance Gap (%)",
     ylab="VAT Revenue (% GDP)",
     pch=19, col="darkorange")
abline(lm(VAT_Revenue_Perc_GDP ~ VAT_Compliance_Gap, data=dataset), col="red", lwd=2)
text(min(dataset$VAT_Compliance_Gap), max(dataset$VAT_Revenue_Perc_GDP), 
     paste("r =", round(cor(dataset$VAT_Compliance_Gap, dataset$VAT_Revenue_Perc_GDP), 3)),
     pos=4, col="red")

dev.off()
cat("  ✓ Salvat: scatter_plots_corelatii.png\n\n")

# 6.5. Matrice de corelație vizuală
cat("Generare: Matrice de corelație vizuală...\n")
png("matrice_corelatie_vizuala.png", width=1000, height=1000, res=120)
corrplot(cor_matrix, 
         method="circle", 
         type="upper",
         addCoef.col="black",
         tl.col="black",
         tl.srt=45,
         diag=FALSE,
         title="Matrice de Corelație Pearson",
         mar=c(0,0,2,0))
dev.off()
cat("  ✓ Salvat: matrice_corelatie_vizuala.png\n\n")

# 6.6. Pairplot complet
cat("Generare: Pairplot complet...\n")
png("pairplot_complet.png", width=1400, height=1400, res=120)
pairs(dataset[, numeric_vars],
      main="Pairplot - Toate variabilele",
      pch=19,
      col="darkblue",
      lower.panel=NULL)
dev.off()
cat("  ✓ Salvat: pairplot_complet.png\n\n")

# 6.7. Density plots
cat("Generare: Density plots pentru distribuții...\n")
png("density_plots.png", width=1200, height=800, res=120)
par(mfrow=c(2,2))

for(var in numeric_vars){
  d <- density(dataset[[var]], na.rm=TRUE)
  plot(d, 
       main=paste("Density Plot -", var),
       xlab=var,
       ylab="Densitate",
       col="darkblue",
       lwd=2)
  polygon(d, col=rgb(0,0,1,0.3), border="darkblue")
  abline(v=mean(dataset[[var]], na.rm=TRUE), col="red", lwd=2, lty=2)
  abline(v=median(dataset[[var]], na.rm=TRUE), col="green", lwd=2, lty=2)
  legend("topright", legend=c("Mean", "Median"), 
         col=c("red", "green"), lty=2, lwd=2, cex=0.8)
}
dev.off()
cat("  ✓ Salvat: density_plots.png\n\n")

# 6.8. Comparație Train vs Test
cat("Generare: Comparație distribuții Train vs Test...\n")
png("comparatie_train_test.png", width=1200, height=1200, res=120)
par(mfrow=c(3,2))

for(var in numeric_vars){
  # Histograme
  hist(train_data[[var]], 
       main=paste(var, "- Train Set"),
       xlab=var,
       col=rgb(0,0,1,0.5),
       border="darkblue",
       breaks=10)
  
  hist(test_data[[var]], 
       main=paste(var, "- Test Set"),
       xlab=var,
       col=rgb(1,0,0,0.5),
       border="darkred",
       breaks=10)
}
dev.off()
cat("  ✓ Salvat: comparatie_train_test.png\n\n")

################################################################################
# 7. SUMAR FINAL
################################################################################

cat("================================================================================\n")
cat(">>> 7. SUMAR FINAL <<<\n")
cat("================================================================================\n\n")

cat("✓ Analiză completă efectuată cu succes!\n\n")

cat("FIȘIERE GENERATE:\n")
cat("  1. histograme_distributii.png        - Histograme pentru toate variabilele\n")
cat("  2. boxplots_variabile.png            - Boxplots pentru identificare outlieri\n")
cat("  3. qq_plots_normalitate.png          - Q-Q plots pentru teste normalitate\n")
cat("  4. scatter_plots_corelatii.png       - Scatter plots cu linii de regresie\n")
cat("  5. matrice_corelatie_vizuala.png     - Matrice de corelație vizuală\n")
cat("  6. pairplot_complet.png              - Pairplot complet toate variabilele\n")
cat("  7. density_plots.png                 - Density plots pentru distribuții\n")
cat("  8. comparatie_train_test.png         - Comparație Train vs Test\n\n")

cat("REZUMAT STATISTICI PRINCIPALE:\n")
cat("--------------------------------------------------------------------------------\n")
cat("Total observații:                ", nrow(dataset), "\n")
cat("Țări analizate:                  ", length(unique(dataset$Country)), "\n")
cat("Variabile numerice:              ", length(numeric_vars), "\n")
cat("Train set (75%):                 ", nrow(train_data), "observații\n")
cat("Test set (25%):                  ", nrow(test_data), "observații\n\n")

cat("CORELAȚII PRINCIPALE:\n")
cat("--------------------------------------------------------------------------------\n")
cat("Shadow Economy vs VAT Gap:       ", round(cor(dataset$ShadowEconomy, 
                                                   dataset$VAT_Compliance_Gap), 4), "\n")
cat("Shadow Economy vs VAT Revenue:   ", round(cor(dataset$ShadowEconomy, 
                                                   dataset$VAT_Revenue_Perc_GDP), 4), "\n")
cat("VAT Gap vs VAT Revenue:          ", round(cor(dataset$VAT_Compliance_Gap, 
                                                   dataset$VAT_Revenue_Perc_GDP), 4), "\n\n")

cat("================================================================================\n")
cat("                    ANALIZA S-A FINALIZAT CU SUCCES!                           \n")
cat("================================================================================\n")