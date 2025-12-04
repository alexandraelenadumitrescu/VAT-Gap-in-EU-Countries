# Instalare pachete (ruleaza doar o data daca nu le ai)
install.packages(c("tidyverse", "corrplot", "car", "glmnet", "caret", "lmtest", "tseries", "cluster", "factoextra"))

# Incarcare biblioteci
library(tidyverse)  # Pentru manipulare date si grafice
library(corrplot)   # Pentru matricea de corelatie
library(car)        # Pentru VIF (multicoliniaritate)
library(glmnet)     # Pentru Lasso si Ridge
library(caret)      # Pentru Machine Learning (Train/Test split)
library(lmtest)     # Pentru testare heteroscedasticitate
library(tseries)    # Pentru testare normalitate
library(cluster)    # Pentru K-Means
library(factoextra) # Vizualizare cluster


# 1. Incarcarea datelor (presupunem ca fisierul se numeste 'dataset.csv')
# Daca le copiezi din text, asigura-te ca separatorul este virgula
df_full <- read.csv("panel.csv", sep=",")
library(tidyverse)

# 2. Crearea sectiunii transversale (Doar anul 2022)
df_2022 <- df_full %>% 
  filter(Year == 2022) %>%
  select(-Year) # Eliminam coloana Year, nu mai e relevanta

# 3. Transformari preliminare (Logaritmare PIB)
# Articolul sugereaza ca PIB are o relatie neliniara cu evaziunea
df_2022$log_GDP <- log(df_2022$GDP_per_capita)

# Verificam primele randuri
head(df_2022)





# 1. Matricea de corelatie
# Selectam doar variabilele numerice pentru corelatie
num_vars <- df_2022 %>% select_if(is.numeric)
cor_matrix <- cor(num_vars)

# Vizualizare
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Matricea de Corelație (2022)", mar=c(0,0,1,0))

# 2. Scatter Plot: Shadow Economy vs VAT Gap (Ipoteza H1 din articol)
ggplot(df_2022, aes(x = ShadowEconomy, y = Value)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_text(aes(label = Country), vjust = -0.5, size = 3) +
  labs(title = "Relatia Economie Subterana vs Decalaj TVA (2022)",
       x = "Shadow Economy (% din PIB)",
       y = "VAT Gap (Value)") +
  theme_minimal()



# 1. Selectam variabilele pentru clustering (ex: VAT Gap si Shadow Economy)
data_cluster <- df_2022 %>% 
  select(Value, ShadowEconomy) 

# Scalam datele (obligatoriu la ML)
data_scaled <- scale(data_cluster)

# 2. Aplicam K-Means (sa zicem 3 clustere: Performeri, Medii, Problematici)
set.seed(123)
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# 3. Vizualizare Cluster
fviz_cluster(km_res, data = data_cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "Gruparea Țărilor UE (Clustering K-Means)") +
  geom_text(aes(label = df_2022$Country), vjust = -1, size = 3)


# Construim modelul OLS
model_ols <- lm(Value ~ ShadowEconomy + log_GDP + CPI_Score + FinalConsumption, data = df_2022)

# Rezumatul modelului
summary(model_ols)




# 1. Test Multicoliniaritate (VIF)
# Daca VIF > 5 sau 10, ai probleme (variabilele se repeta ca informatie)
vif(model_ols)

# 2. Test Heteroscedasticitate (Breusch-Pagan)
# H0: Erorile sunt constante (Homoscedasticitate) -> Vrem p-value > 0.05
bptest(model_ols)

# 3. Test Normalitate Reziduuri (Jarque-Bera sau Shapiro-Wilk)
# H0: Reziduurile sunt normale -> Vrem p-value > 0.05
shapiro.test(residuals(model_ols))








# Modelul 2: Eliminam log_GDP pentru a reduce multicoliniaritatea
model_ols_2 <- lm(Value ~ ShadowEconomy + CPI_Score + FinalConsumption, data = df_2022)

# Rezumat
summary(model_ols_2)

# Verificam din nou VIF
vif(model_ols_2)

# Verificam din nou Normalitatea
shapiro.test(residuals(model_ols_2))









# 1. Train / Test Split (75% antrenare, 25% testare)
set.seed(123)
index <- createDataPartition(df_2022$Value, p = 0.75, list = FALSE)
train_data <- df_2022[index, ]
test_data  <- df_2022[-index, ]

# 2. Pregatirea matricelor pentru GLMNET
x_train <- model.matrix(Value ~ ShadowEconomy + log_GDP + CPI_Score + FinalConsumption, train_data)[,-1]
y_train <- train_data$Value
x_test <- model.matrix(Value ~ ShadowEconomy + log_GDP + CPI_Score + FinalConsumption, test_data)[,-1]
y_test <- test_data$Value

# 3. Model LASSO (Alpha = 1)
# Lasso poate reduce coeficientii la zero (selectie de variabile)
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda <- lasso_cv$lambda.min
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# 4. Predictie si Evaluare
pred_ols <- predict(model_ols, newdata = test_data)
pred_lasso <- predict(lasso_model, newx = x_test)

# Calcul RMSE (Eroarea medie)
rmse_ols <- sqrt(mean((test_data$Value - pred_ols)^2))
rmse_lasso <- sqrt(mean((test_data$Value - pred_lasso)^2))

print(paste("RMSE OLS:", round(rmse_ols, 4)))
print(paste("RMSE Lasso:", round(rmse_lasso, 4)))





library(glmnet)

# Pregatim datele (Matrice X si Vector Y)
x <- model.matrix(Value ~ ShadowEconomy + CPI_Score + FinalConsumption, df_2022)[,-1]
y <- df_2022$Value

# 1. RIDGE Regression (Alpha = 0)
# Ridge pastreaza toate variabilele dar le micsoreaza impactul pentru a reduce varianta
cv_ridge <- cv.glmnet(x, y, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min
model_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)

# Coeficientii Ridge
coef(model_ridge)

# 2. LASSO Regression (Alpha = 1)
# Lasso poate elimina variabilele inutile (le face zero)
cv_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min
model_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)

# Coeficientii Lasso
coef(model_lasso)



# 1. Facem predictii pe datele existente
# (Nota: Ideal se face pe set de test, dar la 27 obs folosim setul intreg pentru demonstratie)
pred_ols <- predict(model_ols_2, newdata = df_2022)
pred_ridge <- predict(model_ridge, newx = x)
pred_lasso <- predict(model_lasso, newx = x)

# 2. Calculam RMSE (Eroarea medie patratica)
rmse_ols <- sqrt(mean((df_2022$Value - pred_ols)^2))
rmse_ridge <- sqrt(mean((df_2022$Value - pred_ridge)^2))
rmse_lasso <- sqrt(mean((df_2022$Value - pred_lasso)^2))

# 3. Afisam rezultatele comparative
results <- data.frame(
  Model = c("OLS Clasic", "Ridge (Regularizare)", "Lasso (Selectie)"),
  RMSE = c(rmse_ols, rmse_ridge, rmse_lasso)
)

print(results)




# ---------------------------------------------------------
# 3. ELASTIC NET Regression (Alpha = 0.5)
# ---------------------------------------------------------
# Alpha 0.5 inseamna un mix egal intre Lasso si Ridge.
# (Nota: Alpha 1 = Lasso, Alpha 0 = Ridge, 0 < Alpha < 1 = Elastic Net)

# Gasim lambda optim pentru Elastic Net
cv_elastic <- cv.glmnet(x, y, alpha = 0.5)
best_lambda_elastic <- cv_elastic$lambda.min

# Antrenam modelul final
model_elastic <- glmnet(x, y, alpha = 0.5, lambda = best_lambda_elastic)

# Afisam coeficientii
print("Coeficienti Elastic Net:")
coef(model_elastic)

# ---------------------------------------------------------
# COMPARATIE FINALA (OLS vs Ridge vs Lasso vs Elastic Net)
# ---------------------------------------------------------

# Facem predictii
pred_elastic <- predict(model_elastic, newx = x)

# Calculam RMSE pentru Elastic Net
rmse_elastic <- sqrt(mean((df_2022$Value - pred_elastic)^2))

# Actualizam tabelul de rezultate
results_final <- data.frame(
  Model = c("OLS Clasic", "Ridge", "Lasso", "Elastic Net"),
  RMSE = c(rmse_ols, rmse_ridge, rmse_lasso, rmse_elastic)
)

print(results_final)