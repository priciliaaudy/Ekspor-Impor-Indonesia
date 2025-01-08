#install.packages("orcutt")
#install.packages("tseries")
library(orcutt)
library(readr)
library(lmtest)
library(car)
library(corrplot)
library(tseries)
library(tidyverse)
library(dplyr)
library(glmnet)
library(ggplot2)
library(caret)
library(MLmetrics)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Define Data
my_locale <- readr::locale(decimal_mark = ".", grouping_mark = ",")
DataEksporImpor <- readr::read_csv("C:/Users/audy8/Downloads/DATAEKSPORIMPOR_TA.csv", locale = my_locale)
DataEksporImpor
dim(DataEksporImpor)
head(DataEksporImpor)
str(data)
summary(DataEksporImpor)
Type <- sapply(DataEksporImpor, class)
data.frame(Type)
#Plot histogram
#hist for y
hist(DataEksporImpor$Impor)
hist(DataEksporImpor$EksporMigas)
hist(DataEksporImpor$EksporNonMigas)
#hist for x
hist(DataEksporImpor$USD)
hist(DataEksporImpor$JPY)
hist(DataEksporImpor$GBP)
hist(DataEksporImpor$CHF)
hist(DataEksporImpor$SGD)
hist(DataEksporImpor$MYR)
hist(DataEksporImpor$HKD)
hist(DataEksporImpor$AUD)
hist(DataEksporImpor$IHBPEkspor)
hist(DataEksporImpor$IHBPEkspor)
hist(DataEksporImpor$IHBPImpor)
hist(DataEksporImpor$GDP)
hist(DataEksporImpor$Inflasi_Umum)
hist(DataEksporImpor$`AdjClose_ JKLQ45`)
hist(DataEksporImpor$`AdjClose_ JKSE`)
hist(DataEksporImpor$BI_Rate)
hist(DataEksporImpor$Harga_Emas)
#KORELASI
library(corrplot)
library(dplyr)
#DATA VISUALISASI UNTUK MELIHAT KORELASI
cor_matrix <- DataEksporImpor %>%
  select_if(is.numeric) %>%
  cor()
# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle")

#korelasi spearman-rho
independent_variables <- DataEksporImpor[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHBPEkspor","IHBPImpor","GDP","M1","M2","Inflasi_Umum","AdjClose_ JKLQ45","AdjClose_ JKSE","BI_Rate","Harga_Emas")]
spearman_rho_results <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$EksporMigas, method = "spearman"))
print(spearman_rho_results)

#korelasi kendall's-tau
independent_variables <- DataEksporImpor[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHBPEkspor","IHBPImpor","GDP","M1","M2","Inflasi_Umum","AdjClose_ JKLQ45","AdjClose_ JKSE","BI_Rate","Harga_Emas")]
kendall_tau_results <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$EksporMigas, method = "kendall"))
print(kendall_tau_results)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#MULTIPLE LINEAR REGRESSION
independent_variables <- DataEksporImpor[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHBPEkspor","IHBPImpor","GDP","M1","M2","Inflasi_Umum","AdjClose_ JKLQ45","AdjClose_ JKSE","BI_Rate","Harga_Emas")]
model1 <- lm(DataEksporImpor$Impor ~ ., data = independent_variables)
model2 <- lm(DataEksporImpor$EksporMigas ~ ., data = independent_variables)
model3<- lm(DataEksporImpor$EksporNonMigas ~ ., data = independent_variables)
model4<- lm(DataEksporImpor$Ekspor ~ ., data = independent_variables)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#UJI ASUMSI KLASIK EKSPOR MIGAS
#UJI NORMALITAS
# Q-Q Plot
qqnorm(residuals(model2))
qqline(residuals(model2))
# Uji 1 Sample KS
#H0: Data berasal dari distribusi normal.
#H1: Data tidak berasal dari distribusi normal.
# Mengekstrak residual
residuals <- residuals(model2)
# Melakukan uji Kolmogorov-Smirnov pada residual
ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
ks_test #0.4735
#karena pvalue>0.05, maka belum ada bukti cukup untuk menolak h0 yang menyatakan data berasal dri dist normal

#UJI HOMOSKEDASITAS
#H0 : Variansi model konstan (homoscedacity)
#HA : Variansi model tidak konstan (heteroscedacity)
# Uji Breusch-Pagan
bptest(model2) #0.4546
#Karena P-val>0.05, maka H0 belum bisa ditolak sehingga Variansi  konstan

#UJI AUTOKORELASI RESIDUAL
#H0: Tidak ada autokorelasi dalam residual dari model regresi.
#H1: Ada autokorelasi dalam residual dari model regresi.
# Uji Durbin-Watson
dwtest(model2) 
#karena pval lebih kecil dari 0.05, maka H0 bisa ditolak sehingga  ada korelasi antara residual
# Terapkan metode Cochrane-Orcutt agar tak ada autokorelasi
model2_co <- cochrane.orcutt(model2)
summary(model2_co)
dwtest(model2_co) #0.4834
#karena pval lebih besar dari 0.05, maka H0 belum bisa ditolak sehingga tidak ada korelasi antara residual
rhomodel2 <- model2_co$rho
# Calculate lagged values for each predictor and the response
lagged_EksporMigas <- c(NA, head(DataEksporImpor$EksporMigas, -1))
lagged_USD <- c(NA, head(DataEksporImpor$USD, -1))
lagged_JPY <- c(NA, head(DataEksporImpor$JPY, -1))
lagged_GBP <- c(NA, head(DataEksporImpor$GBP, -1))
lagged_CHF <- c(NA, head(DataEksporImpor$CHF, -1))
lagged_SGD <- c(NA, head(DataEksporImpor$SGD, -1))
lagged_MYR <- c(NA, head(DataEksporImpor$MYR, -1))
lagged_HKD <- c(NA, head(DataEksporImpor$HKD, -1))
lagged_AUD <- c(NA, head(DataEksporImpor$AUD, -1))
lagged_CAD <- c(NA, head(DataEksporImpor$CAD, -1))
lagged_IHBPEkspor <- c(NA, head(DataEksporImpor$IHBPEkspor, -1))
lagged_IHBPImpor <- c(NA, head(DataEksporImpor$IHBPImpor, -1))
lagged_GDP <- c(NA, head(DataEksporImpor$GDP, -1))
lagged_M1 <- c(NA, head(DataEksporImpor$M1, -1))
lagged_M2 <- c(NA, head(DataEksporImpor$M2, -1))
lagged_Inflasi_Umum <- c(NA, head(DataEksporImpor$Inflasi_Umum, -1))
lagged_AdjClose_JKLQ45 <- c(NA, head(DataEksporImpor$`AdjClose_ JKLQ45`, -1))
lagged_AdjClose_JKSE <- c(NA, head(DataEksporImpor$`AdjClose_ JKSE`, -1))
lagged_BI_Rate <- c(NA, head(DataEksporImpor$BI_Rate, -1))
lagged_Harga_Emas <- c(NA, head(DataEksporImpor$Harga_Emas, -1))

# Remove the first row with NA values due to lag

DataEksporMigas_lagged <- DataEksporImpor[-1, ]
# Apply the Cochrane-Orcutt transformation to the response variable
DataEksporMigas_lagged$EksporMigas_transformed <- DataEksporMigas_lagged$EksporMigas - rhomodel2 * lagged_EksporMigas[-1]

# Apply the Cochrane-Orcutt transformation to each predictor variable
DataEksporMigas_lagged$USD_transformed <- DataEksporMigas_lagged$USD - rhomodel2 * lagged_USD[-1]
DataEksporMigas_lagged$JPY_transformed <- DataEksporMigas_lagged$JPY - rhomodel2 * lagged_JPY[-1]
DataEksporMigas_lagged$GBP_transformed <- DataEksporMigas_lagged$GBP - rhomodel2 * lagged_GBP[-1]
DataEksporMigas_lagged$CHF_transformed <- DataEksporMigas_lagged$CHF - rhomodel2 * lagged_CHF[-1]
DataEksporMigas_lagged$SGD_transformed <- DataEksporMigas_lagged$SGD - rhomodel2 * lagged_SGD[-1]
DataEksporMigas_lagged$MYR_transformed <- DataEksporMigas_lagged$MYR - rhomodel2 * lagged_MYR[-1]
DataEksporMigas_lagged$HKD_transformed <- DataEksporMigas_lagged$HKD - rhomodel2 * lagged_HKD[-1]
DataEksporMigas_lagged$AUD_transformed <- DataEksporMigas_lagged$AUD - rhomodel2 * lagged_AUD[-1]
DataEksporMigas_lagged$CAD_transformed <- DataEksporMigas_lagged$CAD - rhomodel2 * lagged_CAD[-1]
DataEksporMigas_lagged$IHBPEkspor_transformed <- DataEksporMigas_lagged$IHBPEkspor - rhomodel2 * lagged_IHBPEkspor[-1]
DataEksporMigas_lagged$IHBPImpor_transformed <- DataEksporMigas_lagged$IHBPImpor - rhomodel2 * lagged_IHBPImpor[-1]
DataEksporMigas_lagged$GDP_transformed <- DataEksporMigas_lagged$GDP - rhomodel2 * lagged_GDP[-1]
DataEksporMigas_lagged$M1_transformed <- DataEksporMigas_lagged$M1 - rhomodel2 * lagged_M1[-1]
DataEksporMigas_lagged$M2_transformed <- DataEksporMigas_lagged$M2 - rhomodel2 * lagged_M2[-1]
DataEksporMigas_lagged$Inflasi_Umum_transformed <- DataEksporMigas_lagged$Inflasi_Umum - rhomodel2 * lagged_Inflasi_Umum[-1]
DataEksporMigas_lagged$AdjClose_JKLQ45_transformed <- DataEksporMigas_lagged$`AdjClose_ JKLQ45` - rhomodel2 * lagged_AdjClose_JKLQ45[-1]
DataEksporMigas_lagged$AdjClose_JKSE_transformed <- DataEksporMigas_lagged$`AdjClose_ JKSE` - rhomodel2 * lagged_AdjClose_JKSE[-1]
DataEksporMigas_lagged$BI_Rate_transformed <- DataEksporMigas_lagged$BI_Rate - rhomodel2 * lagged_BI_Rate[-1]
DataEksporMigas_lagged$Harga_Emas_transformed <- DataEksporMigas_lagged$Harga_Emas - rhomodel2 * lagged_Harga_Emas[-1]
#MANUALLY EXTRACT Y AND X
# Transform the data
# Create the model matrix for transformed predictors
X_transformed <- model.matrix(~ USD_transformed + JPY_transformed + GBP_transformed + CHF_transformed +
                                SGD_transformed + MYR_transformed + HKD_transformed + AUD_transformed +
                                CAD_transformed + IHBPEkspor_transformed + IHBPImpor_transformed +
                                GDP_transformed + M1_transformed + M2_transformed + Inflasi_Umum_transformed +
                                AdjClose_JKLQ45_transformed + AdjClose_JKSE_transformed + BI_Rate_transformed +
                                Harga_Emas_transformed - 1, data = DataEksporMigas_lagged)

# Extract the transformed response variable
y_transformed <- as.matrix(DataEksporMigas_lagged$EksporMigas_transformed)
#Uji Multikolinearitas
#untuk dicek dan dibandingkan apakah sama dengan hasil model2_co
lm_model <- lm(EksporMigas_transformed ~ USD_transformed + JPY_transformed + GBP_transformed + CHF_transformed +
                 SGD_transformed + MYR_transformed + HKD_transformed + AUD_transformed +
                 CAD_transformed + IHBPEkspor_transformed + IHBPImpor_transformed +
                 GDP_transformed + M1_transformed + M2_transformed + Inflasi_Umum_transformed +
                 AdjClose_JKLQ45_transformed + AdjClose_JKSE_transformed + BI_Rate_transformed +
                 Harga_Emas_transformed, data = DataEksporMigas_lagged)
# View the summary of the model
summary(lm_model)
vif(lm_model)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#SPLIT DATA
n <- 146
split_index <- floor(0.82* n) 
#Pisahkan data berdasarkan indeks split_index
train_data <- DataEksporImpor[1:split_index, ]
test_data <- DataEksporImpor[(split_index + 1):n, ]
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#EKSPOR MIGAS
# Convert data to matrix for model fitting
X_train <- as.matrix(train_data[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHBPEkspor","IHBPImpor","GDP","M1","M2","Inflasi_Umum","AdjClose_ JKLQ45","AdjClose_ JKSE","BI_Rate","Harga_Emas")])
y_train <- as.matrix(train_data[, "EksporMigas"])
X_test <- as.matrix(test_data[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHBPEkspor","IHBPImpor","GDP","M1","M2","Inflasi_Umum","AdjClose_ JKLQ45","AdjClose_ JKSE","BI_Rate","Harga_Emas")])
y_test <- as.matrix(test_data[, "EksporMigas"])
#Pisahkan data berdasarkan indeks split_index
train_data_x <- X_transformed[1:split_index, ]
train_data_y <- y_transformed[1:split_index, ]
test_data_x <- X_transformed[(split_index + 1):n, ]
test_data_y <- y_transformed[(split_index + 1):n, ]
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#FUNGSI RSQR
# Define R-squared function
rsquared <- function(actual, predicted) {
  ss_total <- sum((actual - mean(actual))^2)
  ss_res <- sum((actual - predicted)^2)
  return(1 - (ss_res / ss_total))
}
# Define MAPE function
mape <- function(actual, predicted) {
  return(mean(abs((actual - predicted) / actual)) * 100)
}
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ML REGRESSION
train_data_combined <- data.frame(train_data_y, train_data_x)
# Membuat formula model
formulaemn <- as.formula("train_data_y ~ .")
# Membangun model
modeleksormigasnew <- lm(formulaemn, data = train_data_combined)
summary(modeleksormigasnew)

y_pred_test0 <- predict(modeleksormigasnew, as.data.frame(test_data_x))
y_pred_train0 <- predict(modeleksormigasnew, as.data.frame(train_data_x))

#to transform back the cochranne orcutt
y_test_transformedback0 <- numeric(length(y_pred_test0))
y_test_transformedback0[1] <- y_pred_test0[1]
for (i in 2:length(y_pred_test0)) {
  y_test_transformedback0[i] <- y_pred_test0[i] + rhomodel2 * y_test_transformedback0[i - 1]
}
y_test_transformedback0

y_train_transformedback0 <- numeric(length(y_pred_train0))
y_train_transformedback0[1] <- y_pred_train0[1]
for (i in 2:length(y_pred_train0)) {
  y_train_transformedback0[i] <- y_pred_train0[i] + rhomodel2 * y_train_transformedback0[i - 1]
}
y_train_transformedback0

mape(y_test,y_test_transformedback0)
RMSE(y_test,y_test_transformedback0)
rsquared(y_test,y_test_transformedback0)
mape(y_train, y_train_transformedback0)
RMSE(y_train, y_train_transformedback0)
rsquared(y_train, y_train_transformedback0)

#---------------------------------------------------------------------------------------------------------
#RIDGE REGRESSION
mean_best_lambda <- numeric(100)
exp_mean_best_lambda <- numeric(100)
for (j in 1:100) {
  best_lambdas <- numeric(100)
  for (i in 1:100) {
    # Setting the lambda values for ridge regression
    lambdas_to_try <- 10^seq(10, -10, length.out = 100)
    
    # Perform Lasso regression with cross-validation
    ridge_cv <- cv.glmnet(train_data_x, train_data_y, alpha = 0, lambda = lambdas_to_try, standardize = TRUE)
    
    # Simpan lambda terbaik di iterasi ini
    best_lambdas[i] <- ridge_cv$lambda.min
  }
  filtered_lambdas <- best_lambdas[best_lambdas < 500]
  ln_filtered_lambdas <- log(filtered_lambdas)
  mean_best_lambda[j] <-mean(filtered_lambdas)
  exp_mean_best_lambda[j] <-exp(mean(ln_filtered_lambdas))
}
mean_best_lambda
hist(mean_best_lambda)
ks_test1 <- ks.test(mean_best_lambda, "pnorm", mean = mean(mean_best_lambda), sd = sd(mean_best_lambda))
ks_test1
exp_mean_best_lambda
hist(exp_mean_best_lambda)
ks_test2 <- ks.test(exp_mean_best_lambda, "pnorm", mean = mean(exp_mean_best_lambda), sd = sd(exp_mean_best_lambda))
ks_test2

# Fit with mean_best_lambda ------------------------------------------------------------------------------------
model_cv <- glmnet(train_data_x, train_data_y, alpha = 0, lambda = mean(mean_best_lambda) , standardize = TRUE)
y_pred_test <- predict(model_cv, test_data_x)
y_pred_train <- predict(model_cv, train_data_x)
coef(model_cv)

#to transform back the cochranne orcutt
y_test_transformedback <- numeric(length(y_pred_test))
y_test_transformedback[1] <- y_pred_test[1]
for (i in 2:length(y_pred_test)) {
  y_test_transformedback[i] <- y_pred_test[i] + rhomodel2 * y_test_transformedback[i - 1]
}
y_test_transformedback

y_train_transformedback <- numeric(length(y_pred_train))
y_train_transformedback[1] <- y_pred_train[1]
for (i in 2:length(y_pred_train)) {
  y_train_transformedback[i] <- y_pred_train[i] + rhomodel2 * y_train_transformedback[i - 1]
}
y_train_transformedback


# Create scatter plot with reference line
plot(y_test, y_pred_test, 
     main = "Predicted vs Actual Values",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_train, 
     main = "Predicted vs Actual Values",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_test_transformedback = y_test_transformedback, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_test_transformedback, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) Testing dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

# Create the time series plot
datestrain <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 119)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_train_transformedback = y_train_transformedback, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_train_transformedback, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) Training dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

y_pred_all <- matrix(c(y_train_transformedback, y_test_transformedback), 
                     nrow = length(y_train_transformedback) + length(y_test_transformedback), 
                     ncol = 1)
y_actual <- DataEksporMigas_lagged$EksporMigas
datesall <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 146)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all = y_pred_all, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

MAPE(y_test,y_test_transformedback)
RMSE(y_test,y_test_transformedback)
rsquared(y_test,y_test_transformedback)
MAPE(y_train, y_train_transformedback)
RMSE(y_train, y_train_transformedback)
rsquared(y_train, y_train_transformedback)
mape(y_train, y_train_transformedback)
mape(y_test,y_test_transformedback)

# Fit with exp_mean_best_lambda ------------------------------------------------------------------------------------
model_cv1 <- glmnet(train_data_x, train_data_y, alpha = 0, lambda = mean(exp_mean_best_lambda) , standardize = TRUE)
y_pred_test1 <- predict(model_cv1, test_data_x)
y_pred_train1 <- predict(model_cv1, train_data_x)
coef(model_cv1)

#to transform back the cochranne orcutt
y_test_transformedback1 <- numeric(length(y_pred_test1))
y_test_transformedback1[1] <- y_pred_test1[1]
for (i in 2:length(y_pred_test1)) {
  y_test_transformedback1[i] <- y_pred_test1[i] + rhomodel2 * y_test_transformedback1[i - 1]
}
y_test_transformedback1

y_train_transformedback1<- numeric(length(y_pred_train1))
y_train_transformedback1[1] <- y_pred_train1[1]
for (i in 2:length(y_pred_train1)) {
  y_train_transformedback1[i] <- y_pred_train1[i] + rhomodel2 * y_train_transformedback1[i - 1]
}
y_train_transformedback1

# Create scatter plot with reference line
plot(y_test, y_test_transformedback1, 
     main = "Predicted vs Actual Values",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_train1, 
     main = "Predicted vs Actual Values",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_test_transformedback1 = y_test_transformedback1, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_test_transformedback1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas Testing dengan Metode Regresi RIDGE (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all = y_pred_all, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

# Create the time series plot
datestrain <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 119)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_train_transformedback1 = y_train_transformedback1, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_train_transformedback1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas Training dengan Metode Regresi RIDGE (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all = y_pred_all, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

y_pred_all1 <- matrix(c(y_train_transformedback1, y_test_transformedback1), 
                     nrow = length(y_train_transformedback1) + length(y_test_transformedback1), 
                     ncol = 1)
y_actual <- DataEksporMigas_lagged$EksporMigas
datesall <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 146)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all1 = y_pred_all1, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Time Series Plot of Predicted vs Actual Values (Ridge Regression for Ekspor Migas (dengan Transformasi Log))",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

MAPE(y_test,y_test_transformedback1)
RMSE(y_test,y_test_transformedback1)
rsquared(y_test,y_test_transformedback1)
MAPE(y_train, y_train_transformedback1)
RMSE(y_train, y_train_transformedback1)
rsquared(y_train, y_train_transformedback1)
mape(y_train, y_train_transformedback1)
mape(y_test,y_test_transformedback1)

#---------------------------------------------------------------------------------------------------------
#LASSO REGRESSION
mean_best_lambda1 <- numeric(100)
exp_mean_best_lambda1 <- numeric(100)
for (j in 1:100) {
  best_lambdas1 <- numeric(100)
  for (i in 1:100) {
    # Setting the lambda values for ridge regression
    lambdas_to_try1 <- 10^seq(10, -10, length.out = 100)
    
    # Perform Lasso regression with cross-validation
    lasso_cv <- cv.glmnet(train_data_x, train_data_y, alpha = 1, lambda = lambdas_to_try1, standardize = TRUE)
    
    # Simpan lambda terbaik di iterasi ini
    best_lambdas1[i] <- lasso_cv$lambda.min
  }
  filtered_lambdas1 <- best_lambdas1[best_lambdas1 < 500]
  ln_filtered_lambdas1 <- log(filtered_lambdas1)
  mean_best_lambda1[j] <-mean(filtered_lambdas1)
  exp_mean_best_lambda1[j] <-exp(mean(ln_filtered_lambdas1))
}
mean_best_lambda1
hist(mean_best_lambda1)
ks_test3 <- ks.test(mean_best_lambda1, "pnorm", mean = mean(mean_best_lambda1), sd = sd(mean_best_lambda1))
ks_test3
exp_mean_best_lambda1
hist(exp_mean_best_lambda)
ks_test4 <- ks.test(exp_mean_best_lambda1, "pnorm", mean = mean(exp_mean_best_lambda1), sd = sd(exp_mean_best_lambda1))
ks_test4

# Fit with mean_best_lambda1 ------------------------------------------------------------------------------------
lasso_model_cv <- glmnet(train_data_x, train_data_y, alpha = 1, lambda = mean(mean_best_lambda1) , standardize = TRUE)
y_pred_lasso_test <- predict(lasso_model_cv, test_data_x)
y_pred_lasso_train <- predict(lasso_model_cv, train_data_x)
coef(lasso_model_cv)

#to transform back the cochranne orcutt
y_test_transformedback2 <- numeric(length(y_pred_lasso_test))
y_test_transformedback2[1] <- y_pred_lasso_test[1]
for (i in 2:length(y_pred_lasso_test)) {
  y_test_transformedback2[i] <- y_pred_lasso_test[i] + rhomodel2 * y_test_transformedback2[i - 1]
}
y_test_transformedback2

y_train_transformedback2<- numeric(length(y_pred_lasso_train))
y_train_transformedback2[1] <- y_pred_lasso_train[1]
for (i in 2:length(y_pred_lasso_train)) {
  y_train_transformedback2[i] <- y_pred_lasso_train[i] + rhomodel2 * y_train_transformedback2[i - 1]
}
y_train_transformedback2


# Create scatter plot with reference line
plot(y_test, y_test_transformedback2, 
     main = "Predicted vs Actual Values Testing data (LASSO Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train,y_train_transformedback2, 
     main = "Predicted vs Actual Values Training data (LASSO Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_test_transformedback2 = y_test_transformedback2, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_test_transformedback2, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas Testing (Jan 2012- Mar 2024) dengan Metode Regresi LASSO (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

# Create the time series plot
datestrain <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 119)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_train_transformedback2 = y_train_transformedback2, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_train_transformedback2, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas Training (Jan 2012- Mar 2024) dengan Metode Regresi LASSO (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 8)) +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )


y_pred_all_lasso <- matrix(c(y_train_transformedback2, y_test_transformedback2), 
                      nrow = length(y_train_transformedback2) + length(y_test_transformedback2), 
                      ncol = 1)
y_actual <- DataEksporMigas_lagged$EksporMigas
datesall <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 146)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_lasso = y_pred_all_lasso, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_lasso, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) dengan Metode Regresi LASSO (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )


MAPE(y_test,y_test_transformedback2)
RMSE(y_test,y_test_transformedback2)
rsquared(y_test,y_test_transformedback2)
MAPE(y_train, y_train_transformedback2)
RMSE(y_train, y_train_transformedback2)
rsquared(y_train, y_train_transformedback2)
mape(y_train, y_train_transformedback2)
mape(y_test,y_test_transformedback2)

# Fit with exp_mean_best_lambda1 ------------------------------------------------------------------------------------
lasso_model_cv1 <- glmnet(train_data_x, train_data_y, alpha = 1, lambda = mean(exp_mean_best_lambda1) , standardize = TRUE)
y_pred_lasso_test1 <- predict(lasso_model_cv1, test_data_x)
y_pred_lasso_train1 <- predict(lasso_model_cv1, train_data_x)
coef(lasso_model_cv1)

#to transform back the cochranne orcutt
y_test_transformedback3 <- numeric(length(y_pred_lasso_test1))
y_test_transformedback3[1] <- y_pred_lasso_test1[1]
for (i in 2:length(y_pred_lasso_test1)) {
  y_test_transformedback3[i] <- y_pred_lasso_test1[i] + rhomodel2 * y_test_transformedback3[i - 1]
}
y_test_transformedback3

y_train_transformedback3<- numeric(length(y_pred_lasso_train1))
y_train_transformedback3[1] <- y_pred_lasso_train1[1]
for (i in 2:length(y_pred_lasso_train1)) {
  y_train_transformedback3[i] <- y_pred_lasso_train1[i] + rhomodel2 * y_train_transformedback3[i - 1]
}
y_train_transformedback3

# Create scatter plot with reference line
plot(y_test, y_test_transformedback3, 
     main = "Predicted vs Actual Values Testing data (LASSO Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_train_transformedback3, 
     main = "Predicted vs Actual Values Training data (LASSO Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_test_transformedback3 = y_test_transformedback3, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_test_transformedback3, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas Testing dengan Metode Regresi LASSO (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all = y_pred_all, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

# Create the time series plot
datestrain <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 119)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_train_transformedback3 = y_train_transformedback3, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_train_transformedback3, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas Training dengan Metode Regresi LASSO (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all = y_pred_all, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

y_pred_all_lasso1 <- matrix(c(y_train_transformedback3, y_test_transformedback3), 
                           nrow = length(y_train_transformedback3) + length(y_test_transformedback3), 
                           ncol = 1)
y_actual <- DataEksporMigas_lagged$EksporMigas
datesall <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 146)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_lasso1 = y_pred_all_lasso1, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_lasso1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas dengan Metode Regresi LASSO (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )


MAPE(y_test,y_test_transformedback3)
RMSE(y_test,y_test_transformedback3)
rsquared(y_test,y_test_transformedback3)
MAPE(y_train,y_train_transformedback3)
RMSE(y_train, y_train_transformedback3)
rsquared(y_train, y_train_transformedback3)
mape(y_train, y_train_transformedback3)
mape(y_test,y_test_transformedback3)

#---------------------------------------------------------------------------------------------------------
#ELASTIC NET REGRESSION
tune_grid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),
  lambda = 10^seq(-5, 5, length = 100)
)

control <- trainControl(method = "repeatedcv", 
                        number = 5, 
                        repeats = 5, 
                        search = "grid", 
                        verboseIter = TRUE)
mean_best_lambda2 <- numeric(100)
exp_mean_best_lambda2 <- numeric(100)
mean_best_alphas2 <- numeric(100)
exp_mean_best_alphas2 <- numeric(100)
#last: 54
for (j in 98:100) {
  # Set up storage for best alpha and lambda values
  best_alphas2 <- numeric(100)
  best_lambdas2 <- numeric(100)
  # Loop to run the model 100 times
  for (i in 1:100) {
    # Print iteration info
    cat("Iteration:", i, "\n")
    
    # Training Elastic Net Regression model with cross-validation
    elastic_model <- train(train_data_y ~ ., 
                            data = cbind(train_data_x, train_data_y), 
                            method = "glmnet", 
                            preProcess = c("center", "scale"), 
                            tuneGrid = tune_grid,   # Grid search over specified hyperparameters
                            trControl = control)
    
    # Store the best alpha and lambda from this iteration
    best_alphas2[i] <- elastic_model$bestTune$alpha
    best_lambdas2[i] <- elastic_model$bestTune$lambda
    
    # Optionally print the best alpha and lambda for this iteration
    cat("Best alpha:", best_alphas2[i], "Best lambda:", best_lambdas2[i], "\n")
  }
  filtered_lambdas2 <- best_lambdas2[best_lambdas2>10]
  ln_filtered_lambdas2 <- log(filtered_lambdas2)
  mean_best_lambda2[j] <-mean(filtered_lambdas2)
  exp_mean_best_lambda2[j] <-exp(mean(ln_filtered_lambdas2))
  ln_filtered_alphas2 <- log(best_alphas2)
  mean_best_alphas2[j] <-mean(best_alphas2)
  exp_mean_best_alphas2[j] <-exp(mean(ln_filtered_alphas2))
}
mean_best_lambda2
exp_mean_best_lambda2
mean_best_alphas2
exp_mean_best_alphas2

hist(mean_best_lambda2)
ks_test5 <- ks.test(mean_best_lambda2, "pnorm", mean = mean(mean_best_lambda2), sd = sd(mean_best_lambda2))
ks_test5
hist(exp_mean_best_lambda2)
ks_test6 <- ks.test(exp_mean_best_lambda2, "pnorm", mean = mean(exp_mean_best_lambda2), sd = sd(exp_mean_best_lambda2))
ks_test6

# Fit with mean_best_lambda2 and mean_best_alphas2 ------------------------------------------------------------------------------------
enet_model_cv <- glmnet(X_train, y_train, alpha = mean(mean_best_alphas2), lambda = mean(mean_best_lambda2) , standardize = TRUE)
y_pred_enet_test <- predict(enet_model_cv, X_test)
y_pred_enet_train <- predict(enet_model_cv, X_train)
coef(enet_model_cv)

#to transform back the cochranne orcutt
y_test_transformedback4 <- numeric(length(y_pred_enet_test))
y_test_transformedback4[1] <-y_pred_enet_test[1]
for (i in 2:length(y_pred_enet_test)) {
  y_test_transformedback4[i] <- y_pred_enet_test[i] + rhomodel2 * y_test_transformedback4[i - 1]
}
y_test_transformedback4

y_train_transformedback4<- numeric(length(y_pred_enet_train))
y_train_transformedback4[1] <- y_pred_enet_train[1]
for (i in 2:length(y_pred_enet_train)) {
  y_train_transformedback4[i] <- y_pred_enet_train[i] + rhomodel2 * y_train_transformedback4[i - 1]
}
y_train_transformedback4


# Create scatter plot with reference line
plot(y_test, y_test_transformedback4, 
     main = "Predicted vs Actual Values Testing data (enet Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_train_transformedback4, 
     main = "Predicted vs Actual Values Training data (enet Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_test_transformedback4 = y_test_transformedback4, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_test_transformedback4, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas Testing (Jan 2012- Mar 2024) dengan Metode Regresi Elastic Net (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

# Create the time series plot
datestrain <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 119)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_train_transformedback4 = y_train_transformedback4, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_train_transformedback4, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) Training dengan Metode Regresi Elastic Net (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

y_pred_all_enet <- matrix(c(y_train_transformedback4, y_test_transformedback4), 
                            nrow = length(y_train_transformedback4) + length(y_test_transformedback4), 
                            ncol = 1)
y_actual <- DataEksporMigas_lagged$EksporMigas
datesall <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 146)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_enet = y_pred_all_enet, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_enet, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas dengan Metode Regresi Elastic Net (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

mape(y_test,y_test_transformedback4)
RMSE(y_test,y_test_transformedback4)
rsquared(y_test,y_test_transformedback4)
mape(y_train,y_train_transformedback4)
RMSE(y_train, y_train_transformedback4)
rsquared(y_train, y_train_transformedback4)
# Fit with exp_mean_best_lambda2 and exp_mean_best_alphas2 ------------------------------------------------------------------------------------
enet_model_cv1 <- glmnet(X_train, y_train, alpha = mean(exp_mean_best_alphas2), lambda = mean(exp_mean_best_lambda2) , standardize = TRUE)
y_pred_enet_test1 <- predict(enet_model_cv1, X_test)
y_pred_enet_train1 <- predict(enet_model_cv1, X_train)
coef(enet_model_cv1)

#to transform back the cochranne orcutt
y_test_transformedback5 <- numeric(length(y_pred_enet_test1))
y_test_transformedback5[1] <-y_pred_enet_test1[1]
for (i in 2:length(y_pred_enet_test1)) {
  y_test_transformedback5[i] <- y_pred_enet_test1[i] + rhomodel2 * y_test_transformedback5[i - 1]
}
y_test_transformedback5

y_train_transformedback5<- numeric(length(y_pred_enet_train1))
y_train_transformedback5[1] <- y_pred_enet_train1[1]
for (i in 2:length(y_pred_enet_train1)) {
  y_train_transformedback5[i] <- y_pred_enet_train1[i] + rhomodel2 * y_train_transformedback5[i - 1]
}
y_train_transformedback5


# Create scatter plot with reference line
plot(y_test, y_test_transformedback5, 
     main = "Predicted vs Actual Values Testing data (enet Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_train_transformedback5, 
     main = "Predicted vs Actual Values Training data (enet Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_test_transformedback5 = y_test_transformedback5, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_test_transformedback5, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas Testing (Jan 2012- Mar 2024) dengan Metode Regresi Elastic Net (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )


# Create the time series plot
datestrain <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 119)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_train_transformedback5 = y_train_transformedback5, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_train_transformedback5, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Mar 2024) Training dengan Metode Regresi Elastic Net (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

y_pred_all_enet1 <- matrix(c(y_train_transformedback5, y_test_transformedback5), 
                          nrow = length(y_train_transformedback5) + length(y_test_transformedback5), 
                          ncol = 1)
y_actual <- DataEksporMigas_lagged$EksporMigas
datesall <- seq(from = as.Date("2012-02-01"), by = "month", length.out = 146)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_enet1 = y_pred_all_enet1, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_enet1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas dengan Metode Regresi Elastic Net (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

mape(y_test,y_test_transformedback5)
RMSE(y_test,y_test_transformedback5)
rsquared(y_test,y_test_transformedback5)
mape(y_train,y_train_transformedback5)
RMSE(y_train, y_train_transformedback5)
rsquared(y_train, y_train_transformedback5)