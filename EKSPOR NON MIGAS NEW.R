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
spearman_rho_results <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$EksporNonMigas, method = "spearman"))
print(spearman_rho_results)

#korelasi kendall's-tau
independent_variables <- DataEksporImpor[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHBPEkspor","IHBPImpor","GDP","M1","M2","Inflasi_Umum","AdjClose_ JKLQ45","AdjClose_ JKSE","BI_Rate","Harga_Emas")]
kendall_tau_results <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$EksporNonMigas, method = "kendall"))
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
#UJI ASUMSI KLASIK EKSPOR NON MIGAS
#UJI NORMALITAS
# Q-Q Plot
qqnorm(residuals(model3))
qqline(residuals(model3))
# Uji 1 Sample KS
#H0: Data berasal dari distribusi normal.
#H1: Data tidak berasal dari distribusi normal.
# Mengekstrak residual
residuals <- residuals(model3)
# Melakukan uji Kolmogorov-Smirnov pada residual
ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
ks_test 
#karena pvalue>0.05, maka belum ada bukti cukup untuk menolak h0 yang menyatakan data berasal dri dist normal

#UJI HOMOSKEDASITAS
#H0 : Variansi model konstan (homoscedacity)
#HA : Variansi model tidak konstan (heteroscedacity)
# Uji Breusch-Pagan
bptest(model3)
#Karena P-val>0.05, maka H0 belum bisa ditolak sehingga Variansi  konstan

#UJI AUTOKORELASI RESIDUAL
#H0: Tidak ada autokorelasi dalam residual dari model regresi.
#H1: Ada autokorelasi dalam residual dari model regresi.
# Uji Durbin-Watson
dwtest(model3)
#karena pval lebih besar dari 0.05, maka H0 belum bisa ditolak sehingga tidak ada korelasi antara residual

#UJI MULTIKOLINEARITAS
(vif1 <- data.frame(vif(model3)))
#Jika ada nilai VIF > 10: Ada indikasi multikolinearitas yang signifikan.
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#SPLIT DATA
n <- nrow(DataEksporImpor)
split_index <- floor(0.82* n) 
#Pisahkan data berdasarkan indeks split_index
train_data <- DataEksporImpor[1:split_index, ]
test_data <- DataEksporImpor[(split_index + 1):n, ]
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#EKSPOR NON MIGAS
# Convert data to matrix for model fitting
X_train <- as.matrix(train_data[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHBPEkspor","IHBPImpor","GDP","M1","M2","Inflasi_Umum","AdjClose_ JKLQ45","AdjClose_ JKSE","BI_Rate","Harga_Emas")])
y_train <- as.matrix(train_data[, "EksporNonMigas"])
X_test <- as.matrix(test_data[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHBPEkspor","IHBPImpor","GDP","M1","M2","Inflasi_Umum","AdjClose_ JKLQ45","AdjClose_ JKSE","BI_Rate","Harga_Emas")])
y_test <- as.matrix(test_data[, "EksporNonMigas"])
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#FUNGSI RSQR
# Define R-squared function
rsquared <- function(actual, predicted) {
  ss_total <- sum((actual - mean(actual))^2)
  ss_res <- sum((actual - predicted)^2)
  return(1 - (ss_res / ss_total))
}
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ML REGRESSION
train_data_combined <- as.data.frame(X_train)
train_data_combined$y_train <- y_train

# Membuat formula model
formulaenm <- as.formula("y_train ~ .")
# Membangun model
modelekspornonmigas <- lm(formulaenm, data = train_data_combined)
options(scipen = 999)
summary(modelekspornonmigas)

y_pred_test0 <- predict(modelekspornonmigas, as.data.frame(X_test))
y_pred_train0 <- predict(modelekspornonmigas, as.data.frame(X_train))

MAPE(y_test,y_pred_test0)
RMSE(y_test,y_pred_test0)
rsquared(y_test,y_pred_test0)
MAPE(y_train, y_pred_train0)
RMSE(y_train, y_pred_train0)
rsquared(y_train, y_pred_train0)
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
    ridge_cv <- cv.glmnet(X_train, y_train, alpha = 0, lambda = lambdas_to_try, standardize = TRUE)
    
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
model_cv <- glmnet(X_train, y_train, alpha = 0, lambda = mean(mean_best_lambda) , standardize = TRUE)
y_pred_test <- predict(model_cv, X_test)
y_pred_train <- predict(model_cv, X_train)
coef(model_cv)

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
datatest <- data.frame(Datetest = datestest, y_pred_test = y_pred_test, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_test, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Testing dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 120)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_train = y_pred_train, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_train, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Training dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
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



y_pred_all <- matrix(c(y_pred_train, y_pred_test), 
                     nrow = length(y_pred_train) + length(y_pred_test), 
                     ncol = 1)
y_actual <- DataEksporImpor$Impor
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 147)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all = y_pred_all, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
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


MAPE(y_test,y_pred_test)
RMSE(y_test,y_pred_test)
rsquared(y_test,y_pred_test)
MAPE(y_train, y_pred_train)
RMSE(y_train, y_pred_train)
rsquared(y_train, y_pred_train)

# Fit with exp_mean_best_lambda ------------------------------------------------------------------------------------
model_cv1 <- glmnet(X_train, y_train, alpha = 0, lambda = mean(exp_mean_best_lambda) , standardize = TRUE)
y_pred_test1 <- predict(model_cv1, X_test)
y_pred_train1 <- predict(model_cv1, X_train)
coef(model_cv1)


# Create scatter plot with reference line
plot(y_test, y_pred_test1, 
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
datatest <- data.frame(Datetest = datestest, y_pred_test1 = y_pred_test1, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_test1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Testing dengan Metode Regresi Ridge (Transformasi Log)",
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 120)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_train1 = y_pred_train1, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_train1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Training dengan Metode Regresi Ridge (Transformasi Log)",
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


y_pred_all1 <- matrix(c(y_pred_train1, y_pred_test1), 
                      nrow = length(y_pred_train1) + length(y_pred_test1), 
                      ncol = 1)
y_actual <- DataEksporImpor$Impor
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 147)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all1 = y_pred_all1, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas dengan Metode Regresi Ridge (Transformasi Log)",
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


MAPE(y_test,y_pred_test1)
RMSE(y_test,y_pred_test1)
rsquared(y_test,y_pred_test1)
MAPE(y_train, y_pred_train1)
RMSE(y_train, y_pred_train1)
rsquared(y_train, y_pred_train1)

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
    lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, lambda = lambdas_to_try1, standardize = TRUE)
    
    # Simpan lambda terbaik di iterasi ini
    best_lambdas1[i] <- lasso_cv$lambda.min
  }
  filtered_lambdas1 <- best_lambdas1[best_lambdas1 < 100]
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
lasso_model_cv <- glmnet(X_train, y_train, alpha = 1, lambda = mean(mean_best_lambda1) , standardize = TRUE)
y_pred_lasso_test <- predict(lasso_model_cv, X_test)
y_pred_lasso_train <- predict(lasso_model_cv, X_train)
coef(lasso_model_cv)


# Create scatter plot with reference line
plot(y_test, y_pred_lasso_test, 
     main = "Predicted vs Actual Values Testing data (LASSO Regression for Ekspor Non Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_lasso_train, 
     main = "Predicted vs Actual Values Training data (LASSO Regression for Ekspor Non Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line


# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_lasso_test = y_pred_lasso_test, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_lasso_test, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Testing dengan Metode Regresi LASSO (Tanpa Transformasi Log)",
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 120)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_lasso_train = y_pred_lasso_train, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_lasso_train, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Training dengan Metode Regresi LASSO (Tanpa Transformasi Log)",
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

y_pred_all_lasso <- matrix(c(y_pred_lasso_train, y_pred_lasso_test), 
                           nrow = length(y_pred_lasso_train) + length(y_pred_lasso_test), 
                           ncol = 1)
y_actual <- DataEksporImpor$Impor
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 147)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_lasso = y_pred_all_lasso, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_lasso, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas dengan Metode Regresi LASSO (Tanpa Transformasi Log)",
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


MAPE(y_test,y_pred_lasso_test)
RMSE(y_test,y_pred_lasso_test)
rsquared(y_test,y_pred_lasso_test)
MAPE(y_train, y_pred_lasso_train)
RMSE(y_train, y_pred_lasso_train)
rsquared(y_train, y_pred_lasso_train)

# Fit with exp_mean_best_lambda1 ------------------------------------------------------------------------------------
lasso_model_cv1 <- glmnet(X_train, y_train, alpha = 1, lambda = mean(exp_mean_best_lambda1) , standardize = TRUE)
y_pred_lasso_test1 <- predict(lasso_model_cv1, X_test)
y_pred_lasso_train1 <- predict(lasso_model_cv1, X_train)
coef(lasso_model_cv1)


# Create scatter plot with reference line
plot(y_test, y_pred_lasso_test1, 
     main = "Predicted vs Actual Values Testing data (LASSO Regression for Ekspor Non Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_lasso_train1, 
     main = "Predicted vs Actual Values Training data (LASSO Regression for Ekspor Non Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_lasso_test1 = y_pred_lasso_test1, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_lasso_test1, color = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Testing dengan Metode Regresi LASSO (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 120)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_lasso_train1 = y_pred_lasso_train1, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_lasso_train, color = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Training dengan Metode Regresi LASSO (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  theme_minimal()+
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16),  # Increase size of x-axis label
    axis.title.y = element_text(size = 16),  # Increase size of y-axis label
    axis.text.x = element_text(size = 16),   # Increase size of x-axis numbers
    axis.text.y = element_text(size = 16)    # Increase size of y-axis numbers
  )

y_pred_all_lasso1 <- matrix(c(y_pred_lasso_train1, y_pred_lasso_test1), 
                            nrow = length(y_pred_lasso_train1) + length(y_pred_lasso_test1), 
                            ncol = 1)
y_actual <- DataEksporImpor$Impor
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 147)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_lasso1 = y_pred_all_lasso1, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_lasso1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas dengan Metode Regresi LASSO (Transformasi Log)",
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


MAPE(y_test,y_pred_lasso_test1)
RMSE(y_test,y_pred_lasso_test1)
rsquared(y_test,y_pred_lasso_test1)
MAPE(y_train, y_pred_lasso_train1)
RMSE(y_train, y_pred_lasso_train1)
rsquared(y_train, y_pred_lasso_train1)

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
#last= 57
for (j in 95:100) {
  # Set up storage for best alpha and lambda values
  best_alphas2 <- numeric(100)
  best_lambdas2 <- numeric(100)
  # Loop to run the model 100 times
  for (i in 1:100) {
    # Print iteration info
    cat("Iteration:", i, "\n")
    
    # Training Elastic Net Regression model with cross-validation
    elastic_model <- train(EksporNonMigas ~ ., 
                           data = cbind(X_train, y_train), 
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
  filtered_lambdas2 <- best_lambdas2[best_lambdas2 < 100]
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

# Create scatter plot with reference line
plot(y_test, y_pred_enet_test, 
     main = "Predicted vs Actual Values Testing data (enet Regression for Impor)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_enet_train, 
     main = "Predicted vs Actual Values Training data (enet Regression for Impor)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_enet_test = y_pred_enet_test, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_enet_test, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Testing dengan Metode Regresi Elastic Net (Tanpa Transformasi Log)",
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 120)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_enet_train = y_pred_enet_train, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_enet_train, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas Training dengan Metode Regresi Elastic Net (Tanpa Transformasi Log)",
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


y_pred_all_enet <- matrix(c(y_pred_enet_train, y_pred_enet_test), 
                          nrow = length(y_pred_enet_train) + length(y_pred_enet_test), 
                          ncol = 1)
y_actual <- DataEksporImpor$EksporNonMigas
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 147)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_enet = y_pred_all_enet, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_enet, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Non Migas dengan Metode Regresi Elastic Net (Tanpa Transformasi Log)",
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


MAPE(y_test,y_pred_enet_test)
RMSE(y_test,y_pred_enet_test)
rsquared(y_test,y_pred_enet_test)
MAPE(y_train, y_pred_enet_train)
RMSE(y_train, y_pred_enet_train)
rsquared(y_train, y_pred_enet_train)

# Fit with exp_mean_best_lambda2 and exp_mean_best_alphas2 ------------------------------------------------------------------------------------
enet_model_cv1 <- glmnet(X_train, y_train, alpha = mean(exp_mean_best_alphas2), lambda = mean(exp_mean_best_lambda2) , standardize = TRUE)
y_pred_enet_test1 <- predict(enet_model_cv1, X_test)
y_pred_enet_train1 <- predict(enet_model_cv1, X_train)
coef(enet_model_cv1)

# Create scatter plot with reference line
plot(y_test, y_pred_enet_test1, 
     main = "Predicted vs Actual Values Testing data (enet Regression for Impor)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_enet_train1, 
     main = "Predicted vs Actual Values Training data (enet Regression for Impor)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 27)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_enet_test1 = y_pred_enet_test1, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_enet_test1, color = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual"), size = 1) +
  labs(title = "Time Series Plot of Predicted vs Actual Values Testing data (enet Regression for Ekspor Non Migas)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  theme_minimal()

# Create the time series plot
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 120)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_enet_train1 = y_pred_enet_train1, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_enet_train1, color = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual"), size = 1) +
  labs(title = "Time Series Plot of Predicted vs Actual Values Training data (enet Regression for Ekspor Non Migas)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  theme_minimal()


y_pred_all_enet1 <- matrix(c(y_pred_enet_train1, y_pred_enet_test1), 
                          nrow = length(y_pred_enet_train1) + length(y_pred_enet_test1), 
                          ncol = 1)
y_actual <- DataEksporImpor$EksporNonMigas
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 147)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_enet1 = y_pred_all_enet1, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_enet1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Time Series Plot of Predicted vs Actual Values (Elastic Net Regression for Ekspor Non Migas (dengan Transformasi Log))",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 10))


MAPE(y_test,y_pred_enet_test1)
RMSE(y_test,y_pred_enet_test1)
rsquared(y_test,y_pred_enet_test1)
MAPE(y_train, y_pred_enet_train1)
RMSE(y_train, y_pred_enet_train1)
rsquared(y_train, y_pred_enet_train1)