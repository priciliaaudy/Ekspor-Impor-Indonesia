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
library(readxl)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Define Data
setwd("/Users/audy8/Downloads")
DataEksporImpor<- read_xlsx("Data Ekspor Impor TA.xlsx")
DataEksporImpor
dim(DataEksporImpor)
head(DataEksporImpor)
str(data)
summary(DataEksporImpor)
Type <- sapply(DataEksporImpor, class)
data.frame(Type)

#KORELASI
library(corrplot)
library(dplyr)
#DATA VISUALISASI UNTUK MELIHAT KORELASI
cor_matrix <- DataEksporImpor %>%
  select_if(is.numeric) %>%
  cor()
# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle")

#EKSPOR MIGAS
#korelasi spearman- rho
independent_variables <- DataEksporImpor[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHPB EKSPOR","IHPB IMPOR", "HARGA CRUDE OIL ($ USD)", "GDP ($ B)")]
spearman_rho_results <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$`EKSPOR MIGAS`, method = "spearman"))
print(spearman_rho_results)
#korelasi kendall's-tau
independent_variables <- DataEksporImpor[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHPB EKSPOR","IHPB IMPOR", "HARGA CRUDE OIL ($ USD)", "GDP ($ B)")]
kendall_tau_results <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$`EKSPOR MIGAS`, method = "kendall"))
print(kendall_tau_results)

#EKSPOR NON MIGAS
#korelasi spearman- rho
spearman_rho_results1 <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$`EKSPOR NON MIGAS`, method = "spearman"))
print(spearman_rho_results1)
#korelasi kendall's-tau
kendall_tau_results1 <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$`EKSPOR NON MIGAS`, method = "kendall"))
print(kendall_tau_results1)

#IMPOR
DataEksporImpor$IMPOR <- DataEksporImpor$`IMPOR MIGAS`+ DataEksporImpor$`IMPOR NON MIGAS`
#korelasi spearman- rho
independent_variables2 <- DataEksporImpor[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHPB EKSPOR","IHPB IMPOR", "HARGA CRUDE OIL ($ USD)", "GDP ($ B)")]
spearman_rho_results2 <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$IMPOR, method = "spearman"))
print(spearman_rho_results2)
#korelasi kendall's-tau
independent_variables2 <- DataEksporImpor[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHPB EKSPOR","IHPB IMPOR", "HARGA CRUDE OIL ($ USD)", "GDP ($ B)")]
kendall_tau_results2 <- sapply(independent_variables, function(independent_variables) cor(independent_variables, DataEksporImpor$IMPOR, method = "kendall"))
print(kendall_tau_results2)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#MULTIPLE LINEAR REGRESSION
independent_variables <- DataEksporImpor[, c("USD", "JPY", "GBP", "CHF", "SGD","MYR","HKD","AUD","CAD","IHPB EKSPOR","IHPB IMPOR", "HARGA CRUDE OIL ($ USD)", "GDP ($ B)")]
model3<- lm(DataEksporImpor$`EKSPOR MIGAS` ~ ., data = independent_variables)
summary(model3)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#UJI ASUMSI KLASIK EKSPOR MIGAS
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
ks_test #0.4735
#karena pvalue>0.05, maka belum ada bukti cukup untuk menolak h0 yang menyatakan data berasal dri dist normal

#UJI HOMOSKEDASITAS
#H0 : Variansi model konstan (homoscedacity)
#HA : Variansi model tidak konstan (heteroscedacity)
# Uji Breusch-Pagan
bptest(model3) #0.4546
#Karena P-val>0.05, maka H0 belum bisa ditolak sehingga Variansi  konstan

#UJI AUTOKORELASI RESIDUAL
#H0: Tidak ada autokorelasi dalam residual dari model regresi.
#H1: Ada autokorelasi dalam residual dari model regresi.
# Uji Durbin-Watson
dwtest(model3) #0.7964
#karena pval lebih besar dari 0.05, maka H0 belum bisa ditolak sehingga tidak ada korelasi antara residual

#UJI MULTIKOLINEARITAS
(vif1 <- data.frame(vif(model3)))
#Jika ada nilai VIF > 10: Ada indikasi multikolinearitas yang signifikan.
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#EKSPOR MIGAS DATA LAMA
# Convert data to matrix for model fitting
X_train <- as.matrix(DataEksporImpor[1:108,3:15])
y_train <- as.matrix(DataEksporImpor[1:108,16])
X_test <- as.matrix(DataEksporImpor[109:nrow(DataEksporImpor),3:15])
y_test <- as.matrix(DataEksporImpor[109:nrow(DataEksporImpor),16])
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
formulaemo <- as.formula("y_train ~ .")
# Membangun model
modeleksormigasold <- lm(formulaemo, data = train_data_combined)
options(scipen = 999)
summary(modeleksormigasold)

y_pred_test0 <- predict(modeleksormigasold, as.data.frame(X_test))
y_pred_train0 <- predict(modeleksormigasold, as.data.frame(X_train))

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
  filtered_lambdas <- best_lambdas[best_lambdas > 0.1 & best_lambdas < 2]
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
datestest <- seq(from = as.Date("2021-01-01"), by = "month", length.out = 20)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_test = y_pred_test, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_test, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) Testing dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 108)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_train = y_pred_train, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_train, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) Training dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
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
y_actual <- DataEksporImpor$`EKSPOR MIGAS`
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 128)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all = y_pred_all, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all, color = "Predicted"), linetype = "longdash", size = 1) +
  geom_line(aes(y = y_actual, color = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) dengan Metode Regresi Ridge (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), color = "black", linetype = "dashed", size = 1)+
scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 8))+
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
datestest <- seq(from = as.Date("2021-01-01"), by = "month", length.out = 20)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_test1 = y_pred_test1, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_test1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) Testing dengan Metode Regresi Ridge (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()

# Create the time series plot
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 108)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_train1 = y_pred_train1, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_train1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) Training dengan Metode Regresi Ridge (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()


y_pred_all1 <- matrix(c(y_pred_train1, y_pred_test1), 
                      nrow = length(y_pred_train1) + length(y_pred_test1), 
                      ncol = 1)
y_actual <- DataEksporImpor$`EKSPOR MIGAS`
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 128)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all1 = y_pred_all1, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all1, color = "Predicted"), linetype = "longdash", size = 1) +
  geom_line(aes(y = y_actual, color = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) dengan Metode Regresi Ridge (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), color = "black", linetype = "dashed", size = 1)


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
  filtered_lambdas1 <- best_lambdas1[best_lambdas1 > 0.1  & best_lambdas < 0.3]
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
     main = "Predicted vs Actual Values Testing data (LASSO Regression for Ekspor Migas Old Data)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_lasso_train, 
     main = "Predicted vs Actual Values Training data (LASSO Regression for Ekspor Migas Old Data)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2021-01-01"), by = "month", length.out = 20)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_lasso_test = y_pred_lasso_test, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_lasso_test, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) Testing dengan Metode Regresi LASSO (Tanpa Transformasi Log)",
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 108)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_lasso_train = y_pred_lasso_train, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_lasso_train, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) Training dengan Metode Regresi LASSO (Tanpa Transformasi Log)",
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
y_actual <- DataEksporImpor$`EKSPOR MIGAS`
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 128)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_lasso = y_pred_all_lasso, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_lasso, color = "Predicted"), linetype = "longdash", size = 1) +
  geom_line(aes(y = y_actual, color = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) dengan Metode Regresi LASSO (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), color = "black", linetype = "dashed", size = 1)+
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
     main = "Predicted vs Actual Values Testing data (LASSO Regression for Ekspor Migas Old Data)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_lasso_train1, 
     main = "Predicted vs Actual Values Training data (LASSO Regression for Ekspor Migas Old Data)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line


# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 20)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_lasso_test1 = y_pred_lasso_test1, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_lasso_test1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) Testing dengan Metode Regresi LASSO (Transformasi Log)",
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 108)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_lasso_train1 = y_pred_lasso_train1, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_lasso_train, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) Training dengan Metode Regresi LASSO (Transformasi Log)",
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

y_pred_all_lasso1 <- matrix(c(y_pred_lasso_train1, y_pred_lasso_test1), 
                            nrow = length(y_pred_lasso_train1) + length(y_pred_lasso_test1), 
                            ncol = 1)
y_actual <- DataEksporImpor$`EKSPOR MIGAS`
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 128)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_lasso1 = y_pred_all_lasso1, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_lasso1, color = "Predicted"), linetype = "longdash", size = 1) +
  geom_line(aes(y = y_actual, color = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012- Aug 2022) dengan Metode Regresi LASSO (Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), color = "black", linetype = "dashed", size = 1)+
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
#last= 49
for (j in 50:93) {
  # Set up storage for best alpha and lambda values
  best_alphas2 <- numeric(100)
  best_lambdas2 <- numeric(100)
  # Loop to run the model 100 times
  for (i in 1:100) {
    # Print iteration info
    cat("Iteration:", i, "\n")
    
    # Training Elastic Net Regression model with cross-validation
    elastic_model <- train( `EKSPOR MIGAS` ~ ., 
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
  filtered_lambdas2 <- best_lambdas2[best_lambdas2 <  0.5]
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
     main = "Predicted vs Actual Values Testing data (Elastic net Regression for Ekspor Migas (Tanpa Transformasi Log))",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_enet_train, 
     main = "Predicted vs Actual Values Training data (enet Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 20)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_enet_test = y_pred_enet_test, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_enet_test, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012-Aug 2024) Testing dengan Metode Regresi Elastic Net (Tanpa Transformasi Log)",
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 108)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_enet_train = y_pred_enet_train, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_enet_train, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012-Aug 2024) Training dengan Metode Regresi Elastic Net (Tanpa Transformasi Log)",
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
y_actual <- DataEksporImpor$`EKSPOR MIGAS`
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 128)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_enet = y_pred_all_enet, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_enet, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012-Aug 2024) dengan Metode Regresi Elastic Net (Tanpa Transformasi Log)",
       x = "Date",
       y = "Values") +
  scale_color_manual(name = "Legend", values = c("Predicted" = "blue", "Actual" = "red")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted" = "twodash", "Actual" = "solid")) +
  theme_minimal()+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "black", linetype = "dashed", size = 1)+
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(n = 8))+
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
     main = "Predicted vs Actual Values Testing data (enet Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create scatter plot with reference line
plot(y_train, y_pred_enet_train1, 
     main = "Predicted vs Actual Values Training data (enet Regression for Ekspor Migas)",  
     xlab = "Actual Values",                 
     ylab = "Predicted Values",             
     col = "blue",                          
     pch = 16)
# Add reference line
abline(a = 0, b = 1, col = "red", lwd = 2)  # y = x line

# Create the time series plot
datestest <- seq(from = as.Date("2022-01-01"), by = "month", length.out = 20)
# Create a data frame
datatest <- data.frame(Datetest = datestest, y_pred_enet_test1 = y_pred_enet_test1, y_test = y_test)
ggplot(datatest, aes(x = Datetest)) +
  geom_line(aes(y = y_pred_enet_test1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_test, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012-Mar 2024) Testing dengan Metode Regresi Elastic Net (Transformasi Log)",
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
datestrain <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 108)
# Create a data frame
datatrain <- data.frame(Datetrain = datestrain, y_pred_enet_train1 = y_pred_enet_train1, y_train = y_train)
ggplot(datatrain, aes(x = Datetrain)) +
  geom_line(aes(y = y_pred_enet_train1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_train, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012-Mar 2024) Training dengan Metode Regresi Elastic Net (Transformasi Log)",
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

y_pred_all_enet1 <- matrix(c(y_pred_enet_train1, y_pred_enet_test1), 
                           nrow = length(y_pred_enet_train1) + length(y_pred_enet_test1), 
                           ncol = 1)
y_actual <- DataEksporImpor$`EKSPOR MIGAS`
datesall <- seq(from = as.Date("2012-01-01"), by = "month", length.out = 128)
# Create a data frame
dataall <- data.frame(Dateall = datesall, y_pred_all_enet1 = y_pred_all_enet1, y_actual = y_actual)
ggplot(dataall, aes(x = Dateall)) +
  geom_line(aes(y = y_pred_all_enet1, color = "Predicted", linetype = "Predicted"), size = 1) +
  geom_line(aes(y = y_actual, color = "Actual", linetype = "Actual"), size = 1) +
  labs(title = "Ekspor Migas (Jan 2012-Mar 2024) dengan Metode Regresi Elastic Net (Transformasi Log)",
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

MAPE(y_test,y_pred_enet_test1)
RMSE(y_test,y_pred_enet_test1)
rsquared(y_test,y_pred_enet_test1)
MAPE(y_train, y_pred_enet_train1)
RMSE(y_train, y_pred_enet_train1)
rsquared(y_train, y_pred_enet_train1)