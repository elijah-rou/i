# Required libraries
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(tidyverse)) install.packages("dplyr")
library(dplyr)
if(!require(caret)) install.packages("caret")
library(caret)
if(!require(skimr)) install.packages("skimr")
library(skimr)
if(!require(RANN)) install.packages("RANN")
library(RANN)

# Read in the processed dataframe
df <- read.csv("data/processed/dataframe1.csv")

# Select only relevant data
df <- select(df, -c(X.1, X, survey_date_month, survey_num, job_start_date, job_leave_date, financial_situation_now, financial_situation_5years, age, fin_situ_now, fin_situ_future, com_score, num_score, company_size, monthly_pay, peoplelive_15plus, province, dob)) %>%
    filter(!is.na(volunteer), !is.na(leadershiprole), !is.na(peoplelive), !is.na(anygrant), !is.na(anyhhincome), !is.na(givemoney_yes)) %>%
    mutate(anygrant = factor(anygrant),
               anyhhincome = factor(anyhhincome),
               givemoney_yes = factor(givemoney_yes),
               working = factor(working),
               gender = factor(gender),
               numchildren = as.numeric(numchildren),
               numearnincome = as.numeric(numearnincome))

# Impute numeric variables (e.g. age and test scores)
preProcess_missingdata_model <- preProcess(df, method="knnImpute")
df <- predict(preProcess_missingdata_model, newdata = df)

# Split into training and test data
df_train_index <- df %>%
  select(unid) %>% 
  sample_frac(0.7)
df_train <- left_join(df_train_index, df)
df_test <- anti_join(df, df_train_index)

######################
# Logistic Regression
#####################
logistic <- glm(working ~ gender + age_at_survey + opt_score + grit_score, data=df_train, family="binomial")
predicted_log <- predict(logistic,df_test, type="response")
predicted_log <- ifelse(predicted_log>0.5,1,0)
results_lr <- table(predicted_log, df_test$working)
confusionMatrix(results_lr)

######################
# SVM
#####################
if (!require(e1071)) install.packages("e1071")
library(e1071)
fit <- svm(working ~ gender + age_at_survey + fin_situ_change, data=df_train)
predicted_svm <- predict(fit, df_test)
results_SVM <- table(predicted_svm, df_test$working)
confusionMatrix(results_SVM)

#####################
# Naive Bayes
#####################
fit_nb <- naiveBayes(working ~ gender + age_at_survey + opt_score + grit_score, data=df_train)
predicted_nb <- predict(fit_nb, df_test)
results_nb <- table(predicted_nb, df_test$working)
confusionMatrix(results_nb)

#####################
# KNN (useful)
#####################
if (!require(kknn))  install.packages("kknn")
library(kknn)
library(Metrics)
fit_knn <- kknn(working ~ age_at_survey + grit_score + opt_score + grit_score + numearnincome, df_train, df_test, k = 5, kernel = "rectangular", distance = 2)
predicted_knn <- predict(fit_knn)
results_knn <- table(predicted_knn, df_test$working)
confusionMatrix(results_knn)

######################
# Random Forest (useful)
######################
if (!require(randomForest)) install.packages("randomForest")
library(randomForest)
fit_rf <- randomForest(working ~ gender + age_at_survey + opt_score + cft_score + grit_score, df_train, ntree=10000)
predicted_rf <- predict(fit_rf,df_test)
results_rf <- table(predicted_rf, df_test$working)
confusionMatrix(results_rf)

######################
# GBM
######################
if (!require(gbm)) install.packages("gbm")
fitControl <- trainControl(method = "repeatedcv", number=4, repeats=4)
fit_gbm <- train(working ~ gender + age_at_survey, data=df_train, method="gbm", trControl=fitControl, verbose=FALSE)
predicted_gbm = predict(fit_gbm, df_test, type="prob")[,2]
predicted_gbm <- ifelse(predicted_gbm>0.5,1,0)
results_gbm <- table(predicted_gbm, df_test$working)
confusionMatrix(results_gbm)