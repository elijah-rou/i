# Import Libraries
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(tidyverse)) install.packages("dplyr")
library(dplyr)
if (!require(kknn))  install.packages("kknn")
library(kknn)
if (!require(Metrics))  install.packages("metrics")
library(Metrics)

# Read in the source data file
df <- readRDS(file="data/processed/clean_data.RDS")

# Select the relevant columns and morph into factors where needed
df <- select(df, -c(X, survey_date_month, survey_num, working, job_start_date, job_leave_date, financial_situation_now, financial_situation_5years, age, fin_situ_now, fin_situ_future, com_score, num_score, company_size, monthly_pay, length_of_employment, peoplelive_15plus, province, dob))
df <- df_2 %>% filter(!is.na(volunteer), !is.na(leadershiprole), !is.na(peoplelive), !is.na(anygrant), !is.na(anyhhincome), !is.na(givemoney_yes))
df <- mutate(df,
               loe_morethan6months = factor(loe_morethan6months),
               anygrant = factor(anygrant),
               anyhhincome = factor(anygrant),
               givemoney_yes = factor(anygrant),
               numchildren = as.numeric(numchildren),
               numearnincome = as.numeric(numearnincome))

# Impute numeric variables (e.g. age and test scores)
preProcess_missingdata_model <- preProcess(df, method="knnImpute")
df <- predict(preProcess_missingdata_model, newdata = df)

# Set the seed for consistency
set.seed(69420)

# Split into training and test data (70-30)
df_train_index <- df %>%
  select(unid) %>% 
  sample_frac(0.7)
df_train <- left_join(df_train_index, df)
df_test <- anti_join(df, df_train_index)

# Perform KNN algorithm to train the model
fit <- kknn(loe_morethan6months ~ age_at_survey + grit_score + opt_score + grit_score + numearnincome, df_train, df_test, k = 5, kernel = "rectangular", distance = 2)
predicted <- predict(fit_knn)
results <- table(predicted_knn, df_test$loe_morethan6months)
confusionMatrix(results_knn)