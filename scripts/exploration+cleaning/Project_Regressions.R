install.packages("lubridate")
install.packages("caret")
library(lubridate)
library(tidyverse)
library(caret)


df <- read.csv("data/raw/teaching_training_data.csv")


#1 Predict who is likely to be in work (in survey 1) so that they can intervene at ‘baseline’
#2 Predict who is likely to work for more than 6 months

# Only keep most recent
df <- df[!rev(duplicated(rev(df$unid))),]

# Create Financial Situation 
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)  

# Create Age
df <- df %>% mutate(age_at_survey = interval(dob, survey_date_month) / years(1)) %>% mutate(age = floor(age_at_survey))

# Model Gender, Age, peoplelive, anyhhincome, fin_situ_future...Cleaning Data
sapply(df, class)

df$gender <- ifelse(df$gender == "Male", 1,0)
df$anyhhincome <- ifelse(df$anyhhincome == "TRUE",1,0)
df$fin_situ_now <- as.factor(df$fin_situ_now)
df$working <- as.factor(df$working)
df_select <- df %>% 
  select(unid, gender, age, peoplelive, anyhhincome, working, fin_situ_now)

df_select <- na.omit(df_select)

# Caret 
str(df_select)
dummies_model <- dummyVars(working ~ . , data=df_select)

working <- df_select$working
df_select <- data.frame(predict(dummies_model, newdata = df_select))
df_select$working <- working

#
set.seed(1234)
trainRowNumbers <- createDataPartition(df_select$working, p=0.8, list=FALSE)
trainData <- df_select[trainRowNumbers,]
testData <- df_select[-trainRowNumbers,]

trControl <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)

model_rf <- train(working ~ ., data = df_select, method = 'rf', trControl = trControl)
model_nnet <- train(working ~ ., data = df_select, method = 'nnet', trControl = trControl)

summary(model_nnet)
model_nnet$results

table(df_select$work)
