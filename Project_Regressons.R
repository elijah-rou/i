install.packages("lubridate")
library(lubridate)
library(tidyverse)

df <- read.csv("data/raw/teaching_training_data.csv")
df_unique <- df[!rev(duplicated(rev(df$unid))),]


#1 Predict who is likely to be in work (in survey 1) so that they can intervene at ‘baseline’
#2 Predict who is likely to work for more than 6 months

df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)  



reg1 <- lm(low_one_fin ~ fin_situ_future + gender + numchildren, data = df)
summary(reg1)

df$low_one_fin <- ifelse(df$fin_situ_now == 1, 1, 0)
