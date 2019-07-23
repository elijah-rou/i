install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)

df <- read.csv("teaching_training_data.csv")

# Calculate age
df <- df %>%
  mutate(age_at_survey = interval(dob,survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey))

# Financial Situation Change
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)

# Added in scores
df_cft <- read.csv("teaching_training_data_cft.csv")
df_com <- read.csv("teaching_training_data_com.csv")
df_grit <- read.csv("teaching_training_data_grit.csv")
df_num <- read.csv("teaching_training_data_num.csv")
df_opt <- read.csv("teaching_training_data_opt.csv")

df_cft <- df_cft %>% 
  select(unid, cft_score) %>% 
  distinct(unid, .keep_all = TRUE)

helper_function <- function(file_name) {
  file_name %>% 
    select(2:3) %>% 
    distinct(unid, .keep_all = TRUE)
}

df_com <- helper_function(df_com)
df_grit <- helper_function(df_grit)
df_num <- helper_function(df_num)
df_opt <- helper_function(df_opt)

df <- df %>%
  left_join(df_grit, by="unid")%>%
  left_join(df_opt, by="unid")%>%
  left_join(df_cft, by ="unid") %>% 
  left_join(df_com, by ="unid") %>% 
  left_join(df_num, by ="unid")

rm(df_cft,df_com,df_grit,df_num,df_opt)

# Convert TRUE/FALSE to 1/0 in "Working" column
df <- mutate(df, working = factor(ifelse(working==TRUE,1,0)))

# Use data from first survey only and use distinct individuals only [Model 1 runs on df]
df <- df %>% filter (survey_num == 1)
df <- df %>% distinct(unid,.keep_all=TRUE)

# Length of Employment
df_2 <- df
df_2 <- df_2 %>% mutate (job_start_date=ymd(job_start_date))
df_2 <- df_2 %>% mutate (job_leave_date=ymd(job_leave_date))
df_3 <- df_2 %>% filter (!is.na(job_start_date), !is.na(job_leave_date))
df_3 <- mutate(df_3, length_of_employment= job_leave_date - job_start_date)
df_3 <- df_3 %>% select(unid, length_of_employment)

df_2 <- left_join(df_2, df_3, by="unid")
rm(df_3)

# Remove individuals with missing job leave date values
df_2 <- df_2 %>% filter(working==0|(!is.na(job_start_date)&!is.na(job_leave_date)))

# Create Binary for 6 months of employment [Model 2 runs on df_2]
df_2 <- mutate (df_2, loe_morethan6months = factor(ifelse(length_of_employment>180,1,0)))
df_2$loe_morethan6months[is.na(df_2$loe_morethan6months)] <- 0
table(df_2$loe_morethan6months)

write.csv(df, 'data/processed/dataframe1.csv')

write.csv(df_2, 'data/processed/dataframe2.csv')
