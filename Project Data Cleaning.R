
# Import the necessary libraries.
library(caret)
library(skimr)
library(RANN)
library(tidyverse)
library(lubridate)


# Read in the data from the six datasets.
df_base <- read.csv("data/raw/teaching_training_data.csv")
df_cft <- read.csv("data/raw/teaching_training_data_cft.csv")
df_com <- read.csv("data/raw/teaching_training_data_com.csv")
df_grit <- read.csv("data/raw/teaching_training_data_grit.csv")
df_num <- read.csv("data/raw/teaching_training_data_num.csv")
df_opt <- read.csv("data/raw/teaching_training_data_opt.csv")


# Define a helper function for removing duplicates in the datasets.
helper_function <- function(file_name) {
  file_name %>% 
    select(2:3) %>% 
    distinct(unid, .keep_all = TRUE)
}


# Apply the helper function to the five datasets.
df_cft <- helper_function(df_cft)
df_com <- helper_function(df_com)
df_grit <- helper_function(df_grit)
df_num <- helper_function(df_num)
df_opt <- helper_function(df_opt)


# Remove duplicates on the base data set.
df_base <- df_base %>% 
  distinct(unid, .keep_all = TRUE)


# Join the six datasets together.
df_assess <- left_join(df_base, df_cft, by ="unid")
df_assess <- df_assess %>% 
  left_join(df_com, by = "unid") %>%
  left_join(df_grit, by ="unid") %>% 
  left_join(df_num, by ="unid") %>% 
  left_join(df_opt, by ="unid")


# Method to check the number of columns or rows are NAs.
colSums(is.na(df_assess))
rowSums(is.na(df_assess))


# Convert binary variables to 0 and 1.
df_assess <- df_assess %>%
  mutate(working = factor(ifelse(working == TRUE, 1, 0))) %>%
  mutate(volunteer = factor(ifelse(volunteer == 'Yes', 1, 0))) %>%
  mutate(leadershiprole = factor(ifelse(leadershiprole == 'Yes', 1, 0))) %>%
  mutate(anygrant = factor(ifelse(anygrant == TRUE, 1, 0))) %>%
  mutate(anyhhincome = factor(ifelse(anyhhincome == TRUE, 1, 0))) %>%
  mutate(givemoney_yes = factor(ifelse(givemoney_yes == TRUE, 1, 0))) %>%
  mutate(gender = factor(ifelse(gender == 'Male', 1, 0)))


# Compute age(squared) and add to dataframe.
df_assess <- df_assess %>% 
  mutate(age_squared = floor(interval(dob, survey_date_month)/years(1)-(1/3)) ^ 2)


# Compute year and month of survey and add to dataframe.
df_assess <- df_assess %>%
  mutate(survey_year = year(ymd(survey_date_month)))

df_assess <- df_assess %>%
  mutate(survey_month = month(ymd(survey_date_month)))


# Convert 


# Remove unimportant columns.
df_assess <- subset(df_assess, select = -c(X, unid, survey_date_month, survey_num, job_start_date, job_leave_date, company_size, monthly_pay, dob))


# Select rows with a province listed.
df_provinces <- df_assess %>%
  filter(!is.na(province))

# Select rows with NO province listed.
df_no_provinces <- df_assess %>%
  filter(is.na(province))

# Remove province column from df_no_provinces.
df_no_provinces <- subset(df_no_provinces, select = -c(province))




# Preprocess the data, including imputing all the missing values.
preprocess <- preProcess(df_assess[c('cft_score', 'com_score','grit_score', 'num_score', 'opt_score')], method=c('center', 'scale', 'knnImpute'))
df_assess[c('cft_score', 'com_score','grit_score', 'num_score', 'opt_score')] <- predict(preprocess, newdata = df_assess[c('cft_score', 'com_score','grit_score', 'num_score', 'opt_score')])


# Construct the training control.
trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)


# Train the data using random partitions model and the training control.
model_rpart <- train(working ~ ., data=df_assess, method='rpart', trControl = trControl)


# Train the data using ADABoost.
model_adaboost <- train(working ~ ., data=df_assess, method='adaboost')


# Show 
model_rpart$results

model_adaboost$results
