# File Name: CLEANING.R
# Last Modified On: July 28, 2019
# Last Modified By: Galen Palowitch
# Description: A cleaning script for use with the Model.


# Load the necessary libraries.
library(caret)
library(skimr)
library(RANN)
library(tidyverse)
library(lubridate)


# Read in the data from the six relevant datasets.
df_base <- read.csv("data/raw/testing_training_data.csv")
df_cft <- read.csv("data/raw/testing_training_data_cft.csv")
df_com <- read.csv("data/raw/testing_training_data_com.csv")
df_grit <- read.csv("data/raw/testing_training_data_grit.csv")
df_num <- read.csv("data/raw/testing_training_data_num.csv")
df_opt <- read.csv("data/raw/testing_training_data_opt.csv")


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


# Join the six datasets together into one large dataset.
df_full <- left_join(df_base, df_cft, by ="unid")
df_full <- df_full %>% 
  left_join(df_com, by = "unid") %>%
  left_join(df_grit, by ="unid") %>% 
  left_join(df_num, by ="unid") %>% 
  left_join(df_opt, by ="unid")


# Method to check the number of columns or rows are NAs.
# colSums(is.na(df_full))
# rowSums(is.na(df_full))


# Check levels of certain variables to determine if need recoding.
# levels(df_full$numearnincome)


# Recode certain variables to numeric.
df_full <- df_full %>%
  mutate(peoplelive = as.numeric(as.character(recode(df_full$peoplelive, "15 or more" = "15", "0: I live alone" = "0", "More than 15" = "15")))) %>%
  mutate(peoplelive_15plus = as.numeric(as.character(recode(df_full$peoplelive_15plus, "More than 15" = "15")))) %>%
  mutate(numchildren = as.numeric(as.character(recode(df_full$numchildren, "4 or more" = "4", "4more" = "4")))) %>%
  mutate(financial_situation_now = as.numeric(as.character(recode(df_full$financial_situation_now, "1 (Worst possible financial situation)" = "1", "10 (Best possible financial situation)" = "10")))) %>%
  mutate(financial_situation_5years = as.numeric(as.character(recode(df_full$financial_situation_5years, "1 (Worst possible financial situation)" = "1", "10 (Best possible financial situation)" = "10")))) %>%
  mutate(numchildren = as.numeric(as.character(recode(df_full$numchildren, "4 or more" = "4", "4more" = "4")))) %>%
  mutate(numearnincome = as.numeric(as.character(recode(df_full$numearnincome, "15 or more" = "15", "0: I live alone" = "0"))))
  

# Convert binary variables to 0 and 1.
df_full <- df_full %>%
  mutate(working = factor(ifelse(working == TRUE, 1, 0))) %>%
  mutate(volunteer = factor(ifelse(volunteer == 'Yes', 1, 0))) %>%
  mutate(leadershiprole = factor(ifelse(leadershiprole == 'Yes', 1, 0))) %>%
  mutate(anygrant = factor(ifelse(anygrant == TRUE, 1, 0))) %>%
  mutate(anyhhincome = factor(ifelse(anyhhincome == TRUE, 1, 0))) %>%
  mutate(givemoney_yes = factor(ifelse(givemoney_yes == TRUE, 1, 0))) %>%
  mutate(gender = factor(ifelse(gender == 'Male', 1, 0)))


# Compute age(squared) and add to dataframe.
df_full <- df_full %>% 
  mutate(age_squared = floor(interval(dob, survey_date_month)/years(1)-(1/3)) ^ 2)


# Compute year and month of survey and add to dataframe.
df_full <- df_full %>%
  mutate(survey_year = year(ymd(survey_date_month)))

df_full <- df_full %>%
  mutate(survey_month = month(ymd(survey_date_month)))


# Remove unimportant columns.
df_full <- subset(df_full, select = -c(X, province, survey_date_month, survey_num, job_start_date, job_leave_date, company_size, monthly_pay, dob))


# Write the cleaned testing dataset to a CSV
### MUST BE MODIFIED TO GO IN CLEAN DATA FOLDER ###
write.csv(df_full, "C:.../clean/testing.csv", row.names=F)


