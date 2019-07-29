# File Name: MODEL.R
# Last Modified On: July 28, 2019
# Last Modified By: Galen Palowitch
# Description: A Machine Learning Model to predict the working status of South African citizens in the program.


# Load all necessary libraries.
library(tidyverse)
library(lubridate)
library(caret)
library(ClusterR)


### TRAINING ###

# Load cleaned training data.
df_pretraining <- read.csv("data/clean/training.csv")

# Split into test and training sets
set.seed(300)
index <- createDataPartition(df_training$working, p = 0.75, list=FALSE)
df_training <- df_pretraining[index,]
df_testing <- df_pretraining[-index,]

# STEP 1: RUN A K-MEANS CLUSTERING ALGORITHM ON TEST DATA TO MAKE CLUSTERS BASED ON DISTRIBUTION OF NAs

# Make a dataframe in which all NAs are represented by a 0 and all non-NAs are represented by a 1.
df_recoded <- df_training
df_recoded <- subset(df_recoded, select = -c(grit_score, survey_year, survey_month, working))
df_recoded <- df_recoded %>%
  mutate(gender = ifelse(is.na(gender),0,1)) %>%
  mutate(volunteer = ifelse(is.na(volunteer),0,1)) %>%
  mutate(leadershiprole = ifelse(is.na(leadershiprole),0,1)) %>%
  mutate(peoplelive = ifelse(is.na(peoplelive),0,1)) %>%
  mutate(peoplelive_15plus = ifelse(is.na(peoplelive_15plus),0,1)) %>%
  mutate(numchildren = ifelse(is.na(numchildren),0,1)) %>%
  mutate(numearnincome = ifelse(is.na(numearnincome),0,1)) %>%
  mutate(anygrant = ifelse(is.na(anygrant),0,1)) %>%
  mutate(anyhhincome = ifelse(is.na(anyhhincome),0,1)) %>%
  mutate(financial_situation_now = ifelse(is.na(financial_situation_now),0,1)) %>%
  mutate(financial_situation_5years = ifelse(is.na(financial_situation_5years),0,1)) %>%
  mutate(givemoney_yes = ifelse(is.na(givemoney_yes),0,1)) %>%
  mutate(age_squared = ifelse(is.na(age_squared),0,1)) %>%
  mutate(cft_score = ifelse(is.na(cft_score),0,1)) %>%
  mutate(com_score = ifelse(is.na(com_score),0,1)) %>%
  mutate(num_score = ifelse(is.na(num_score),0,1)) %>%
  mutate(opt_score = ifelse(is.na(opt_score),0,1))

# Create an elbow plot to help determine optimal number of clusters. 
k.max <- 15
data <- subset(df_recoded, select = -c(unid))
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# Do a K-Means clustering using ClusterR package instead of normal kmeans() function. This is because the
# package allows for an easy method to assign test data on a previously created clustering assignment. Use
# 4 clusters based on the elbow plot.
set.seed(300)
kmeangroups <- KMeans_rcpp(data, clusters = 4, num_init = 50, max_iters = 100, initializer = 'kmeans++')
df_recoded$cluster_num = kmeangroups$clusters

# Isolate the two relevant columns.
df_groups_train <- df_recoded %>%
  select(unid, cluster_num)


# STEP 2: SPLIT BY CLUSTER

# Separate by cluster assignment.
df_group1_train <- df_groups_train %>%
  filter(df_groups_train$cluster_num == 1) %>%
  left_join(df_training, by ="unid")

df_group2_train <- df_groups_train %>%
  filter(df_groups_train$cluster_num == 2) %>%
  left_join(df_training, by ="unid")

df_group3_train <- df_groups_train %>%
  filter(df_groups_train$cluster_num == 3) %>%
  left_join(df_training, by ="unid")

df_group4_train <- df_groups_train %>%
  filter(df_groups_train$cluster_num == 4) %>%
  left_join(df_training, by ="unid")


# STEP 3: REDUCE AND CLEAN DATA BASED ON CLUSTER

# Remove columns in each with large proportions of NAs.
# MODIFY DATA FOR GROUP 1 
# round((colSums(is.na(df_group1_train)) / nrow(df_group1_train)) * 100)
df_group1_train <- select(df_group1_train, -unid, -cluster_num, -cft_score, -numearnincome, -com_score, -num_score)

# MODIFY DATA FOR GROUP 2
# round((colSums(is.na(df_group2_train)) / nrow(df_group2_train)) * 100)
df_group2_train <- select(df_group2_train, -unid, -cluster_num, -volunteer, -leadershiprole, -peoplelive, -peoplelive_15plus, -numchildren, -numearnincome, -financial_situation_now, -financial_situation_5years, -givemoney_yes, -com_score, -num_score, -opt_score)

# MODIFY DATA FOR GROUP 3
# round((colSums(is.na(df_group3_train)) / nrow(df_group3_train)) * 100)
df_group3_train <- select(df_group3_train, -unid, -cluster_num, -volunteer, -leadershiprole, -peoplelive, -peoplelive_15plus, -numchildren, -numearnincome, -anygrant, -anyhhincome, -financial_situation_now, -financial_situation_5years, -givemoney_yes, -com_score, -num_score, -opt_score)

# MODIFY DATA FOR GROUP 4
# round((colSums(is.na(df_group4_train)) / nrow(df_group4_train)) * 100)
df_group4_train <- select(df_group4_train, -unid, -cluster_num, -peoplelive_15plus)


# STEP 4: TRAIN SEPARATE MODELS FOR EACH CLUSTER

# Construct the training control used for all models.
trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

# TRAIN MODEL FOR GROUP 1
preprocess1 <- preProcess(df_group1_train, method=c('medianImpute'))
df_group1_train <- predict(preprocess1, df_group1_train)

# Train using knn 
system.time(model_knn1 <- train(as.factor(working) ~ ., data=df_group1_train, method='knn'))


### Try xgboost ###
# system.time(model_xgboost <- train(as.factor(working) ~ ., data=df_group1_train, method='xgbTree'))
# predictedBoost <- predict(model_xgboost, df_group1_train)
# model_xgboost$results
# confusionMatrix(predictedBoost, as.factor(df_group1_train$working))

### Try rparts ###
# system.time(model_rpart1 <- train(as.factor(working) ~ ., data=df_group1_train, method='rpart', trControl = trControl))
# predicted <- predict(model_rpart1, df_group1_train)
# model_rpart1$results
# confusionMatrix(predicted, as.factor(df_group1_train$working))

# TRAIN MODEL FOR GROUP 2
preprocess2 <- preProcess(df_group2_train, method=c('medianImpute'))
df_group2_train <- predict(preprocess2, df_group2_train)

system.time(model_knn2 <- train(as.factor(working) ~ ., data=df_group2_train, method='knn', tuneLength= 5, trControl = trControl))

# TRAIN MODEL FOR GROUP 3
preprocess3 <- preProcess(df_group3_train, method=c('medianImpute'))
df_group3_train <- predict(preprocess3, df_group3_train)

system.time(model_knn3 <- train(as.factor(working) ~ ., data=df_group3_train, method='knn', tuneLength=5, trControl = trControl))

# TRAIN MODEL FOR GROUP 4
preprocess4 <- preProcess(df_group4_train, method=c('medianImpute'))
df_group4_train <- predict(preprocess4, df_group4_train)

system.time(model_knn4 <- train(as.factor(working) ~ ., data=df_group4_train, method='knn', tuneLength=5, trControl = trControl))





### TESTING ###

# Load cleaned testing data.
df_testing <- read.csv("data/clean/testing.csv") 


# STEP 1: ASSIGN OBSERVATIONS TO CLUSTERS

# Make a dataframe in which all NAs are represented by a 0 and all non-NAs are represented by a 1.
df_recoded_test <- df_testing
df_recoded_test <- subset(df_recoded_test, select = -c(grit_score, survey_year, survey_month, working))
df_recoded_test <- df_recoded_test %>%
  mutate(gender = ifelse(is.na(gender),0,1)) %>%
  mutate(volunteer = ifelse(is.na(volunteer),0,1)) %>%
  mutate(leadershiprole = ifelse(is.na(leadershiprole),0,1)) %>%
  mutate(peoplelive = ifelse(is.na(peoplelive),0,1)) %>%
  mutate(peoplelive_15plus = ifelse(is.na(peoplelive_15plus),0,1)) %>%
  mutate(numchildren = ifelse(is.na(numchildren),0,1)) %>%
  mutate(numearnincome = ifelse(is.na(numearnincome),0,1)) %>%
  mutate(anygrant = ifelse(is.na(anygrant),0,1)) %>%
  mutate(anyhhincome = ifelse(is.na(anyhhincome),0,1)) %>%
  mutate(financial_situation_now = ifelse(is.na(financial_situation_now),0,1)) %>%
  mutate(financial_situation_5years = ifelse(is.na(financial_situation_5years),0,1)) %>%
  mutate(givemoney_yes = ifelse(is.na(givemoney_yes),0,1)) %>%
  mutate(age_squared = ifelse(is.na(age_squared),0,1)) %>%
  mutate(cft_score = ifelse(is.na(cft_score),0,1)) %>%
  mutate(com_score = ifelse(is.na(com_score),0,1)) %>%
  mutate(num_score = ifelse(is.na(num_score),0,1)) %>%
  mutate(opt_score = ifelse(is.na(opt_score),0,1))

data_test <- subset(df_recoded_test, select = -c(unid))

# Use the "trained" k-means clustering to assign the test data to clusters.
test_assignments <- predict_KMeans(data_test, kmeangroups$centroids)

df_recoded_test$cluster_num <- test_assignments

df_groups_test <- df_recoded_test %>%
  select(unid, cluster_num)


# STEP 2: SPLIT BY CLUSTER

# Separate by cluster assignment.
df_group1_test <- df_groups_test %>%
  filter(df_groups_test$cluster_num == 1) %>%
  left_join(df_testing, by ="unid")

df_group2_test <- df_groups_test %>%
  filter(df_groups_test$cluster_num == 2) %>%
  left_join(df_testing, by ="unid")

df_group3_test <- df_groups_test %>%
  filter(df_groups_test$cluster_num == 3) %>%
  left_join(df_testing, by ="unid")

df_group4_test <- df_groups_test %>%
  filter(df_groups_test$cluster_num == 4) %>%
  left_join(df_testing, by ="unid")


# STEP 3: REDUCE AND CLEAN DATA BASED ON CLUSTER

# MODIFY DATA FOR GROUP 1
df_group1_test <- select(df_group1_test, -unid, -cluster_num, -cft_score, -numearnincome, -com_score, -num_score)

# MODIFY DATA FOR GROUP 2
df_group2_test <- select(df_group2_test, -unid, -cluster_num, -volunteer, -leadershiprole, -peoplelive, -peoplelive_15plus, -numchildren, -numearnincome, -financial_situation_now, -financial_situation_5years, -givemoney_yes, -com_score, -num_score, -opt_score)

# MODIFY DATA FOR GROUP 3
df_group3_test <- select(df_group3_test, -unid, -cluster_num, -volunteer, -leadershiprole, -peoplelive, -peoplelive_15plus, -numchildren, -numearnincome, -anygrant, -anyhhincome, -financial_situation_now, -financial_situation_5years, -givemoney_yes, -com_score, -num_score, -opt_score)

# MODIFY DATA FOR GROUP 4
df_group4_test <- select(df_group4_test, -unid, -cluster_num, -peoplelive_15plus)


# STEP 4: MAKE PREDICTIONS USING SEPARATE MODELS FOR EACH CLUSTER

# MAKE PREDICTIONS FOR GROUP 1
preprocess1test <- preProcess(df_group1_test, method=c('medianImpute'))
df_group1_test <- predict(preprocess1test, df_group1_test)
predknn1 <- predict(model_knn1, df_group1_test)
con1 <- confusionMatrix(predknn1, as.factor(df_group1_test$working))
mat1 <- con1$table

# MAKE PREDICTIONS FOR GROUP 2
preprocess2test <- preProcess(df_group2_test, method=c('medianImpute'))
df_group2_test <- predict(preprocess2test, df_group2_test)
predknn2 <- predict(model_knn2, df_group2_test)
con2 <- confusionMatrix(predknn2, as.factor(df_group2_test$working))
mat2 <- con2$table

# MAKE PREDICTIONS FOR GROUP 3
preprocess3test <- preProcess(df_group3_test, method=c('medianImpute'))
df_group3_test <- predict(preprocess3test, df_group3_test)
predknn3 <- predict(model_knn3, df_group3_test)
con3 <- confusionMatrix(predknn3, as.factor(df_group3_test$working))
mat3 <- con3$table
    
# MAKE PREDICTIONS FOR GROUP 4
preprocess4test <- preProcess(df_group4_test, method=c('medianImpute'))
df_group4_test <- predict(preprocess4test, df_group4_test)
predknn4 <- predict(model_knn4, df_group4_test)
con4 <- confusionMatrix(predknn4, as.factor(df_group4_test$working))
mat4 <- con4$table


# STEP 5: COMBINE PREDICTIONS TOGETHER

# Combine the confusion matrices
conFinal <- mat1 + mat2 + mat3 + mat4
conFinal











