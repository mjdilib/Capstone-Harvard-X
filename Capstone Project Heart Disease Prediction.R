###############
# Libraries and Setup
###############

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(officer)) install.packages("officer", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(fastAdaboost)) install.packages("fastAdaboost", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(wsrf)) install.packages("wsrf", repos = "http://cran.us.r-project.org")

# Import libraries

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(readr)
library(kableExtra)
library(RColorBrewer)
library(corrplot)
library(wsrf)
library(kernlab)
library(xgboost)

###############
# Download Data
###############

# Download file from UCI link
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
              "processed.cleveland.data")

# Read Table and apply comma separation
read.table("processed.cleveland.data", sep = ",")

# Load the dataset with named columns
variable_names <- c("Age", "Sex", "Chest_Pain_Type", "Blood_Pressure_AR", "Cholesterol", "Blood_Sugar_F", "ECG_AR", "Max_HR",
                    "Angina_Exercise", "Old_Peak", "Slope", "Number_Vessels", "Defect_Presence", "Heart_Disease")

# Load dataset with reviewed names
data_complete <- read.table("processed.cleveland.data", sep=",", col.names = variable_names)


# Quick Overview of Initial Data
summary(data_complete)


##############
# Data Preparation
##############

# Review names
# Binarization of target variable
# Variable Options are made more interpretable with more explicit labels

data_proc_1 <- data_complete %>% 
  mutate(Sex = if_else(Sex == 1, "M", "F"),
         Blood_Sugar_F = if_else(Blood_Sugar_F == 1, "Greater than 120", "Lesser or Equal to 120"),
         Angina_Exercise = if_else(Angina_Exercise == 1, "Yes" ,"No"),
         Chest_Pain_Type = if_else(Chest_Pain_Type == 1, "Typical Angina",
                                   if_else(Chest_Pain_Type == 2, "Atypical Angina", 
                                           if_else(Chest_Pain_Type == 3, "Non-Anginal", "Asymptomatic"))),
         ECG_AR = if_else(ECG_AR == 0, "Normal",
                          if_else(ECG_AR == 1, "Abnormal", "Probable or Definite")),
         Slope = as.factor(Slope),
         Number_Vessels = as.factor(Number_Vessels),
         Defect_Presence = as.factor(Defect_Presence),
         Heart_Disease = if_else(Heart_Disease == 0, "No", "Yes")) %>% 
  mutate_if(is.character, as.factor) %>% #convert character variables to factors
  dplyr::select(Heart_Disease, Sex, Blood_Sugar_F, Angina_Exercise, Chest_Pain_Type, ECG_AR, Slope, Number_Vessels, Defect_Presence, everything())

# Eliminate rows with '?' values
data_proc_2 <- data_proc_1 %>% filter_all(all_vars(.!="?"))

# Quick Overview of Initial Data
glimpse(data_proc_2)
summary(data_proc_2)



##############
# Data Visualization
##############

# Number of Cases of Heart Disease (Count of Target Variable)
ggplot(data = data_proc_2,
       aes(x= Heart_Disease, fill = Heart_Disease)) +
  geom_bar() +
  ggtitle("Number of Heart Disease Cases") +
  xlab("Presence of Heart Disease") +
  ylab("Number of Cases") +
  theme(legend.position = "top", legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))

# Number of Males and Female
ggplot(data = data_proc_2,
       aes(x= Sex, fill = Sex)) +
  geom_bar() +
  ggtitle("Number of Males vs Females") +
  xlab("Males and Females") +
  ylab("Number of Cases") +
  theme(legend.position = "top", legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))

# Number of Cases of Heart Disease by Sex
ggplot(data = data_proc_2,
       aes(x= Heart_Disease, fill = Sex)) +
  geom_bar() +
  ggtitle("Number of Heart Disease Cases highlighting Sex of Individual") +
  xlab("Presence of Heart Disease") +
  ylab("Number of Cases") +
  theme(legend.position = "top", legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))

# Distribution of Age / Sex
ggplot(data = data_proc_2,
       aes(x= Age, fill = Sex)) +
  geom_histogram(bins = 20) +
  ggtitle("Distribution of Age and Sex") +
  xlab("Age") +
  ylab("Number of Cases") +
  theme(legend.position = "top", legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))

# Density of Heart Disease considering Sex and Age
ggplot(data = data_proc_2,
       aes(x= Age, col = Sex, fill = Sex)) +
  geom_density(alpha = 0.4) +
  ggtitle("Density of Heart Disease Cases considering Sex and Age") +
  xlab("Age") +
  ylab("Density") +
  theme(legend.position = "top", legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))

# Density of Heart Disease and Heart Rate
ggplot(data = data_proc_2,
       aes(x= Max_HR, col = Heart_Disease, fill = Heart_Disease)) +
  geom_density(alpha = 0.4) +
  ggtitle("Density of Heart Disease considering Max Heart Rate") +
  xlab("Max Heart Rate") +
  ylab("Density") +
  theme(legend.position = "top", legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))

# Density of Heart Disease and Heart Rate
ggplot(data = data_proc_2,
       aes(x= Chest_Pain_Type, col = Heart_Disease, fill = Heart_Disease)) +
  geom_bar(alpha = 0.4) +
  ggtitle("Heart Disease Cases and Chest Pain Type") +
  xlab("Chest Pain Type") +
  ylab("Count") +
  theme(legend.position = "top", legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))

# Cholesterol by Age and Sex
data_proc_2 %>%
  ggplot(aes(x=Age,y=Cholesterol,color=Sex, size=Cholesterol))+
  geom_point(alpha=0.4)+
  ggtitle("Cholesterol Levels by Age and Sex") +
  xlab("Age") +
  ylab("Cholesterol")+
  theme(legend.position = "top", legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))

# Defect Presence
ggplot(data_proc_2, aes(x = Defect_Presence, col = Heart_Disease, fill = Heart_Disease))+
  geom_bar()+
  ggtitle("Presence of Heart Defect") +
  xlab("Heart Defect") +
  ylab("Count of Cases")+
  theme(legend.position = "top", legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))

#############
# Setup data for Modelling Applications
#############

# Train / Test Split via 80/20 split
set.seed(28081991, sample.kind = "Rounding")
index <- createDataPartition(y = data_proc_2$Heart_Disease, times = 1, p = 0.2, list = FALSE)
TrainingSet <- data_proc_2[-index,]  
TestingSet <- data_proc_2[index,]

#############
# Model Creation and Deployment
#############

#############
############# Model 1: KNN Model
############# 

# Train KNN Model
set.seed(28081991, sample.kind = "Rounding")
knn_tune <- data.frame(k = seq(1,30,1))
train_knn <- train(Heart_Disease ~ ., method = "knn",
                   data = TrainingSet,
                   tuneGrid = knn_tune)

# Visualize optimal K and train model accordingly
k_plot <- ggplot(train_knn, highlight = TRUE)
k_plot
optimal_k <- train_knn$bestTune[1,1]
knn_optimal_model <- knn3(Heart_Disease  ~ ., data = TrainingSet, k = optimal_k) 

# Predictions and Confusion Matrix
y_predictions_knn <- predict(knn_optimal_model, TestingSet, type = "class")
confusion_matrix_knn <- confusionMatrix(data = y_predictions_knn, 
                                        reference = TestingSet$Heart_Disease, positive = "Yes") 
print(confusion_matrix_knn)

# Create Metrics
knn_accuracy <- confusion_matrix_knn$overall["Accuracy"]
knn_balanced_accuracy <- confusion_matrix_knn$byClass["Balanced Accuracy"]
knn_sensitivity <- confusion_matrix_knn$byClass["Sensitivity"]
knn_specificity <- confusion_matrix_knn$byClass["Specificity"]

# Group Metrics in a dataframe that can then be use for other models as well
knn_results <- c(knn_accuracy, knn_balanced_accuracy, knn_sensitivity, knn_specificity)


#############
############# Model 2: AdaBoost Classification Trees
#############

# Train AdaBoost Model
set.seed(28081991, sample.kind = "Rounding")
train_ada <- train(Heart_Disease ~ ., method = "adaboost",
                   data = TrainingSet)

# Predictions and Confusion Matrix
y_predictions_ada <- predict(train_ada, TestingSet)
confusion_matrix_ada <- confusionMatrix(data = y_predictions_ada, 
                                        reference = TestingSet$Heart_Disease, positive = "Yes") 
print(confusion_matrix_ada)


#############
############# Model 3: XGBoost
#############

# Setup Parameters for Optimization
# set up the cross-validated hyper-parameter search

set.seed(28081991, sample.kind = "Rounding")

train_control <- trainControl(method = "cv", number = 5)

xgb_grid <- expand.grid(nrounds = 1000,
                        max_depth = c(2,5,10),
                        eta = c(0.01),
                        gamma = c(0.5,1.0),
                        colsample_bytree = c(0.5),
                        subsample = c(0.5, 0.6),
                        min_child_weight = seq(1))

# Train xgb model
train_xgb <- train(Heart_Disease ~ .,
                   data = TrainingSet,
                   method ="xgbTree",
                   tuneGrid = xgb_grid,
                   trControl = train_control)

# Predictions and Confusion Matrix
y_predictions_xgb <- predict(train_xgb, TestingSet)
confusion_matrix_xgb <- confusionMatrix(data = y_predictions_xgb, 
                                        reference = TestingSet$Heart_Disease, positive = "Yes") 
print(confusion_matrix_xgb)

#############
############# Model 4: Weighted Space Random Forest
#############

set.seed(28081991, sample.kind = "Rounding")

# Train Model
train_wsrf <- train(Heart_Disease ~ .,
                    data = TrainingSet,
                    method ="wsrf",
                    trControl = train_control)

# Predictions and Confusion Matrix
y_predictions_wsrf <- predict(train_wsrf, TestingSet)
confusion_matrix_wsrf <- confusionMatrix(data = y_predictions_wsrf, 
                                         reference = TestingSet$Heart_Disease, positive = "Yes") 
print(confusion_matrix_wsrf)

#############
############# Model 5: SVMR
#############

# Train Model
train_svmR <- train(Heart_Disease ~ .,
                    data = TrainingSet,
                    method ="svmRadial",
                    trControl = train_control)

# Predictions and Confusion Matrix
y_predictions_svmR <- predict(train_svmR, TestingSet)
confusion_matrix_svmR <- confusionMatrix(data = y_predictions_svmR, 
                                         reference = TestingSet$Heart_Disease, positive = "Yes") 
print(confusion_matrix_svmR)

#############
############# Conclusion: Creation of Metrics for comparison
#############


############# KNN

# Create Metrics
knn_accuracy <- confusion_matrix_knn$overall["Accuracy"]
knn_balanced_accuracy <- confusion_matrix_knn$byClass["Balanced Accuracy"]
knn_sensitivity <- confusion_matrix_knn$byClass["Sensitivity"]
knn_specificity <- confusion_matrix_knn$byClass["Specificity"]

# Group Metrics in a dataframe that can then be use for other models as well
knn_results <- c(knn_accuracy, knn_balanced_accuracy, knn_sensitivity, knn_specificity)


############# AdaBoost

# Create Metrics
ada_accuracy <- confusion_matrix_ada$overall["Accuracy"]
ada_balanced_accuracy <- confusion_matrix_ada$byClass["Balanced Accuracy"]
ada_sensitivity <- confusion_matrix_ada$byClass["Sensitivity"]
ada_specificity <- confusion_matrix_ada$byClass["Specificity"]

# Group Metrics in a dataframe and add to results dataframe
ada_results <- c(ada_accuracy, ada_balanced_accuracy, ada_sensitivity, ada_specificity)
model_results_2 <-  rbind(knn_results, ada_results)


############# XGBoost

# Create Metrics
xgb_accuracy <- confusion_matrix_xgb$overall["Accuracy"]
xgb_balanced_accuracy <- confusion_matrix_xgb$byClass["Balanced Accuracy"]
xgb_sensitivity <- confusion_matrix_xgb$byClass["Sensitivity"]
xgb_specificity <- confusion_matrix_xgb$byClass["Specificity"]

# Group Metrics in a dataframe and add to results dataframe
xgb_results <- c(xgb_accuracy, xgb_balanced_accuracy, xgb_sensitivity, xgb_specificity)
model_results_3 <-  rbind(model_results_2, xgb_results)


############# WSRF

# Create Metrics
wsrf_accuracy <- confusion_matrix_wsrf$overall["Accuracy"]
wsrf_balanced_accuracy <- confusion_matrix_wsrf$byClass["Balanced Accuracy"]
wsrf_sensitivity <- confusion_matrix_wsrf$byClass["Sensitivity"]
wsrf_specificity <- confusion_matrix_wsrf$byClass["Specificity"]

# Group Metrics in a dataframe and add to results dataframe
wsrf_results <- c(wsrf_accuracy, wsrf_balanced_accuracy, wsrf_sensitivity, wsrf_specificity)
model_results_4 <-  rbind(model_results_3, wsrf_results)


############# SVMR

# Create Metrics
svmR_accuracy <- confusion_matrix_svmR$overall["Accuracy"]
svmR_balanced_accuracy <- confusion_matrix_svmR$byClass["Balanced Accuracy"]
svmR_sensitivity <- confusion_matrix_svmR$byClass["Sensitivity"]
svmR_specificity <- confusion_matrix_svmR$byClass["Specificity"]

# Group Metrics in a dataframe and add to results dataframe
svmR_results <- c(svmR_accuracy, svmR_balanced_accuracy, svmR_sensitivity, svmR_specificity)
model_results_5 <-  rbind(model_results_4, svmR_results)



