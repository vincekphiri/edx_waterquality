#Clear memory and objects from workspace
rm(list = ls())
gc()

#Load Required libraries
if (!require(pacman)) {
  install.packages("pacman", repos = "https://cran.r-project.org")
  library(pacman)
}

if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(lubridate))
  install.packages("lubridate", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(knitr))
  install.packages("knitr", repos = "http://cran.us.r-project.org")
if (!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(VIM))
  install.packages("VIM", repos = "http://cran.us.r-project.org")
if (!require(corrplot))
  install.packages("corrplot", repos = "http://cran.us.r-project.org")
if (!require(cowplot))
  install.packages("cowplot", repos = "http://cran.us.r-project.org")
if (!require(randomForest))
  install.packages("randomForest", repos = "http://cran.us.r-project.org")
if (!require(rpart))
  install.packages("rpart", repos = "http://cran.us.r-project.org")



pacman::p_load(tidyverse, lubridate, caret, ggplot2, knitr, data.table, VIM, corrplot, cowplot, rpart, randomForest)

#Import the water quality dataset
water_quality <- read.csv('./data/water_potability.csv')

#Dataset description
str(water_quality)

summary(water_quality)

head(water_quality)

#Check for missing variables
sum(is.na(water_quality))

#plot missingness using the VIM package
missingness <- aggr(
  water_quality,
  numbers = TRUE,
  prop = TRUE,
  sortVars = TRUE,
  labels = names(water_quality),
  cex.axis = 0.7,
  cex.lab = 0.7
)

#Explatory analysis

#recode the values from 1 and 0 to Yes and No
water_quality <- water_quality %>% 
  mutate(Potability = factor(Potability, levels = c(1, 0), labels = c("Yes", "No")))

str(water_quality)

#Plot water potability distribution
ggplot(water_quality) +
  aes(x = Potability, fill = Potability) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#009990", "#E1FFBB") ) +
  labs(title = "Distribution of Water Potability") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, face = "bold"))


water_quality$Potability <- as.factor(water_quality$Potability)

phpplot <- ggplot(water_quality) +
  aes(x = ph, fill = Potability) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set2") +  # Use a Brewer palette
  labs(title = "ph distribution against potability", fill = "Potability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

Hardnessplot <- ggplot(water_quality) +
  aes(x = Hardness, fill = Potability) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set2") +  # Use a Brewer palette
  labs(title = "Hardness distribution against potability", fill = "Potability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

Solidsplot <- ggplot(water_quality) +
  aes(x = Solids, fill = Potability) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set2") +  # Use a Brewer palette
  labs(title = "Solids distribution against potability", fill = "Potability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

Chloraminesplot <- ggplot(water_quality) +
  aes(x = Chloramines, fill = Potability) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set2") +  # Use a Brewer palette
  labs(title = "Chloramines distribution against potability", fill = "Potability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

Sulfateplot <- ggplot(water_quality) +
  aes(x = Sulfate, fill = Potability) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set2") +  # Use a Brewer palette
  labs(title = "Sulfate distribution against potability", fill = "Potability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

Conductivityplot <- ggplot(water_quality) +
  aes(x = Conductivity, fill = Potability) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set2") +  # Use a Brewer palette
  labs(title = "Conductivity distribution against potability", fill = "Potability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

Organic_carbonplot <- ggplot(water_quality) +
  aes(x = Organic_carbon, fill = Potability) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set2") +  # Use a Brewer palette
  labs(title = "Organic_carbon distribution against potability", fill = "Potability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

Trihalomethanesplot <- ggplot(water_quality) +
  aes(x = Trihalomethanes, fill = Potability) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set2") +  # Use a Brewer palette
  labs(title = "Trihalomethanes distribution against potability", fill = "Potability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

Turbidityplot <- ggplot(water_quality) +
  aes(x = Turbidity, fill = Potability) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set2") +  # Use a Brewer palette
  labs(title = "Turbidity distribution against potability", fill = "Potability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#Display the plots
phpplot
Hardnessplot
Solidsplot
Chloraminesplot
Sulfateplot
Conductivityplot
Organic_carbonplot
Trihalomethanesplot
Turbidityplot

#Data variability and outliers for all variables
box1 <- ggplot(water_quality) +
  aes(x = "", y = ph, fill = Potability) +
  geom_boxplot() +
  labs(title = "ph Data Variability & Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

box2 <- ggplot(water_quality) +
  aes(x = "", y = Hardness, fill = Potability) +
  geom_boxplot() +
  labs(title = "Hardness Data Variability & Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

box3 <- ggplot(water_quality) +
  aes(x = "", y = Solids, fill = Potability) +
  geom_boxplot() +
  labs(title = "Solids Data Variability & Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

box4 <- ggplot(water_quality) +
  aes(x = "", y = Chloramines, fill = Potability) +
  geom_boxplot() +
  labs(title = "Chloramines Data Variability & Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

box5 <- ggplot(water_quality) +
  aes(x = "", y = Sulfate, fill = Potability) +
  geom_boxplot() +
  labs(title = "Sulfate Data Variability & Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

box6 <- ggplot(water_quality) +
  aes(x = "", y = Conductivity, fill = Potability) +
  geom_boxplot() +
  labs(title = "Conductivity Data Variability & Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

box7 <- ggplot(water_quality) +
  aes(x = "", y = Organic_carbon, fill = Potability) +
  geom_boxplot() +
  labs(title = "Organic_carbon Data Variability & Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

box8 <- ggplot(water_quality) +
  aes(x = "", y = Trihalomethanes, fill = Potability) +
  geom_boxplot() +
  labs(title = "Trihalomethanes Data Variability & Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

box9 <- ggplot(water_quality) +
  aes(x = "", y = Turbidity, fill = Potability) +
  geom_boxplot() +
  labs(title = "Turbidity Data Variability & Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#plot the charts
box1
box2
box3
box4
box5
box6
box7
box8
box9

#Impute using mean for all the variables
mean_value_Ph <- round(mean(water_quality$ph, na.rm = TRUE))
mean_value_Chloramines <- round(mean(water_quality$Chloramines, na.rm = TRUE))
mean_value_Conductivity <- round(mean(water_quality$Conductivity, na.rm = TRUE))
mean_value_Hardness <- round(mean(water_quality$Hardness, na.rm = TRUE))
mean_value_OrganicCarbon <- round(mean(water_quality$Organic_carbon, na.rm = TRUE))
median_value_Solids <- round(median(water_quality$Solids, na.rm = TRUE))
mean_value_Sulfate <- round(mean(water_quality$Sulfate, na.rm = TRUE))
mean_value_Trihalomethanes <- round(mean(water_quality$Trihalomethanes, na.rm = TRUE))
mean_value_Turbidity <- round(mean(water_quality$Turbidity, na.rm = TRUE))

water_quality <- water_quality %>%
  mutate(ph = ifelse(is.na(ph), mean_value_Ph, ph)) %>% 
  mutate(Chloramines = ifelse(is.na(Chloramines), mean_value_Chloramines, Chloramines)) %>% 
  mutate(Conductivity = ifelse(is.na(Conductivity), mean_value_Conductivity, Conductivity)) %>% 
  mutate(Hardness = ifelse(is.na(Hardness), mean_value_Hardness, Hardness)) %>% 
  mutate(Organic_carbon = ifelse(is.na(Organic_carbon), mean_value_OrganicCarbon, Organic_carbon)) %>% 
  mutate(Solids = ifelse(is.na(Solids), median_value_Solids, Solids)) %>% 
  mutate(Sulfate = ifelse(is.na(Sulfate), mean_value_Sulfate, Sulfate)) %>% 
  mutate(Trihalomethanes = ifelse(is.na(Trihalomethanes), mean_value_Trihalomethanes, Trihalomethanes)) %>% 
  mutate(Turbidity = ifelse(is.na(Turbidity), mean_value_Turbidity, Turbidity)) 

summary(water_quality)

#plot a Correlation matrix
COR_matrix <- water_quality
COR_matrix$Potability <- as.integer(COR_matrix$Potability)
str(COR_matrix)

COR_water_quality = cor(COR_matrix)

corrplot(COR_water_quality, method = 'color', order = 'alphabet')


#Data split 80/20 and create training and testing datasets
set.seed(123)
sample <- createDataPartition(water_quality$Potability, p = 0.8, list = FALSE)
train_data <- water_quality[sample, ]
test_data <- water_quality[-sample, ]

# Random Forest
train_control <- trainControl(method = "cv", number = 8)
rf_model <- train(Potability ~ ., data = train_data, method = "rf", trControl = train_control) #The model
rf_predict <- predict(rf_model, test_data) #Predicting the test data
test_data$Potability_pred <- rf_predict

#evaluate model
rf_confusionMatrix <- table(test_data$Potability, test_data$Potability_pred)
rf_confusionMatrix

rf_classification_accuracy <- sum(diag(rf_confusionMatrix)/sum(rf_confusionMatrix))
rf_classification_accuracy

#Elements of confusion matrix
rf_TN <- rf_confusionMatrix [1,1]
rf_FP <- rf_confusionMatrix [1,2]
rf_FN <- rf_confusionMatrix [2,1]
rf_TP <- rf_confusionMatrix [2,2]

#precision metric
rf_precision <- rf_TP/(rf_TP+rf_FP)
rf_precision

#recall metric
rf_recall <- rf_TP/(rf_TP+rf_FN)
rf_recall

#f1 score metric
rf_f1_score <- 2*(rf_precision * rf_recall)/(rf_precision + rf_recall)
rf_f1_score

#KNN Model
train_control2 <- trainControl(method = "cv", number = 10)
knn_model <- train(Potability ~ ., data = train_data, method = "knn", trControl = train_control2) #The model
knn_predict <- predict(knn_model, test_data) #Predicting the test data
test_data$Potability_pred2 <- knn_predict

#Evaluation
knn_classification_accuracy <- sum(diag(knn_confusionMatrix)/sum(knn_confusionMatrix))
knn_classification_accuracy

#Elements of confusion matrix
knn_TN <- knn_confusionMatrix [1,1]
knn_FP <- knn_confusionMatrix [1,2]
knn_FN <- knn_confusionMatrix [2,1]
knn_TP <- knn_confusionMatrix [2,2]

#precision metric
knn_precision <- knn_TP/(knn_TP+knn_FP)
knn_precision

#recall metric
knn_recall <- knn_TP/(knn_TP+knn_FN)
knn_recall

#f1 score metric
knn_f1_score <- 2*(knn_precision * knn_recall)/(knn_precision + knn_recall)
knn_f1_score



#########################################




