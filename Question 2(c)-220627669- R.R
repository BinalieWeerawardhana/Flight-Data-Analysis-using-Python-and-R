
## (c)Logistic Regression Models for the probability of Diverted US Flights ##

## 2006 ##

# Importing the 2006 dataset for (c) that was merged and cleaned in python
dataframe_1 <- read.csv("C:/Users/DELL/Documents/python codes/dataframe_1.csv")

dataframe_1

# Splitting the dataset into features (X_2006) and target variable (y_2006)

# Convert Diverted to numeric
dataframe_1$Diverted <- as.numeric(as.character(dataframe_1$Diverted))

# Subset the data
X_2006 <- subset(dataframe_1, select = -Diverted)
y_2006 <- dataframe_1$Diverted

# Splitting the data into training and testing sets
install.packages("caret")
library(caret)

# Setting the seed for reproducibility
set.seed(42)
# Dividing the data into sets for training and testing 
indexes <- createDataPartition(y_2006, p = 0.8, list = FALSE)
X_2006_train <- X_2006[indexes, ]
X_2006_test <- X_2006[-indexes, ]
y_2006_train <- y_2006[indexes]
y_2006_test <- y_2006[-indexes]

# Viewing the shape of x_train and x_test
dim(X_2006_train)
dim(X_2006_test)

#Checking training set to see whether the 'Diverted' column has balanced data in terms of number of 0s and 1s.
table(y_2006_train)

# Using a bar plot to visualise the imbalance of the training set

class_counts <- table(y_2006_train)
colors <- c('lightgreen', 'blue')
barplot(class_counts, col = colors, names.arg = c('0', '1'),
        xlab = 'Diverted', ylab = 'Count',
        main = 'Class Distribution in Training Set')

train_set <- cbind(X_2006_train, y_2006_train)

# Dividing the dataset into majority and minority classes to perform class balancing
data_majority_2006 <- train_set[train_set$Diverted == 0, ]
data_minority_2006 <- train_set[train_set$Diverted == 1, ]

# Downsampling the majority class
set.seed(42)
desired_sample_size <- min(nrow(data_majority_2006), as.integer(5595567 * 0.0075))
data_majority2006_downsampled <- data_majority_2006[sample(nrow(data_majority_2006), 
                                                           size = desired_sample_size,
                                                           replace = FALSE), ]
# Upsampling the minority class
set.seed(42)
population_size <- nrow(data_minority_2006)
desired_sample_size <- min(population_size, as.integer(12904 * 20))  # Adjusted to ensure it's not larger than population size
data_minority_2006_upsampled <- data_minority_2006[sample(nrow(data_minority_2006), 
                                                          size = desired_sample_size,
                                                          replace = FALSE), ]

data_minority_2006_upsampled

# Set seed for reproducibility
set.seed(42)

# Calculate desired sample size
population_size <- nrow(data_minority_2006)
desired_sample_size <- min(population_size, as.integer(12904 * 20))

# Check if desired_sample_size is greater than 0
if (desired_sample_size > 0) {
  # Perform sampling
  data_minority_2006_upsampled <- data_minority_2006[sample(nrow(data_minority_2006), 
                                                            size = desired_sample_size,
                                                            replace = TRUE), ]
} else {
  # Handle case when desired_sample_size is 0 or negative
  print("Desired sample size is 0 or negative. Please check your calculations.")
}

# Set seed for reproducibility
set.seed(42)

# Calculate desired sample size
desired_sample_size <- max(nrow(data_minority_2006), 12904 * 20)

# Check if desired_sample_size is greater than 0
if (desired_sample_size > 0) {
  # Perform sampling
  data_minority_2006_upsampled <- data_minority_2006[sample(nrow(data_minority_2006), 
                                                            size = desired_sample_size,
                                                            replace = TRUE), ]
} else {
  # Handle case when desired_sample_size is 0 or negative
  print("Desired sample size is 0 or negative. Please check your calculations.")
}
data_minority_2006_upsampled


# Combining the sampled majority and minority classes to create a balanced dataset
train_set2006 <- rbind(data_majority2006_downsampled, data_minority2006_upsampled)

# Displaying new class count in the training set
table(train_set2006$Diverted)

train_set2006
names(train_set2006)
str(train_set2006)

#Defining the categorical columns 
cat_cols <- c('UniqueCarrier', 'Origin', 'Dest','state_origin','state_dest','city_origin','city_dest')

library(dplyr)
# Encoding the categorical variables into integer labels
for(col in cat_cols) {
  train_set2006[[col]] <- as.numeric(factor(train_set2006[[col]]))
}

#Selecting the categorical columns from the dataset
df2006_cat <- train_set2006[, cat_cols, drop = FALSE]

str(df2006_cat)

#Selecting the required numerical columns 
num_cols <- c('Diverted', 'Month', 'DayofMonth', 'DayOfWeek', 'CRSDepTime', 'CRSArrTime', 'DepDelay', 'CarrierDelay', 'WeatherDelay', 'NASDelay', 
              'SecurityDelay', 'LateAircraftDelay', 'TaxiOut', 'Distance','lat_origin', 'long_origin','lat_dest', 'long_dest')


df2006_num <- train_set2006 %>% select(num_cols)
df2006_num

# Combining the encoded categorical variables and numerical variables to create a new encoded dataframe
df2006_new <- cbind(df2006_num, df2006_cat)
df2006_new

library(corrplot)
corr <- cor(df2006_new)
par(mar=c(5,5,2,2))  
corrplot(corr, method="color", col="RdPu", addCoef.col = "black", tl.cex = 0.8)

table(train_set2006$Diverted)

# Checking the number of 0s and 1s in the 'Diverted' column of the new encoded dataset
table(df2006_new$Diverted)

#Making the new x_train and y_train
X1_2006 <- df2006_new[, -which(names(df2006_new) == 'Diverted')]
y1_2006 <- df2006_new$Diverted

# Splitting into new train and test data
set.seed(42)
trainIndex <- createDataPartition(y1_2006, p = 0.8, list = FALSE)
X1_2006_train <- X1_2006[trainIndex, ]
X1_2006_test <- X1_2006[-trainIndex, ]
y1_2006_train <- y1_2006[trainIndex]
y1_2006_test <- y1_2006[-trainIndex]


library(caret)
# Fitting the standard scaler on the encoded training data
preproc <- preProcess(X1_2006_train, method = c("range"))
X_train_scaled2006 <- predict(preproc, X1_2006_train)

# Fitting the standard scaler on the encoded testing data
X_test_scaled2006 <- predict(preproc, X1_2006_test)

# Fitting the logistic regression model to the training data
lr_model2006 <- glm(y1_2006_train ~ ., family = binomial(link = "logit"), data = cbind(Diverted = y1_2006_train, X_train_scaled2006))

# Using the logistic regression model to predict labels for the testing data 
y_pred2006 <- predict(lr_model2006, newdata = X_test_scaled2006, type = "response")

# Calculating the accuracy of the model on the testing data
accuracy <- mean(y1_2006_test == y_pred2006)
print(paste("Accuracy:", accuracy))

# Predicting probabilities on the standardized testing data
y_pred_prob_lr2006 <- predict(lr_model2006, newdata = X_test_scaled2006, type = "response")

# Calculating the false positive rate (fpr), true positive rate (tpr), and thresholds

install.packages("ROCR")
library(ROCR)

roc <- performance(pred, "tpr", "fpr")
tpr <- unlist(performance(pred, "tpr")@y.values)
fpr <- unlist(performance(pred, "fpr")@y.values)

install.packages("DMwR")
library(DMwR)

# Calculating the F1 score 
threshold <- 0.5
y_pred_lr_thresholded <- ifelse(y_pred_prob_lr2006 >= threshold, 1, 0)

f1 <- F_meas(y_pred_lr_thresholded, y1_2006_test)
print(paste("F1 Score:", f1))

# Calculating the area under the ROC curve
roc_auc <- auc(fpr, tpr)
print(roc_auc)

# Plotting the ROC curve
plot(fpr, tpr, type = "l", col = "blue", lwd = 2, main = "Receiver Operating Characteristic (ROC) Curve",
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0, 1, col = "gray", lty = 2)
legend("bottomright", legend = paste("AUC =", round(roc_auc, 2)), col = "blue", lwd = 2, bty = "n")

# Visualising the importance coefficients for the features of the 'Diverted' column

# Normalizing feature importance coefficients
min_importance <- min(feature_importance)
max_importance <- max(feature_importance)
normalized_importance <- (feature_importance - min_importance) / (max_importance - min_importance) * 2 - 1

#Dataframe to store features and their importance coefficents
feature_importance_df <- data.frame(Feature = colnames(X1_2006), Importance = normalized_importance)

# Plotting the feature importance coefficients in a bar chart
library(ggplot2)
ggplot(data = feature_importance_df, aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "hotpink") +
  labs(title = "Feature Importance Coefficients for flight Diversions - 2006",
       x = "Features",
       y = "Importance Coefficients") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



## 2007 ##

# Importing the 2007 dataset for (c) that was merged and cleaned in python
dataframe_1 <- read.csv("C:\Users\DELL\Documents\python codes\dataframe_2.csv")

# Splitting the dataset into features (X_2007) and target variable (y_2007)

X_2007 <- subset(dataframe_1, select = -Diverted)
y_2007 <- dataframe_1$Diverted

# Splitting the data into training and testing sets
library(caret)

set.seed(42)
# Dividing the data into sets for testing and training
indexes <- createDataPartition(y_2007, p = 0.8, list = FALSE)
X_2007_train <- X_2007[indexes, ]
X_2007_test <- X_2007[-indexes, ]
y_2007_train <- y_2007[indexes]
y_2007_test <- y_2007[-indexes]

# Viewing the shape of x_train and x_test
dim(X_2007_train)
dim(X_2007_test)

#Checking training set to see whether the 'Diverted' column has balanced data in terms of number of 0s and 1s.
table(y_2007_train)

# Using a bar plot to visualise the imbalance of the training set

class_counts <- table(y_2007_train)
colors <- c('lightgreen', 'blue')
barplot(class_counts, col = colors, names.arg = c('0', '1'),
        xlab = 'Diverted', ylab = 'Count',
        main = 'Class Distribution in Training Set')

train_set <- cbind(X_2007_train, y_2007_train)

# Dividing the dataset into majority and minority classes to perform class balancing
data_majority_2007 <- train_set[train_set$Diverted == 0, ]
data_minority_2007 <- train_set[train_set$Diverted == 1, ]

# Downsampling the majority class
set.seed(42)
desired_sample_size <- min(nrow(data_majority_2007), as.integer(5595567 * 0.0075))
data_majority2007_downsampled <- data_majority_2007[sample(nrow(data_majority_2007), 
                                                           size = desired_sample_size,
                                                           replace = FALSE), ]
# Upsampling the minority class
set.seed(42)
desired_sample_size <- min(nrow(data_minority_2007), as.integer(12904  * 20))
data_minority_2007_upsampled <- data_minority_2007[sample(nrow(data_minority_2007), 
                                                          size = desired_sample_size,
                                                          replace = FALSE), ]

# Combining the sampled majority and minority classes to create a balanced dataset
train_set2007 <- rbind(data_majority2007_downsampled, data_minority2007_upsampled)

# Displaying new class count in the training set
table(train_set2007$Diverted)

train_set2007
names(train_set2007)
str(train_set2007)

#Defining the categorical columns 
cat_cols <- c('UniqueCarrier', 'Origin', 'Dest','state_origin','state_dest','city_origin','city_dest')

library(dplyr)
# Encoding the categorical variables into integer labels
for(col in cat_cols) {
  train_set2007[[col]] <- as.numeric(factor(train_set2007[[col]]))
}

#Selecting the categorical columns from the dataset
df2007_cat <- train_set2007[, cat_cols, drop = FALSE]

str(df2007_cat)

#Selecting the required numerical columns 
num_cols <- c('Diverted', 'Month', 'DayofMonth', 'DayOfWeek', 'CRSDepTime', 'CRSArrTime', 'DepDelay', 'CarrierDelay', 'WeatherDelay', 'NASDelay', 
              'SecurityDelay', 'LateAircraftDelay', 'TaxiOut', 'Distance','lat_origin', 'long_origin','lat_dest', 'long_dest')

df2007_num <- train_set2007 %>% select(num_cols)
df2007_num

# Combining the encoded categorical variables and numerical variables to create a new encoded dataframe
df2007_new <- cbind(df2007_num, df2007_cat)
df2007_new

# Creating a heatmap to visualise the correlation/relationship between the columns in 2007
library(corrplot)
corr <- cor(df2007_new)
par(mar=c(5,5,2,2))  
corrplot(corr, method="color", col="RdPu", addCoef.col = "black", tl.cex = 0.8)

table(train_set2007$Diverted)

# Checking the number of 0s and 1s in the 'Diverted' column of the new encoded dataset
table(df2007_new$Diverted)

#Making the new x_train and y_train
X1_2007 <- df2007_new[, -which(names(df2007_new) == 'Diverted')]
y1_2007 <- df2007_new$Diverted

# Splitting into new train and test data
set.seed(42)
trainIndex <- createDataPartition(y1_2007, p = 0.8, list = FALSE)
X1_2007_train <- X1_2007[trainIndex, ]
X1_2007_test <- X1_2007[-trainIndex, ]
y1_2007_train <- y1_2007[trainIndex]
y1_2007_test <- y1_2007[-trainIndex]

library(caret)

# Fitting the standard scaler on the encoded training data
preproc <- preProcess(X1_2007_train, method = c("range"))
X_train_scaled2007 <- predict(preproc, X1_2007_train)

# Fitting the standard scaler on the encoded testing data
X_test_scaled2007 <- predict(preproc, X1_2007_test)

# Fitting the logistic regression model to the training data
lr_model2007 <- glm(y1_2007_train ~ ., family = binomial(link = "logit"), data = cbind(Diverted = y1_2007_train, X_train_scaled2007))

# Using the logistic regression model to predict labels for testing data
y_pred2007 <- predict(lr_model2007, newdata = X_test_scaled2007, type = "response")

# Calculating the accuracy of the model on the testing data
accuracy <- mean(y1_2007_test == y_pred2007)
print(paste("Accuracy:", accuracy))

# Predicting probabilities on the standardized test data
y_pred_prob_lr2007 <- predict(lr_model2007, newdata = X_test_scaled2007, type = "response")

# Calculating false positive rate (fpr), true positive rate (tpr), and thresholds

install.packages("ROCR")
library(ROCR)

roc <- performance(pred, "tpr", "fpr")
tpr <- unlist(performance(pred, "tpr")@y.values)
fpr <- unlist(performance(pred, "fpr")@y.values)

install.packages("DMwR")
library(DMwR)

# Calculating the F1 score 
threshold <- 0.5
y_pred_lr_thresholded <- ifelse(y_pred_prob_lr2007 >= threshold, 1, 0)

f1 <- F_meas(y_pred_lr_thresholded, y1_2007_test)
print(paste("F1 Score:", f1))

# Calculating the area under the ROC curve
roc_auc <- auc(fpr, tpr)
print(roc_auc)

# Plotting the ROC curve
plot(fpr, tpr, type = "l", col = "blue", lwd = 2, main = "Receiver Operating Characteristic (ROC) Curve",
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0, 1, col = "gray", lty = 2)
legend("bottomright", legend = paste("AUC =", round(roc_auc, 2)), col = "blue", lwd = 2, bty = "n")

# Visualising the importance coefficients for the features of the 'Diverted' column

# Normalizing feature importance coefficients
min_importance <- min(feature_importance)
max_importance <- max(feature_importance)
normalized_importance <- (feature_importance - min_importance) / (max_importance - min_importance) * 2 - 1

# Data frame to store features and their importance coefficients
feature_importance_df <- data.frame(Feature = colnames(X1_2007), Importance = normalized_importance)

# Plotting the feature importance coefficients in a bar chart
library(ggplot2)
ggplot(data = feature_importance_df, aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Feature Importance Coefficients for flight Diversions - 2007",
       x = "Features",
       y = "Importance Coefficients") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
