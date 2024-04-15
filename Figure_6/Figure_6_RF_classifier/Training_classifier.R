setwd("/no_backup/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/heidelberg_lung_samples")

library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)
library(reshape)
library(reshape2)
library(randomForest)

##### Using split bams as input

##### Predicting the tissue


data_all <- as.data.frame(t(merged2))

data2 <- as.data.frame(data_all[1:20, ])
data1 <- as.data.frame(data_all[21:40, ])



data1$tissue <- rownames(data1)

data1$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data1))

data2$tissue <- rownames(data2)

data2$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data2))


##### data1 will be used for training and data2 for testing

library(caret)
set.seed(123)
train_data <- data1
test_data <- data2

train_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', train_data$tissue)
test_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', test_data$tissue)

colnames(train_data) <- paste0("mod", colnames(train_data))
colnames(test_data) <- paste0("mod", colnames(test_data))

train_data$modtissue <- as.factor(train_data$modtissue)
test_data$modtissue <- as.factor(test_data$modtissue)

colnames(train_data) <- gsub(":", "_", colnames(train_data))
colnames(test_data) <- gsub(":", "_", colnames(test_data))


### Train the classifier with only 1 iteration of training
rf_model <- randomForest(modtissue ~ ., data = train_data, ntree = 500, mtry = 3, na.action = na.omit)

### Train the classifier with 4 iterations of training

for (iter in 1:4){
  rf[[iter]] <- randomForest(modtissue ~ ., data = train_data, ntree = 500, mtry = 3, na.action = na.omit)
  
  # predict on test data
  predicted <- predict(rf[[iter]], newdata = test_data)
  
  # calculate accuracy
  acc[iter] <- mean(predicted == test_data$modtissue)
  
  # update training data for next iteration
  train_data <- rbind(train_data, test_data)
  train_data$modtissue <- as.factor(train_data$modtissue)
  
}



### Use the 1st iteration after retraining the model - this can be changed if needed

rf_model <- rf[[1]]

predicted <- predict(rf_model, newdata = test_data)
confusionMatrix(predicted, test_data$modtissue)

# Load ggplot2 package
library(ggplot2)

# Predict on the test data
test_pred <- predict(rf_model, test_data)

# Create confusion matrix
cm <- table(test_data$modtissue, test_pred)

# Convert to data frame and add row names
cm_df <- as.data.frame.matrix(cm)
cm_df$tissue <- row.names(cm_df)

# Convert to long format for ggplot2
cm_df_long <- reshape2::melt(cm_df, id.var = "tissue")

# Plot the confusion matrix as a heatmap

pdf("randomForest_classifier_heidelberg_lung_cancer_40_samples_10082023.pdf",height=8, width=12)
ggplot(cm_df_long, aes(x = tissue, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
