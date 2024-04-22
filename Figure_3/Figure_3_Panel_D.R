#### Panel D

## Files used in this script were made by randomly subsetting bam files to produce pseudoreplicates. These pseudoreplicates were then processed using EpiNano, and then used for training and testing a Random Forest classifier.

## The random subsetting was performed as following (changing the percentage of subset reads and the bam file):
## samtools view -s 123.0037 -b fast5---bc_1_s.bam > percentage_0.37/adult_brain_subsample_rep3_5_1.bam

## The first replicate was used for training the classifier; the objects 'merged_18S_28S_adult_brain_split_rep1_t', 'merged_18S_28S_adult_heart_split_rep1_t', 'merged_18S_28S_adult_liver_split_rep1_t' and 'merged_18S_28S_adult_testis_split_rep1_t' were made as explaind in the script for Figure 1.

## The second replicate was used for testing the classifier

## The third replicate was used for validating the classifier and for studying the coverage-performance correlation

## To find how the coverage affects the classifier performance, bam files of an independent biological replicate were subsampled to contain 1200, 1000, 800, 600, 400, 200 and 100 reads (eg. merged_18S_28S_adult_brain_rep3_percentage_5_t was computed on a bam file containing 1200 reads). 

library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)
library(reshape)
library(reshape2)
library(randomForest)

#### Reiterational classifier training and testing


data1 <- rbind(merged_18S_28S_adult_brain_split_rep1_t, merged_18S_28S_adult_heart_split_rep1_t, merged_18S_28S_adult_liver_split_rep1_t, merged_18S_28S_adult_testis_split_rep1_t)

data1$tissue <- rownames(data1)

data1$tissue <- sub('^([^_]+_[^_]+).*', '\\1', data1$tissue)

data2 <- rbind(merged_18S_28S_adult_brain_split_rep2_t, merged_18S_28S_adult_heart_split_rep2_t, merged_18S_28S_adult_liver_split_rep2_t, merged_18S_28S_adult_testis_split_rep2_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('^([^_]+_[^_]+).*', '\\1', data2$tissue)


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


### Use the 2nd iteration after retraining the model - this can be changed if needed

rf_model <- rf[[2]]

predicted <- predict(rf_model, newdata = test_data)
confusionMatrix(predicted, test_data$modtissue)





# First - classifier for 1200 reads


data2 <- rbind(merged_18S_28S_adult_brain_rep3_percentage_5_t, merged_18S_28S_adult_heart_rep3_percentage_5_t, merged_18S_28S_adult_liver_rep3_percentage_5_t, merged_18S_28S_adult_testis_rep3_percentage_5_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('^([^_]+_[^_]+).*', '\\1', data2$tissue)

##### data1 will be used for training and data2 for testing


test_data <- data2


colnames(test_data) <- paste0("mod", colnames(test_data))

test_data$modtissue <- as.factor(test_data$modtissue)

colnames(test_data) <- gsub(":", "_", colnames(test_data))


# Create a new separate test data object
test_data_percentage_5 <- test_data

# Make predictions using the modified test_data
predictions_percentage_5 <- predict(rf_model, newdata = test_data_percentage_5)

# Create confusion matrix
confusionMatrix(predictions_percentage_5, test_data_percentage_5$modtissue)


# Second - classifier for 1000 reads


data2 <- rbind(merged_18S_28S_adult_brain_rep3_percentage_3.7_t, merged_18S_28S_adult_heart_rep3_percentage_3.7_t, merged_18S_28S_adult_liver_rep3_percentage_3.7_t, merged_18S_28S_adult_testis_rep3_percentage_3.7_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('^([^_]+_[^_]+).*', '\\1', data2$tissue)

##### data1 will be used for training and data2 for testing


test_data <- data2


colnames(test_data) <- paste0("mod", colnames(test_data))

test_data$modtissue <- as.factor(test_data$modtissue)

colnames(test_data) <- gsub(":", "_", colnames(test_data))


# Create a new separate test data object
test_data_percentage_3.7 <- test_data

# Make predictions using the modified test_data
predictions_percentage_3.7 <- predict(rf_model, newdata = test_data_percentage_3.7)

# Create confusion matrix
confusionMatrix(predictions_percentage_3.7, test_data_percentage_3.7$modtissue)


# Third - classifier for 800 reads


data2 <- rbind(merged_18S_28S_adult_brain_rep3_percentage_3_t, merged_18S_28S_adult_heart_rep3_percentage_3_t, merged_18S_28S_adult_liver_rep3_percentage_3_t, merged_18S_28S_adult_testis_rep3_percentage_3_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('^([^_]+_[^_]+).*', '\\1', data2$tissue)

##### data1 will be used for training and data2 for testing


test_data <- data2


colnames(test_data) <- paste0("mod", colnames(test_data))

test_data$modtissue <- as.factor(test_data$modtissue)

colnames(test_data) <- gsub(":", "_", colnames(test_data))


# Create a new separate test data object
test_data_percentage_3 <- test_data

# Make predictions using the modified test_data
predictions_percentage_3 <- predict(rf_model, newdata = test_data_percentage_3)

# Create confusion matrix
confusionMatrix(predictions_percentage_3, test_data_percentage_3$modtissue)

# Fourth - classifier for 600 reads


data2 <- rbind(merged_18S_28S_adult_brain_rep3_percentage_2.3_t, merged_18S_28S_adult_heart_rep3_percentage_2.3_t, merged_18S_28S_adult_liver_rep3_percentage_2.3_t, merged_18S_28S_adult_testis_rep3_percentage_2.3_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('^([^_]+_[^_]+).*', '\\1', data2$tissue)

##### data1 will be used for training and data2 for testing


test_data <- data2


colnames(test_data) <- paste0("mod", colnames(test_data))

test_data$modtissue <- as.factor(test_data$modtissue)

colnames(test_data) <- gsub(":", "_", colnames(test_data))


# Create a new separate test data object
test_data_percentage_2.3 <- test_data

# Make predictions using the modified test_data
predictions_percentage_2.3 <- predict(rf_model, newdata = test_data_percentage_2.3)

# Create confusion matrix
confusionMatrix(predictions_percentage_2.3, test_data_percentage_2.3$modtissue)


# Sixth - classifier for 400 reads


data2 <- rbind(merged_18S_28S_adult_brain_rep3_percentage_1.5_t, merged_18S_28S_adult_heart_rep3_percentage_1.5_t, merged_18S_28S_adult_liver_rep3_percentage_1.5_t, merged_18S_28S_adult_testis_rep3_percentage_1.5_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('^([^_]+_[^_]+).*', '\\1', data2$tissue)

##### data1 will be used for training and data2 for testing


test_data <- data2


colnames(test_data) <- paste0("mod", colnames(test_data))

test_data$modtissue <- as.factor(test_data$modtissue)

colnames(test_data) <- gsub(":", "_", colnames(test_data))


# Create a new separate test data object
test_data_percentage_1.5 <- test_data

# Make predictions using the modified test_data
predictions_percentage_1.5 <- predict(rf_model, newdata = test_data_percentage_1.5)

# Create confusion matrix
confusionMatrix(predictions_percentage_1.5, test_data_percentage_1.5$modtissue)



# Sixth - classifier for 200 reads


data2 <- rbind(merged_18S_28S_adult_brain_rep3_percentage_0.74_t, merged_18S_28S_adult_heart_rep3_percentage_0.74_t, merged_18S_28S_adult_liver_rep3_percentage_0.74_t, merged_18S_28S_adult_testis_rep3_percentage_0.74_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('^([^_]+_[^_]+).*', '\\1', data2$tissue)

##### data1 will be used for training and data2 for testing


test_data <- data2


colnames(test_data) <- paste0("mod", colnames(test_data))

test_data$modtissue <- as.factor(test_data$modtissue)

colnames(test_data) <- gsub(":", "_", colnames(test_data))


# Create a new separate test data object
test_data_percentage_0.74 <- test_data

# Make predictions using the modified test_data
predictions_percentage_0.74 <- predict(rf_model, newdata = test_data_percentage_0.74)

# Create confusion matrix
confusionMatrix(predictions_percentage_0.74, test_data_percentage_0.74$modtissue)


# Seventh - classifier for 100 reads


# Assuming your data frames are stored in data_list

data2 <- rbind(merged_18S_28S_adult_brain_rep3_percentage_0.37_t, merged_18S_28S_adult_heart_rep3_percentage_0.37_t, merged_18S_28S_adult_liver_rep3_percentage_0.37_t, merged_18S_28S_adult_testis_rep3_percentage_0.37_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('^([^_]+_[^_]+).*', '\\1', data2$tissue)

##### data1 will be used for training and data2 for testing


test_data <- data2


colnames(test_data) <- paste0("mod", colnames(test_data))

test_data$modtissue <- as.factor(test_data$modtissue)

colnames(test_data) <- gsub(":", "_", colnames(test_data))


# Create a new separate test data object
test_data_percentage_0.37 <- test_data

# Make predictions using the modified test_data
predictions_percentage_0.37 <- predict(rf_model, newdata = test_data_percentage_0.37)

# Create confusion matrix
confusionMatrix(predictions_percentage_0.37, test_data_percentage_0.37$modtissue)


#### Plotting the ROC curves

# Load the required libraries
library(pROC)
library(ggplot2)



# Define the base model names - IMPORTANT: this part is created in the script where the RF classifiers are trained

base_models <- c("rf_model", "rf_model", "rf_model", "rf_model", "rf_model", "rf_model", "rf_model")


# Define the corresponding object names
object_names <- c("predictions_percentage_5", "predictions_percentage_3.7", "predictions_percentage_3",
                  "predictions_percentage_2.3", "predictions_percentage_1.5", "predictions_percentage_0.74", "predictions_percentage_0.37")

# Define the corresponding test data names
test_data_names <- c("test_data_percentage_5", "test_data_percentage_3.7", "test_data_percentage_3",
                     "test_data_percentage_2.3", "test_data_percentage_1.5", "test_data_percentage_0.74", "test_data_percentage_0.37")


# Create the objects using a loop
for (i in seq_along(base_models)) {
  # Get the model object using the base model name
  model_object <- get(base_models[i])
  
  # Get the test data object using the corresponding test data name
  test_data_object <- get(test_data_names[i])
  
  # Create the object using the predicted probabilities
  assign(object_names[i], as.data.frame(predict(model_object, test_data_object, type = "prob")))
}



# Define the object names
object_names <- c("predictions_percentage_5", "predictions_percentage_3.7", "predictions_percentage_3",
                  "predictions_percentage_2.3", "predictions_percentage_1.5", "predictions_percentage_0.74", "predictions_percentage_0.37")

# Define the corresponding thresholds
thresholds <- c(1, 0.8, 0.6, 0.4, 0.2, 0.1)

# Perform the code for each object
for (i in seq_along(object_names)) {
  # Get the object using the object name
  predictions <- get(object_names[i])
  
  # Predict class and attach test class
  predictions$predict <- names(predictions)[1:4][max.col(predictions[, 1:4])]
  predictions$observed <- test_data$modtissue
  
  # Create a binary vector indicating whether the prediction was accurate or not
  predictions$accurate <- ifelse(predictions$predict == predictions$observed, 1, 0)
  
  # Assign the updated object back to the environment
  assign(object_names[i], predictions)
}




# Define the object names  
object_names <- c("predictions_percentage_5", "predictions_percentage_3.7", "predictions_percentage_3",
                  "predictions_percentage_2.3", "predictions_percentage_1.5", "predictions_percentage_0.74", "predictions_percentage_0.37")

# Create a list to store the AUC values
auc_values <- vector("list", length(object_names))

# Define the class names
class_names <- c("adult_brain", "adult_heart", "adult_liver", "adult_testis")

# Initialize a list to store ROC curve objects
roc_curves_list <- vector("list", length(object_names))

# Perform the code for each object
for (i in seq_along(object_names)) {
  # Get the object using the object name
  predictions <- get(object_names[i])
  
  # Extract the class probabilities
  class_probs <- as.matrix(predictions[, class_names])
  
  # Initialize a list to store binary response vectors for each class
  binary_responses <- vector("list", length(class_names))
  
  # Create a binary response vector for each class
  for (j in seq_along(class_names)) {
    binary_response <- ifelse(predictions$observed == class_names[j], 1, 0)
    binary_responses[[j]] <- binary_response
  }
  
  # Combine the ROC curves for all classes using multiclass.roc
  multiclass_roc <- multiclass.roc(as.numeric(unlist(binary_responses)), as.numeric(as.vector(class_probs)))
  
  # Calculate AUC for the multiclass ROC curve
  auc_value <- multiclass_roc$auc[1]
  
  # Store the AUC value in the list
  auc_values[[i]] <- auc_value
  
  # Store the multiclass_roc object in the list with appropriate names
  assign(paste0("multiclass_roc_", object_names[i]), multiclass_roc)
}




# Function to convert multiclass ROC curve to a data.frame
convert_roc_to_dataframe <- function(roc_curve) {
  data.frame(FPR = roc_curve$specificities[[1]], TPR = roc_curve$sensitivities[[1]])
}

# Create a data.frame with combined ROC curves and AUC values
roc_data_list <- lapply(seq_along(roc_curves_list), function(i) {
  data <- convert_roc_to_dataframe(roc_curves_list[[i]])
  if (nrow(data) > 0) {
    data$Classifier <- object_names[i]
    data$AUC <- auc_values[[i]]
  }
  data
})

# Filter out any empty data.frames
roc_data_list <- Filter(function(data) nrow(data) > 0, roc_data_list)

# Combine the data.frames
roc_data <- do.call(rbind, roc_data_list)


# Print the AUC values
for (i in seq_along(object_names)) {
  cat("AUC for", object_names[i], ":", auc_values[[i]], "\n")
}



############ PLOTTING THE ROCs






# Define the object names
object_names <- c("predictions_percentage_5", "predictions_percentage_3.7", "predictions_percentage_3",
                  "predictions_percentage_2.3", "predictions_percentage_1.5", "predictions_percentage_0.74", "predictions_percentage_0.37")


# Create a list to store the multiclass_roc objects
multiclass_roc_list <- vector("list", length(object_names))

# Perform the code for each object
for (i in seq_along(object_names)) {
  # Get the object using the object name
  predictions <- get(object_names[i])
  
  # Extract the class probabilities
  class_probs <- as.matrix(predictions[, c("adult_brain", "adult_heart", "adult_liver", "adult_testis")])
  
  # Convert the observed tissue column to a factor with appropriate levels
  observed_tissue <- factor(predictions$observed, levels = colnames(class_probs))
  
  # Create the multiclass ROC object
  multiclass_roc <- multiclass.roc(observed_tissue, class_probs)
  
  # Store the multiclass ROC object in the list
  multiclass_roc_list[[i]] <- multiclass_roc
}


assign(paste0("multiclass_roc_percentage_", 1:7), multiclass_roc_list)



# Access the first multiclass_roc object
multiclass_roc_percentage_5 <- multiclass_roc_list[[1]]

# Access the second multiclass_roc object
multiclass_roc_percentage_3.7 <- multiclass_roc_list[[2]]

# Access the third multiclass_roc object
multiclass_roc_percentage_3 <- multiclass_roc_list[[3]]

# Access the fourth multiclass_roc object
multiclass_roc_percentage_2.3 <- multiclass_roc_list[[4]]

# Access the fifth multiclass_roc object
multiclass_roc_percentage_1.5 <- multiclass_roc_list[[5]]

# Access the sixth multiclass_roc object
multiclass_roc_percentage_0.74 <- multiclass_roc_list[[6]]

# Access the sixth multiclass_roc object
multiclass_roc_percentage_0.37 <- multiclass_roc_list[[7]]




# Extract the ROC curve for predictions_percentage_5
roc_curve_5 <- multiclass_roc_predictions_percentage_5$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_5 <- roc_curve_5$specificities
sensitivities_5 <- roc_curve_5$sensitivities

# Extract the ROC curve for predictions_percentage_3.7
roc_curve_3.7 <- multiclass_roc_predictions_percentage_3.7$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_3.7 <- roc_curve_08$specificities
sensitivities_3.7 <- roc_curve_08$sensitivities

# Extract the ROC curve for predictions_percentage_3
roc_curve_3 <- multiclass_roc_predictions_percentage_3$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_3 <- roc_curve_3$specificities
sensitivities_3 <- roc_curve_3$sensitivities

# Extract the ROC curve for predictions_percentage_2.3
roc_curve_2.3 <- multiclass_roc_predictions_percentage_2.3$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_2.3 <- roc_curve_2.3$specificities
sensitivities_2.3 <- roc_curve_2.3$sensitivities

# Extract the ROC curve for predictions_percentage_1.5
roc_curve_1.5 <- multiclass_roc_predictions_percentage_1.5$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_1.5 <- roc_curve_1.5$specificities
sensitivities_1.5 <- roc_curve_1.5$sensitivities

# Extract the ROC curve for predictions_percentage_0.74
roc_curve_0.74 <- multiclass_roc_predictions_percentage_0.74$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_0.74 <- roc_curve_0.74$specificities
sensitivities_0.74 <- roc_curve_0.74$sensitivities

# Extract the ROC curve for predictions_percentage_0.37
roc_curve_0.37 <- multiclass_roc_predictions_percentage_0.37$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_0.37 <- roc_curve_0.37$specificities
sensitivities_0.37 <- roc_curve_0.37$sensitivities




# Create a data frame for plotting
roc_data_5 <- data.frame(Specificity = specificities_5, Sensitivity = sensitivities_5)
roc_data_3.7 <- data.frame(Specificity = specificities_3.7, Sensitivity = sensitivities_3.7)
roc_data_3 <- data.frame(Specificity = specificities_3, Sensitivity = sensitivities_3)
roc_data_2.3 <- data.frame(Specificity = specificities_2.3, Sensitivity = sensitivities_2.3)
roc_data_1.5 <- data.frame(Specificity = specificities_1.5, Sensitivity = sensitivities_1.5)
roc_data_0.74 <- data.frame(Specificity = specificities_0.74, Sensitivity = sensitivities_0.74)
roc_data_0.37 <- data.frame(Specificity = specificities_0.37, Sensitivity = sensitivities_0.37)



library(ggplot2)

# Create a list of roc_data objects
roc_data_list <- list(
  roc_data_5,
  roc_data_3.7,
  roc_data_3,
  roc_data_2.3,
  roc_data_1.5,
  roc_data_0.74,
  roc_data_0.37
)

# Create a vector of colors for each ROC curve
roc_colors <- c("red", "blue", "green", "purple", "orange", "yellow", "pink")

# Create a ggplot object with all ROC curves
p <- ggplot() +
  geom_line(data = roc_data_list[[1]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[1]) +
  geom_line(data = roc_data_list[[2]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[2]) +
  geom_line(data = roc_data_list[[3]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[3]) +
  geom_line(data = roc_data_list[[4]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[4]) +
  geom_line(data = roc_data_list[[5]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[5]) +
  geom_line(data = roc_data_list[[6]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[6]) +
  geom_line(data = roc_data_list[[7]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[7]) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab("False Positive Rate (1 - Specificity)") +
  ylab("True Positive Rate (Sensitivity)") +
  ggtitle("ROC Curves for Different Classifiers") +
  theme_bw()

# Print the plot
print(p)

# Save the plot
pdf("ROC_curves_classifiers_coverage_on_independent_replicate.pdf", height = 8, width = 8)
print(p)
dev.off()




