#### Before running this script, you have to run the heidelberg samples classifier, as well as all the individual scripts for subsets (1200, 1000, 700 reads etc)


predictions_percentage_1200 <- predicted_1200
predictions_percentage_1000 <- predicted_1000
predictions_percentage_700 <- predicted_700
predictions_percentage_500 <- predicted_500
predictions_percentage_250 <- predicted_250
predictions_percentage_100 <- predicted_100

base_models <- c("rf_model_1200", "rf_model_1000", "rf_model_700", "rf_model_500", "rf_model_250", "rf_model_100")
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")
test_data_names <- c("test_data_1200", "test_data_1000", "test_data_700", "test_data_500", "test_data_250", "test_data_100")


# Load the required libraries
library(pROC)
library(ggplot2)



# Define the base model names - IMPORTANT: this part is created in the script where the RF classifiers are trained

base_models <- c("rf_model_1200", "rf_model_1000", "rf_model_700", "rf_model_500", "rf_model_250", "rf_model_100")


# Define the corresponding object names
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")

# Define the corresponding test data names
test_data_names <- c("test_data_1200", "test_data_1000", "test_data_700", "test_data_500", "test_data_250", "test_data_100")


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
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")

# Define the corresponding thresholds
thresholds <- c(1, 0.8, 0.6, 0.4, 0.2, 0.1)

# Perform the code for each object
for (i in seq_along(object_names)) {
  # Get the object using the object name
  predictions <- get(object_names[i])
  
  # Predict class and attach test class
  predictions$predict <- names(predictions)[1:2][max.col(predictions[, 1:2])]
  predictions$observed <- test_data$modtissue
  
  # Create a binary vector indicating whether the prediction was accurate or not
  predictions$accurate <- ifelse(predictions$predict == predictions$observed, 1, 0)
  
  # Assign the updated object back to the environment
  assign(object_names[i], predictions)
}




# Define the object names  
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")

# Create a list to store the AUC values
auc_values <- vector("list", length(object_names))

# Define the class names
class_names <- c("tumor", "normal")

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
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")


# Create a list to store the multiclass_roc objects
multiclass_roc_list <- vector("list", length(object_names))

# Perform the code for each object
for (i in seq_along(object_names)) {
  # Get the object using the object name
  predictions <- get(object_names[i])
  
  # Extract the class probabilities
  class_probs <- as.matrix(predictions[, c("tumor", "normal")])
  
  # Convert the observed tissue column to a factor with appropriate levels
  observed_tissue <- factor(predictions$observed, levels = colnames(class_probs))
  
  # Create the multiclass ROC object
  multiclass_roc <- multiclass.roc(observed_tissue, class_probs)
  
  # Store the multiclass ROC object in the list
  multiclass_roc_list[[i]] <- multiclass_roc
}


assign(paste0("multiclass_roc_percentage_", 1:6), multiclass_roc_list)



# Access the first multiclass_roc object
multiclass_roc_percentage_1200 <- multiclass_roc_list[[1]]

# Access the second multiclass_roc object
multiclass_roc_percentage_1000 <- multiclass_roc_list[[2]]

# Access the third multiclass_roc object
multiclass_roc_percentage_700 <- multiclass_roc_list[[3]]

# Access the fourth multiclass_roc object
multiclass_roc_percentage_500 <- multiclass_roc_list[[4]]

# Access the fifth multiclass_roc object
multiclass_roc_percentage_250 <- multiclass_roc_list[[5]]

# Access the sixth multiclass_roc object
multiclass_roc_percentage_100 <- multiclass_roc_list[[6]]






# Extract the ROC curve for predictions_percentage_5
roc_curve_1200 <- multiclass_roc_predictions_percentage_1200$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_1200 <- roc_curve_1200$specificities
sensitivities_1200 <- roc_curve_1200$sensitivities

# Extract the ROC curve for predictions_percentage_1000
roc_curve_1000 <- multiclass_roc_predictions_percentage_1000$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_1000 <- roc_curve_08$specificities
sensitivities_1000 <- roc_curve_08$sensitivities

# Extract the ROC curve for predictions_percentage_700
roc_curve_700 <- multiclass_roc_predictions_percentage_700$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_700 <- roc_curve_700$specificities
sensitivities_700 <- roc_curve_700$sensitivities

# Extract the ROC curve for predictions_percentage_500
roc_curve_500 <- multiclass_roc_predictions_percentage_500$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_500 <- roc_curve_500$specificities
sensitivities_500 <- roc_curve_500$sensitivities

# Extract the ROC curve for predictions_percentage_250
roc_curve_250 <- multiclass_roc_predictions_percentage_250$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_250 <- roc_curve_250$specificities
sensitivities_250 <- roc_curve_250$sensitivities

# Extract the ROC curve for predictions_percentage_100
roc_curve_100 <- multiclass_roc_predictions_percentage_100$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_100 <- roc_curve_100$specificities
sensitivities_100 <- roc_curve_100$sensitivities




# Create a data frame for plotting
roc_data_1200 <- data.frame(Specificity = specificities_1200, Sensitivity = sensitivities_1200)
roc_data_1000 <- data.frame(Specificity = specificities_1000, Sensitivity = sensitivities_1000)
roc_data_700 <- data.frame(Specificity = specificities_700, Sensitivity = sensitivities_700)
roc_data_500 <- data.frame(Specificity = specificities_500, Sensitivity = sensitivities_500)
roc_data_250 <- data.frame(Specificity = specificities_250, Sensitivity = sensitivities_250)
roc_data_100 <- data.frame(Specificity = specificities_100, Sensitivity = sensitivities_100)



library(ggplot2)

# Create a list of roc_data objects
roc_data_list <- list(
  roc_data_1200,
  roc_data_1000,
  roc_data_700,
  roc_data_500,
  roc_data_250,
  roc_data_100
)

# Create a vector of colors for each ROC curve
roc_colors <- c("red", "blue", "green", "purple", "orange", "yellow")

# Create a ggplot object with all ROC curves
p <- ggplot() +
  geom_line(data = roc_data_list[[1]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[1]) +
  geom_line(data = roc_data_list[[2]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[2]) +
  geom_line(data = roc_data_list[[3]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[3]) +
  geom_line(data = roc_data_list[[4]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[4]) +
  geom_line(data = roc_data_list[[5]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[5]) +
  geom_line(data = roc_data_list[[6]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[6]) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab("False Positive Rate (1 - Specificity)") +
  ylab("True Positive Rate (Sensitivity)") +
  ggtitle("ROC Curves for Different Classifiers") +
  theme_bw()

# Print the plot
print(p)

# Save the plot
print(p)



