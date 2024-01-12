# Load the required packages
library(readxl)
library(glmnet)
library(DAAG)

# Read the Excel file into a data frame
health_data <- read_excel("./question_1/Health.xlsx")




# OUTLIER DETECTION
# Set a file name for the output plot (change as needed)
output_file_Before <- "./question_1/boxplotsBefore.png"

# Box plot for each feature
png(output_file_Before, width = 800, height = 600)  # Set the file type and dimensions
par(mfrow = c(2, 3))  # Set the layout to 2 rows and 3 columns for better visualization

# Box plot for X1
boxplot(health_data$X1, main = "X1", ylab = "Values")

# Box plot for X2
boxplot(health_data$X2, main = "X2", ylab = "Values")

# Box plot for X3
boxplot(health_data$X3, main = "X3", ylab = "Values")

# Box plot for X4
boxplot(health_data$X4, main = "X4", ylab = "Values")

# Box plot for X5
boxplot(health_data$X5, main = "X5", ylab = "Values")

dev.off()  # Close the graphics device

# Print a message indicating where the plot is saved
cat("Box plots saved to", output_file_Before, "\n")





# REMOVING OUTLIERS
health_data_clean <- health_data
health_data_clean <- health_data_clean[health_data_clean$X1 >= 6 & health_data_clean$X2 <= 200 & health_data_clean$X3 <= 1000 &
                           health_data_clean$X4 <= 12 & health_data_clean$X5 <= 200, ]


output_file_After <- "./question_1/boxplotsAfter.png"

# Box plot for each feature
png(output_file_After, width = 800, height = 600)  # Set the file type and dimensions
par(mfrow = c(2, 3))  # Set the layout to 2 rows and 3 columns for better visualization

# Box plot for X1
boxplot(health_data_clean$X1, main = "X1", ylab = "Values")

# Box plot for X2
boxplot(health_data_clean$X2, main = "X2", ylab = "Values")

# Box plot for X3
boxplot(health_data_clean$X3, main = "X3", ylab = "Values")

# Box plot for X4
boxplot(health_data_clean$X4, main = "X4", ylab = "Values")

# Box plot for X5
boxplot(health_data_clean$X5, main = "X5", ylab = "Values")

dev.off()  # Close the graphics device

# Print a message indicating where the plot is saved
cat("Box plots saved to", output_file_After, "\n")



# CORRELATION MATRIX TO FIND OUT HOW FEATURES ARE CORELATED TO EACH OTHER
cor_matrix <- cor(health_data_clean)
cat("\nCorrelation Matrix\n")
print(cor_matrix)



# SIMPLE REGRESSION BEFORE OUTLIER REMOVAL
# Perform linear regression with X1 as the target and X2 as a feature
lrmodel_simple <- lm(X1 ~ X2, data = health_data)

# Get the predicted values from the model
predicted_values_simple <- predict(lrmodel_simple, newdata = health_data)

# Calculate the mean squared loss
mse_simple <- mean((health_data$X1 - predicted_values_simple)^2)

# Display the mean squared loss
cat("\nMean Squared Loss for Simple Regression before Outlier Removal:", mse_simple, "\n")



# SIMPLE REGRESSION AFTER OUTLIER REMOVAL
# Perform linear regression with X1 as the target and X2 as a feature
lrmodel_simple <- lm(X1 ~ X2, data = health_data_clean)

# Get the predicted values from the model
predicted_values_simple <- predict(lrmodel_simple, newdata = health_data_clean)

# Calculate the mean squared loss
mse_simple <- mean((health_data_clean$X1 - predicted_values_simple)^2)

# Display the mean squared loss
cat("Mean Squared Loss for Simple Regression after Outlier Removal:", mse_simple, "\n")



# FEATURE SCALING
# STANDARDIZATION
# Function to standardize (Z-score normalize) a column
standardize_column <- function(column) {
  (column - mean(column)) / sd(column)
}

# Standardize columns X1 to X5
standardized_columns <- lapply(health_data_clean[, c("X1", "X2", "X3", "X4", "X5")], standardize_column)

# Combine the standardized columns into a new data frame
standardized_health_data <- as.data.frame(standardized_columns)




# FEATURE SCALING
# MIN MAX SCALING
min_max_normalize <- function(column) {
  (column - min(column)) / (max(column) - min(column))
}

# Normalize columns X1 to X5
normalized_columns <- lapply(health_data_clean[, c("X1", "X2", "X3", "X4", "X5")], min_max_normalize)

# Combine the normalized columns into a new data frame
normalized_health_data <- as.data.frame(normalized_columns)




# MULTIPLE REGRESSION BEFORE OUTLIER REMOVAL
# Perform linear regression with X1 as the target and other features as predictors
lrmodel_multiple <- lm(X1 ~ X2 + X3 + X4 + X5, data = health_data)

# Get the predicted values from the model
predicted_values_multiple <- predict(lrmodel_multiple, newdata = health_data)

# Calculate the mean squared loss
mse_multiple <- mean((health_data$X1 - predicted_values_multiple)^2)

# Display the mean squared loss
cat("\nMean Squared Loss for Multiple Regression before outlier removal:", mse_multiple, "\n")



# MULTIPLE REGRESSION AFTER OUTLIER REMOVAL
# Perform linear regression with X1 as the target and other features as predictors
lrmodel_multiple <- lm(X1 ~ X2 + X3 + X4 + X5, data = health_data_clean)

# Get the predicted values from the model
predicted_values_multiple <- predict(lrmodel_multiple, newdata = health_data_clean)

# Calculate the mean squared loss
mse_multiple <- mean((health_data_clean$X1 - predicted_values_multiple)^2)

# Display the mean squared loss
cat("Mean Squared Loss for Multiple Regression after outlier removal:", mse_multiple, "\n")




# MULTIPLE REGRESSION, RIDGE AND LASSO USING STANDARDIZATION FEATURE SCALING
# Perform linear regression with X1 as the target and other features as predictors
lrmodel_multiple <- lm(X1 ~ X2 + X3 + X4 + X5, data = standardized_health_data)

# Get the predicted values from the model
predicted_values_multiple <- predict(lrmodel_multiple, newdata = standardized_health_data)

# Calculate the mean squared loss
mse_multiple <- mean((standardized_health_data$X1 - predicted_values_multiple)^2)

# Display the mean squared loss
cat("\nMean Squared Loss for Multiple Regression using Standardization Scaling:", mse_multiple, "\n")

# Separate the predictors (X2, X3, X4, X5) and the target variable (X1)
X <- as.matrix(standardized_health_data[, c("X2", "X3", "X4", "X5")])
y <- standardized_health_data$X1

#RIDGE Regression
ridge_model <- cv.glmnet(X, y, alpha = 0)  # alpha = 0 for Ridge
ridge_mse <- min(ridge_model$cvm)  # Minimum mean squared error
cat("Ridge Mean Squared Error using Standardization Scaling:", ridge_mse, "\n")

#LASSO Regression
lasso_model <- cv.glmnet(X, y, alpha = 1)  # alpha = 1 for Lasso
lasso_mse <- min(lasso_model$cvm)  # Minimum mean squared error
cat("Lasso Mean Squared Error using Standardization Scaling:", lasso_mse, "\n")





# MULTIPLE REGRESSION, RIDGE AND LASSO USING MIN-MAX FEATURE SCALING
# Perform linear regression with X1 as the target and other features as predictors
lrmodel_multiple <- lm(X1 ~ X2 + X3 + X4 + X5, data = normalized_health_data)

# Get the predicted values from the model
predicted_values_multiple <- predict(lrmodel_multiple, newdata = normalized_health_data)

# Calculate the mean squared loss
mse_multiple <- mean((normalized_health_data$X1 - predicted_values_multiple)^2)

# Display the mean squared loss
cat("\nMean Squared Loss for Multiple Regression using Min-Max Scaling:", mse_multiple, "\n")

# Separate the predictors (X2, X3, X4, X5) and the target variable (X1)
X <- as.matrix(normalized_health_data[, c("X2", "X3", "X4", "X5")])
y <- normalized_health_data$X1

#RIDGE Regression
ridge_model <- cv.glmnet(X, y, alpha = 0)  # alpha = 0 for Ridge
ridge_mse <- min(ridge_model$cvm)  # Minimum mean squared error
cat("Ridge Mean Squared Error using Min-Max Scaling:", ridge_mse, "\n")

#LASSO Regression
lasso_model <- cv.glmnet(X, y, alpha = 1)  # alpha = 1 for Lasso
lasso_mse <- min(lasso_model$cvm)  # Minimum mean squared error
cat("Lasso Mean Squared Error using Min-Max Scaling:", lasso_mse, "\n")


# CROSS VALIDAITON
# Ridge Regression
ridge_model_loocv <- cv.glmnet(X, y, alpha = 0, nfolds = nrow(X))  # LOOCV
ridge_model_5fold <- cv.glmnet(X, y, alpha = 0, nfolds = 5)  # 5-fold cross-validation
ridge_model_10fold <- cv.glmnet(X, y, alpha = 0, nfolds = 10)  # 10-fold cross-validation

ridge_mse_loocv <- min(ridge_model_loocv$cvm)  # Minimum mean squared error for LOOCV
ridge_mse_5fold <- min(ridge_model_5fold$cvm)  # Minimum mean squared error for 5-fold
ridge_mse_10fold <- min(ridge_model_10fold$cvm)  # Minimum mean squared error for 10-fold

cat("\nRidge Mean Squared Error (LOOCV):", ridge_mse_loocv, "\n")
cat("Ridge Mean Squared Error (5-fold):", ridge_mse_5fold, "\n")
cat("Ridge Mean Squared Error (10-fold):", ridge_mse_10fold, "\n")

# Lasso Regression
lasso_model_loocv <- cv.glmnet(X, y, alpha = 1, nfolds = nrow(X))  # LOOCV
lasso_model_5fold <- cv.glmnet(X, y, alpha = 1, nfolds = 5)  # 5-fold cross-validation
lasso_model_10fold <- cv.glmnet(X, y, alpha = 1, nfolds = 10)  # 10-fold cross-validation

lasso_mse_loocv <- min(lasso_model_loocv$cvm)  # Minimum mean squared error for LOOCV
lasso_mse_5fold <- min(lasso_model_5fold$cvm)  # Minimum mean squared error for 5-fold
lasso_mse_10fold <- min(lasso_model_10fold$cvm)  # Minimum mean squared error for 10-fold

cat("\nLasso Mean Squared Error (LOOCV):", lasso_mse_loocv, "\n")
cat("Lasso Mean Squared Error (5-fold):", lasso_mse_5fold, "\n")
cat("Lasso Mean Squared Error (10-fold):", lasso_mse_10fold, "\n")
