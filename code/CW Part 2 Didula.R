
# Load necessary libraries
library(readxl)
library(neuralnet)
library(caret)
library(dplyr)
library(ggplot2) 

# Load the dataset
exchange_data <- read_excel("ExchangeUSD (2).xlsx")

# Extract the "USD/EUR" column from exchange_data
exchange_rate <- exchange_data %>% pull(`USD/EUR`)

# Split dataset into training and testing sets
train_data <- exchange_rate[1:400]
test_data <- exchange_rate[401:length(exchange_rate)]


# Define Input Variables for MLP Models (Autoregressive Approach)
create_input <- function(data, lag){
  if (!is.vector(data)) {
    stop("Input data must be a vector.")
  }
  lagged_data <- embed(data, lag + 1)
  input <- lagged_data[, -1]
  output <- lagged_data[, 1]
  return(list(input = input, output = output))
}

# Experiment with four input vectors
lag_values <- c(1, 4, 7, 10)  # Choose lag values
input_vectors <- lapply(lag_values, function(lag) create_input(as.vector(train_data), lag))

head (input_vectors)

# Initialize an empty list to store models
models <- list()

# Initialize lists to store training data and predicted values
train_actual <- list()
train_predicted <- list()

# Train MLP Models using neuralnet and store them in a list
for (lag_index in seq_along(input_vectors)) {
  lag_input <- input_vectors[[lag_index]]
  lag <- lag_values[lag_index]
  
  model_sizes <- c(5, 10, 15)
  for (size in model_sizes) {
    # Combine lagged input variables with output variable
    train_data <- data.frame(lag_input$input, output = lag_input$output)
    
    # Define formula
    formula <- as.formula(paste("output ~ .", collapse = "+"))
    
    # Train neural network
    model <- neuralnet(formula, data = train_data, hidden = c(5,10,15), linear.output = TRUE)
    plot(model)
    
    # Store model
    model_name <- paste("lag_", lag, "_nodes_", size, sep = "")
    models[[model_name]] <- model
    
    # Store training data and predicted values
    train_actual[[model_name]] <- train_data$output
    train_predicted[[model_name]] <- predict(model, train_data)
  }
}
# Plot all neural network models
plot_models <- function(models) {
  par(mfrow = c(length(models), 1))  # Set up multiple plots
  
  for (model_name in names(models)) {
    plot(models[[model_name]])
  }
}

# Call the function to plot all models
plot_models(models)






# Define a function to calculate evaluation metrics
calculate_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((predicted - actual)^2))
  mae <- mean(abs(predicted - actual))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  smape <- mean(200 * abs(predicted - actual) / (abs(actual) + abs(predicted)))
  return(list(RMSE = rmse, MAE = mae, MAPE = mape, sMAPE = smape))
}

# Initialize a list to store evaluation metrics for each model
evaluation_metrics <- list()

# Loop through each model
for (model_name in names(models)) {
  # Extract actual and predicted values for the current model
  actual_values <- train_actual[[model_name]]
  predicted_values <- train_predicted[[model_name]]
  
  # Calculate evaluation metrics for the current model
  metrics <- calculate_metrics(actual_values, predicted_values)
  
  # Store the evaluation metrics for the current model
  evaluation_metrics[[model_name]] <- metrics
}

# Print the evaluation metrics for each model
for (model_name in names(evaluation_metrics)) {
  cat("Model:", model_name, "\n")
  cat("RMSE:", evaluation_metrics[[model_name]]$RMSE, "\n")
  cat("MAE:", evaluation_metrics[[model_name]]$MAE, "\n")
  cat("MAPE:", evaluation_metrics[[model_name]]$MAPE, "\n")
  cat("sMAPE:", evaluation_metrics[[model_name]]$sMAPE, "\n\n")
}





# Print the performance table
# Initialize lists to store evaluation metrics
rmse_list <- c()
mae_list <- c()
mape_list <- c()
smape_list <- c()

# Loop through each model's evaluation metrics
for (model_name in names(evaluation_metrics)) {
  # Extract evaluation metrics for the current model
  metrics <- evaluation_metrics[[model_name]]
  
  # Append metrics to respective lists
  rmse_list <- c(rmse_list, metrics$RMSE)
  mae_list <- c(mae_list, metrics$MAE)
  mape_list <- c(mape_list, metrics$MAPE)
  smape_list <- c(smape_list, metrics$sMAPE)
}

# Create performance table
performance_table <- data.frame(
  Model = names(evaluation_metrics),
  RMSE = rmse_list,
  MAE = mae_list,
  MAPE = mape_list,
  sMAPE = smape_list
)

# Print performance table
print(performance_table)


# Find the row index of the model with the lowest RMSE
best_model_index <- which.min(performance_table$RMSE)

# Extract the best model information
best_model <- performance_table[best_model_index, ]

# Print the best model
print("Best Model:")
print(best_model)






# Actual VS predicted plot
dev.off()  # Close all open graphics devices
library(ggplot2)  # Re-import ggplot2 if needed


# Prepare the test data input using the lag value for the best model
best_lag <- 1  # The lag used in "lag_1_nodes_10" is 1, based on your naming convention
test_input_data <- create_input(test_data, best_lag)
test_input <- as.matrix(test_input_data$input)  # Ensure data is in matrix form for prediction

# Retrieve the best model from your list
best_model <- models[["lag_1_nodes_10"]]

# Predict using the best model
predicted_values_best <- predict(best_model, test_input)

# Prepare actual values for comparison; adjust indices according to lag
# Correct the indices to match the number of predictions
actual_values_best <- test_data[(best_lag + 1):length(test_data)]

# Make sure the number of actual and predicted values are the same
if (length(actual_values_best) > length(predicted_values_best)) {
  actual_values_best <- actual_values_best[1:length(predicted_values_best)]
}

# Create a data frame for plotting Actual vs. Predicted
actual_vs_predicted_best <- data.frame(Actual = actual_values_best, Predicted = predicted_values_best)

# Plot Actual vs. Predicted Exchange Rates
ggplot(actual_vs_predicted_best, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  labs(x = "Actual Exchange Rate", y = "Predicted Exchange Rate", title = "Actual vs. Predicted Exchange Rates (Best Model)") +
  theme_minimal()





# Time Series Plot of Actual vs. Predicted Exchange Rates for the best model

# Predict using the best model on the test input data
predicted_values_test <- predict(best_model, as.matrix(test_input))

# Create a data frame for plotting Actual vs. Predicted on the test data
exchange_ts_test <- data.frame(
  Date = seq(as.Date("2020-01-01"), by = "day", length.out = length(predicted_values_test)),
  Actual = actual_values_best,  # Adjust variable name to match the test data
  Predicted = predicted_values_test  # Adjust variable name to match the test data
)

# Plot Actual vs. Predicted Exchange Rates for the test data
ggplot(exchange_ts_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed") +
  labs(x = "Date", y = "Exchange Rate", color = "Series") +
  ggtitle("Time Series Plot of Actual vs. Predicted Exchange Rates (Best Model - Test Data)")





# # Print test performance metrics
# test_rmse <- sapply(models, function(model) {
#   predicted_values <- predict(model, as.matrix(test_data))
#   sqrt(mean((predicted_values - test_data)^2))
# })
# 
# test_mae <- sapply(models, function(model) {
#   predicted_values <- predict(model, as.matrix(test_data))
#   mean(abs(predicted_values - test_data))
# })
# 
# test_mape <- sapply(models, function(model) {
#   predicted_values <- predict(model, as.matrix(test_data))
#   mean(abs((predicted_values - test_data) / test_data)) * 100
# })
# 
# test_smape <- sapply(models, function(model) {
#   predicted_values <- predict(model, as.matrix(test_data))
#   mean(200 * abs(predicted_values - test_data) / (abs(predicted_values) + abs(test_data)))
# })
# 
# # Print test performance metrics
# cat("Test RMSE:", test_rmse, "\n")
# cat("Test MAE:", test_mae, "\n")
# cat("Test MAPE:", test_mape, "\n")
# cat("Test sMAPE:", test_smape, "\n")
# 







