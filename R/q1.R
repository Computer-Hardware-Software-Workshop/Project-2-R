# Generate some sample data
set.seed(123)
x <- 1:20
y <- 2 * x + rnorm(20, mean = 0, sd = 3)

# Implement linear regression function
linear_regression <- function(x, y) {
  n <- length(x)
  
  # Calculate the mean of x and y
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  # Calculate the slope (beta1) and intercept (beta0)
  beta1 <- sum((x - mean_x) * (y - mean_y)) / sum((x - mean_x)^2)
  beta0 <- mean_y - beta1 * mean_x
  
  # Return the coefficients
  return(c(beta0, beta1))
}

# Fit the model
coefficients <- linear_regression(x, y)

# Make predictions
predictions <- coefficients[1] + coefficients[2] * x

# Plot the data and the regression line
plot(x, y, main = "Linear Regression", xlab = "X", ylab = "Y")
abline(coefficients[1], coefficients[2], col = "red")

# Add points for predicted values
points(x, predictions, col = "blue", pch = 2)

# Legend
legend("topleft", legend = c("Data Points", "Regression Line", "Predicted Values"),
       col = c("black", "red", "blue"), pch = c(1, NA, 2), lty = c(NA, 1, NA))

# Print coefficients
cat("Intercept (beta0):", coefficients[1], "\n")
cat("Slope (beta1):", coefficients[2], "\n")

# Calculate Mean Squared Error (MSE)
mse <- mean((y - predictions)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(y - predictions))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Calculate R-squared
ss_total <- sum((y - mean(y))^2)
ss_residual <- sum((y - predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)
cat("R-squared:", r_squared, "\n")
