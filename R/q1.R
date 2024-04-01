library(ggplot2)

library(ggplot2)

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

# Create a data frame with x, y, and predictions
plot_data <- data.frame(x = x, y = y, predictions = predictions)

# Create a scatter plot with points for data and the regression line
px <- ggplot(data = plot_data, aes(x = x, y = y)) +
  geom_point(color = "black") +  # Data points
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +  # Regression line
  geom_point(aes(y = predictions), color = "blue", shape = 2) +  # Predicted values
  labs(x = "X", y = "Y", title = "Linear Regression") +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  scale_color_manual(name = "Legend",
                     values = c("black", "red", "blue"),
                     labels = c("Data Points", "Regression Line", "Predicted Values")) +
  guides(color = guide_legend(override.aes = list(linetype = c(NA, 1, NA), shape = c(1, NA, 2))))

# Save the plot as an image file
ggsave("linear_regression_plot.png", px, width = 6, height = 4, dpi = 300)

# Display the plot
print(px)


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
