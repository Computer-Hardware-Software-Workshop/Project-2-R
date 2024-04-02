library(ggplot2)
library(httr)

# Generate some sample data for binary classification
set.seed(123)
x <- 1:20
y <- ifelse(2 * x + rnorm(20, mean = 0, sd = 3) > 15, 1, 0)

sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

# Implement logistic regression function with gradient descent
logistic_regression <- function(x, y, learning_rate = 0.01, epochs = 1000) {
  n <- length(x)
  
  # Initialize coefficients
  beta0 <- 0
  beta1 <- 0
  
  # Sigmoid (logistic) function
  
  
  # Gradient descent
  for (epoch in 1:epochs) {
    # Calculate predicted probabilities
    probabilities <- sigmoid(beta0 + beta1 * x)
    
    # Update coefficients using gradient descent
    beta0 <- beta0 - learning_rate * sum(probabilities - y) / n
    beta1 <- beta1 - learning_rate * sum((probabilities - y) * x) / n
  }
  
  # Return the coefficients
  return(c(beta0, beta1))
}

# Fit the logistic regression model
coefficients <- logistic_regression(x, y)

# Generate a sequence of x values for plotting the sigmoid function
x_values <- seq(min(x), max(x), length.out = 100)
probabilities <- 1 / (1 + exp(-(coefficients[1] + coefficients[2] * x_values)))

# Combine data into a data frame
plot_data <- data.frame(x = x, y = y, predictions = probabilities)

# Plot using ggplot2
px <- ggplot(data = plot_data, aes(x = x, y = y)) +
  geom_point(color = "black") +  # Data points
  geom_line(aes(x = x_values, y = probabilities), color = "blue", size = 2) +  # Sigmoid function
  labs(x = "X", y = "Probability", title = "Logistic Regression Example") +
  theme_classic() +
  theme(text = element_text(size = 12))
px
# Save the plot as an image file
ggsave("logistic_regression_plot.png", px, width = 6, height = 4, dpi = 300)

# Display the plot
print(px)

# Print coefficients
cat("Intercept (beta0):", coefficients[1], "\n")
cat("Slope (beta1):", coefficients[2], "\n")
