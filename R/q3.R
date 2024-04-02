# Install and load the 'titanic' package
install.packages("titanic")
library(titanic)

# Load the Titanic dataset
data("titanic_train")
titanic <- as.data.frame(titanic_train)

# Display the structure of the dataset
str(titanic)

install.packages("randomForest")
install.packages("caret")

library(randomForest)
library(caret)
library(ggplot2)

# Removing the unnecessary columns
titanic <- titanic[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Survived")]

# Handling the missing values
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
titanic$Embarked[is.na(titanic$Embarked)] <- "S"

# Convert categorical variables to factors
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# Display the first few rows of the preprocessed dataset
head(titanic)

# Using caret package for feature selection
set.seed(123)
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
result <- rfe(titanic[, -8], titanic$Survived, sizes = c(1:7), rfeControl = ctrl)

# Display feature selection results
print(result)

# Train the Random Forest model
set.seed(123)
model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = titanic, ntree = 500)

# Display model summary
print(model)

# Plot variable importance
varImpPlot(model)

# Plot confusion matrix
confusionMatrix(predict(model, titanic), titanic$Survived)

# Plot survival distribution based on age
px <- ggplot(titanic, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Survival Distribution Based on Age",
       x = "Age",
       y = "Count") +
  theme_minimal()

# Save the plot as an image file
ggsave("survival_distribution_plot.png", px, width = 6, height = 4, dpi = 300)

# Display the plot
print(px)
