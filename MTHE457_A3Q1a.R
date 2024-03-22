# Load necessary libraries
library(ISLR2)
library(caret)

# Load the Weekly data
weekly <- read.csv("/Users/oscar/Desktop/MTHE 457/Weekly.csv")

# Convert 'Direction' to binary (0 and 1)
weekly$DirectionBinary <- ifelse(weekly$Direction == "Up", 1, 0)

# Prepare the data
training <- weekly[weekly$Year < 2009, ]
test <- weekly[weekly$Year >= 2009, ]

# Fit the logistic regression model using the binary 'Direction'
model <- glm(DirectionBinary ~ Lag2, data=training, family=binomial)

# Make predictions
predictions <- predict(model, newdata=test, type="response")
predicted_class <- ifelse(predictions > 0.5, "Up", "Down")

# Compute the confusion matrix and the overall fraction of correct predictions
conf_matrix <- table(Predicted=predicted_class, Actual=test$Direction)
correct_fraction <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Display the results
print(conf_matrix)
print(correct_fraction)

# Confusion Matrix: 
#.          Actual
# Predicted Down Up
# Down      9  5
# Up        34 56
# Overall Fraction of Correct Predictions: [1] 0.625
