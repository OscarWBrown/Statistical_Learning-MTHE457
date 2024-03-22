# Load necessary libraries
library(ISLR2)
library(MASS) # For lda()

# Load the Weekly data
weekly <- read.csv("/Users/oscar/Desktop/MTHE 457/Weekly.csv")

# Prepare the data
training <- weekly[weekly$Year < 2009, ]
test <- weekly[weekly$Year >= 2009, ]

# Fit the LDA model
model <- lda(Direction ~ Lag2, data=training)

# Make predictions
predictions <- predict(model, newdata=test)
predicted_class <- predictions$class

# Compute the confusion matrix and the overall fraction of correct predictions
conf_matrix <- table(Predicted=predicted_class, Actual=test$Direction)
correct_fraction <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Display the results
print(conf_matrix)
print(correct_fraction)

# Confusion Matrix: 
#           Actual
# Predicted Down Up
# Down      9  5
# Up        34 56
# Overall Fraction of Correct Predictions: [paste the fraction here]