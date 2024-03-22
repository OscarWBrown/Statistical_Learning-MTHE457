library(glmnet)
library(readr)

# Load the datasets
X <- read_csv("crime_X.csv")
y <- read_csv("crime_y.csv")

# Prepare the data
X_matrix <- as.matrix(X)
y_vector <- y$x  # Correcting the access to the y column based on your file structure

# Fit Lasso model using cross-validation to select lambda
set.seed(123) # For reproducibility
cv_fit <- cv.glmnet(X_matrix, y_vector, alpha = 1)

# Best lambda
best_lambda <- cv_fit$lambda.min

# Fit model using the best lambda
lasso_model <- glmnet(X_matrix, y_vector, alpha = 1, lambda = best_lambda)

# Identify non-zero coefficients (selected variables)
selected_variables <- coef(lasso_model, s = best_lambda)[,1]

# Filter out zero coefficients
selected_variables <- selected_variables[selected_variables != 0]

# Print selected variables and their coefficients
print(selected_variables)

# Output
# (Intercept)        racePctWhite          pctWInvInc       PctUnemployed         PctEmplManu         TotalPctDiv 
# 8.231608e-01       -2.379550e-01       -1.579734e-01        3.997975e-03       -5.753988e-02        6.363810e-02 
# PctFam2Par         PctKids2Par            PctIlleg      PctHousLess3BR        PctHousOccup    PctVacantBoarded 
# -2.080818e-01       -3.392593e-01        5.177848e-05        4.778221e-02       -1.719584e-02        3.924570e-03 
# NumStreet    PolicReqPerOffic           PolicCars LemasGangUnitDeploy 
# 1.591734e-01        3.901968e-02        4.546114e-02        1.212495e-02 
