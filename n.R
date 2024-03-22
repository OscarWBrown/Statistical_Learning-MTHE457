library(glmnet)
library(leaps) # For subset selection, may not scale well for large 'p'

# set.seed(42) # Ensure reproducibility

# Generate data
n <- 500
p <- 20
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- 1 + 0.2 * rowSums(X[, 1:5]) + rnorm(n, 0, 0.1)

# Best Subset Selection (conceptual, may not be feasible for large 'p')
subset_fit <- regsubsets(X, y, nbest = 1)
subset_selection <- summary(subset_fit)$which[which.min(summary(subset_fit)$cp), ]

# LASSO with cross-validation
cv_lasso <- cv.glmnet(X, y, alpha = 1)
lambda_min <- cv_lasso$lambda.min
lambda_1se <- cv_lasso$lambda.1se

# Fit LASSO model using lambda_min and lambda_1se
lasso_min <- glmnet(X, y, alpha = 1, lambda = lambda_min)
lasso_1se <- glmnet(X, y, alpha = 1, lambda = lambda_1se)

# Evaluate model selection ability
evaluate_selection <- function(model_coef) {
  # Assuming the model includes the intercept, adjust indices by 1
  active_predictors <- which(model_coef != 0)[-1]
  correct_identification <- all(active_predictors %in% 1:5) && length(active_predictors) == 5
  return(correct_identification)
}

# Evaluate
correct_min <- evaluate_selection(coef(lasso_min))
correct_1se <- evaluate_selection(coef(lasso_1se))
correct_subsel <- evaluate_selection(coef(subset_selection))

# Results
print(paste("LASSO with lambda_min correctly identifies the true model:", correct_min))
print(paste("LASSO with lambda_1se correctly identifies the true model:", correct_1se))

