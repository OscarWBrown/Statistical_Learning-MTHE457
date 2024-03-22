library(glmnet)

# Soft-thresholding function
soft_threshold <- function(x, lambda) {
  sign(x) * pmax(abs(x) - lambda, 0)
}

# ADMM for LASSO
ADMM_lasso <- function(X, y, lambda, rho, iter_max = 1000, tol = 1e-10) {
  n <- ncol(X)
  m <- nrow(X)
  I <- diag(n)
  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  
  # Precompute matrices for efficiency
  M <- solve(XtX + rho * I)
  
  # Initialize beta, theta (z in some formulations), and dual variable mu (u in some formulations)
  beta <- rep(0, n)
  theta <- rep(0, n)
  mu <- rep(0, n)
  
  for(iter in 1:iter_max) {
    # Update beta
    beta <- M %*% (Xty + rho * (theta - mu))
    
    # Update theta with soft-thresholding
    theta_old <- theta
    theta <- soft_threshold(beta + mu, lambda / rho)
    
    # Update mu
    mu <- mu + (beta - theta)
    
    # Check for convergence
    primal_resid <- sqrt(sum((beta - theta)^2))
    dual_resid <- rho * sqrt(sum((theta - theta_old)^2))
    
    if(primal_resid < tol && dual_resid < tol) {
      break
    }
  }
  
  return(list(beta = beta, theta = theta, mu = mu, iterations = iter))
}

# Load and standardize the data
X <- as.matrix(read.csv('A1Q1_X.csv'))
y <- as.matrix(read.csv('A1Q1_y.csv'), ncol = 1)

# Ensure y is a vector if it's read as a matrix
if (is.matrix(y)) y <- y[, 1]

# Standardize X and y
X_std <- scale(X)
y_std <- scale(y)

# Parameters
lambda <- 0.1
rho <- 1 # This is a tuning parameter for ADMM and may need adjustment

# Run ADMM for LASSO
results <- ADMM_lasso(X_std, y_std, lambda, rho)

# Using glmnet for comparison
fit_glmnet <- glmnet(X_std, y_std, alpha = 1, lambda = lambda)
coef_glmnet <- coef(fit_glmnet, s = lambda)[-1, 1] # Exclude intercept

# Print results for comparison
cat("ADMM LASSO Beta Estimates:\n")
print(results$beta)

cat("\nGLMNet Beta Estimates:\n")
print(as.vector(coef_glmnet))

#ADMM LASSO Beta Estimates:

#V1   4.098941e-01
#V2   3.980578e-01
#V3   3.850206e-01
#V4   4.072069e-01
#V5   4.137334e-01
#V6  -3.982239e-01
#V7   1.469889e-05
#V8   1.695302e-04
#V9   2.708740e-05
#V10 -3.326075e-04
#V11  1.462521e-05
#V12 -3.999182e-04
#V13  6.473522e-04
#V14 -6.043617e-04
#V15  8.026522e-05
#V16 -6.152524e-04
#V17  1.182448e-04
#V18  9.766722e-06
#V19  2.355772e-04
#V20 -2.756477e-05
#V21  2.929639e-05
#V22  2.072984e-05
#V23  4.768715e-05
#V24 -1.083041e-05
#V25  5.728883e-04
#V26 -9.780197e-04
#V27 -1.656875e-05
#V28 -2.458039e-05
#V29 -9.264721e-04
#V30 -1.722651e-04

#GLMNet Beta Estimates:

#[1]  0.3154236  0.2948605  0.2878118  0.3166389  0.3119188 -0.3022167  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#[12]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#[23]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
