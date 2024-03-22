library(glmnet)

# Elastic Net Soft-thresholding Function
soft_thresholding_enet <- function(x, lambda, alpha, n) {
  l1 <- lambda * alpha
  l2 <- lambda * (1 - alpha)
  sign(x) * pmax(abs(x) - l1, 0) / (1 + l2 / n)
}

# Elastic Net using Coordinate Descent
my_elastic_net <- function(X, y, lambda, alpha, tol = 1e-10, max_iter = 1000) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)
  iter <- 0
  change <- 1
  
  while (iter < max_iter & change > tol) {
    iter <- iter + 1
    old_beta <- beta
    for (j in 1:p) {
      X_j <- X[, j, drop = FALSE]
      rj <- y - X %*% beta + X_j * beta[j]
      rho_j <- crossprod(X_j, rj) / n
      beta[j] <- soft_thresholding_enet(rho_j, lambda, alpha, n)
    }
    change <- max(abs(beta - old_beta))
  }
  return(list(beta = beta, iter_no = iter))
}

# Load and standardize the data
X <- read.csv('A1Q1_X.csv')
y <- read.csv('A1Q1_y.csv')
X_std <- scale(X)
y_std <- scale(y)

# Parameters
lambda <- 0.1
alpha <- 0.6

# Compute Elastic Net solution
enet_result <- my_elastic_net(X_std, y_std, lambda, alpha)

# Compare with glmnet
fit <- glmnet(X_std, y_std, lambda = lambda, alpha = alpha)
glmnet_coef <- coef(fit, s = lambda)[-1,]

# Print results
print(enet_result$beta)
print(glmnet_coef)


# MY RESULTS
# [1]  0.3525655  0.3354405  0.3260599  0.3522440  0.3519175 -0.3400035  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
# [12]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
# [23]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000

# GLMNET RESULTS
# V1         V2         V3         V4         V5         V6         V7         V8         V9        V10        V11        V12 
# 0.3403921  0.3228509  0.3145462  0.3405747  0.3389152 -0.3281235  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000 
# V13        V14        V15        V16        V17        V18        V19        V20        V21        V22        V23        V24 
# 0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000 
# V25        V26        V27        V28        V29        V30 
# 0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000 
