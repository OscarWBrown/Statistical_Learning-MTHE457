# Define the function
f <- function(x) {
  (x - 1)^6 + (x - 3)^4 + (x - 5)^2
}

# Define the first derivative of the function
f_prime <- function(x) {
  6*(x - 1)^5 + 4*(x - 3)^3 + 2*(x - 5)
}

# Define the second derivative of the function
f_double_prime <- function(x) {
  30*(x - 1)^4 + 12*(x - 3)^2 + 2
}

# Newton-Raphson Algorithm
x0 <- 0
tolerance <- 10^-12
max_iter <- 1000 # Maximum number of iterations to prevent infinite loops
iter <- 0

while(iter < max_iter) {
  x1 <- x0 - f_prime(x0) / f_double_prime(x0)
  if (abs(f_prime(x1)) < tolerance) {
    break
  } else {
    x0 <- x1
    iter <- iter + 1
  }
}

# Display the results
cat("Minimizer (x*):", x1, "\n")
cat("Derivative value at x* (|f'(x*)|):", abs(f_prime(x1)), "\n")

# Minimizer (x*): 2.082612
# Derivative value at x* (|f'(x*)|): 1.29674e-13 