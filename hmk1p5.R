# h(x)
h <- function(y) {
  return(3/sqrt(pi) * y^(1/2) * exp(-y/3))
}

# Generate random variable using rejection acceptance method
generate_X <- function(n) {
  X <- numeric(n)
  count_accept <- 0
  
  while (count_accept < n) {
    y <- -3/2 * log(1 - runif(1))
    u2 <- runif(1)
    if (u2 <= h(y)/1.275) {
      X[count_accept + 1] <- y
      count_accept <- count_accept + 1
    }
  }
  
  return(X)
}

# Simulate 1000 samples
simulations <- generate_X(1000)

# Create bar plot
hist(simulations, breaks = 30, col = 'skyblue', freq = FALSE, main = "Simulated Distribution of X", xlab = "X", ylab = "Density")

# Overlay the density function
curve(f(x), add = TRUE, col = "red", lwd = 2, n = 1000, from = 0)

