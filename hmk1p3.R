# Define the density function h(x)
h <- function(y) {
  return(30 * (y^2 - 2 * y^3 + y^4))
}

# Define the calculated constant c
c <- 1.875

# Generate random variable using rejection-acceptance method
generate_X <- function(n) {
  X <- numeric(n)
  count_accept <- 0
  
  while (count_accept < n) {
    y <- runif(1)  # Generate a uniform random variable y
    
    # Generate a uniform random variable u2 for acceptance-rejection
    u2 <- runif(1)
    
    # Check acceptance condition
    if (u2 <= h(y)/c) {
      X[count_accept + 1] <- y
      count_accept <- count_accept + 1
    }
  }
  
  return(X)
}

# Simulate 1000 samples
simulations <- generate_X(1000)

# Create density plot
hist(simulations, breaks = 30, col='skyblue', freq = FALSE, main = "Simulated Distribution of X", xlab = "X", ylab = "Density")
curve(dnorm(x, mean(simulations), sd(simulations)), col = "red", lwd = 2, add = TRUE, yaxt = "n")
