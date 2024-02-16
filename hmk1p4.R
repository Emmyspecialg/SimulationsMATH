# Set the seed for reproducibility
set.seed(123)

# Number of simulations
n <- 1000

# Generate n uniform random numbers between 0 and 1
U <- runif(n)

# Calculate X using the given formula
X <- log(U * (exp(1) - 1) + 1)

# Plot the histogram of X
hist(X, breaks = 20, col = "skyblue", main = "Histogram of X", xlab = "X")

