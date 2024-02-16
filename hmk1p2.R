# homework1 problem2
# Setting seed
set.seed(125)

# Number of simulations
n <- 1000

# Generating n uniform random number between 0 and 1
U <- runif(n)

# Calculate X using the given formula
X <- 6 * floor(5 * U) + 7

#histogram of X
hist(proportions, breaks = 20, col = "skyblue", main = "Histogram of X", xlab = "X")

