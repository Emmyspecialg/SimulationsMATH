# homework1 problem1
#Values and probabilities
values <- c(1, 2, 3, 4)
probabilities <- c(0.25, 0.5, 0.1, 0.15)

# Calculate cumulative distribution function (CDF) values
cdf_values <- cumsum(probabilities)

# Number of simulations
num_simulations <- 1000

# Function to simulate random variable X using inverse transform technique
simulate_inverse_transform <- function(random_value, cdf_values, values) {
  category <- sum(random_value > cdf_values)
  return(values[category + 1])
}

# Perform simulations
simulated_values <- sapply(runif(num_simulations), simulate_inverse_transform, cdf_values, values)

# Calculate proportions
proportions <- table(simulated_values) / num_simulations

# Bar graph
barplot(proportions, names.arg = as.character(values), col = "blue", 
        main = 'Distribution of Random Variable X', xlab = 'X', ylab = 'Proportion')


# Function to simulate rand X
simulate_X <- function(n) {
  # generate uniform random number 
  rn <- runif(n)
  
  # Initialize counts 
  counts <- c(0, 0, 0, 0)
  
  # Counting occurrences of each outcome
  for (i in 1:n) {
    if (rn[i] < 0.25) {
      counts[1] <- counts[1] + 1
    } else if (rn[i] < 0.75) {
      counts[2] <- counts[2] + 1
    } else if (rn[i] < 0.85) {
      counts[3] <- counts[3] + 1
    } else {
      counts[4] <- counts[4] + 1
    }
  }
  
  # Calculatingg proportions
  proportions <- counts / n
  
  # Returning proportions
  return(proportions)
}

# Simulate 1000 samples
simulations <- simulate_X(1000)

#plot 
barplot(simulations, names.arg = c(1, 2, 3, 4), col='skyblue', xlab = "Outcome", ylab = "Proportion", main = "Simulated Distribution of X")

