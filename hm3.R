# Set up parameters
pj <- 1:10
probabilities <- c(0.05, 0.05, 0.05, 0.05, 0.05, 0.15, 0.15, 0.15, 0.15, 0.15)
Qj <- rep(0.1, 10)

# Threshold constant
c <- 4
# Function to perform acceptance-rejection simulation
acceptance_rejection_simulation <- function(num_simulations) {
  accepted_values <- c()
  
  for (i in 1:num_simulations) {
    while (TRUE) {
      #Generate U
      U <- runif(1)
      
      # STransform U to variable y
      y <- as.integer(10 * U) + 1
      
      # Step 3: Compute the ratio 
      ratio <- probabilities[y] / Qj[y]
      
      # Step 4: Generate another uniform 
      V <- runif(1)
      
      # Step 5: Accept or reject
      threshold <- ratio / c
      if (V <= threshold) {
        accepted_values <- c(accepted_values, y)
        break
      }
    }
  }
  return(accepted_values)
}

# Perform simulation 1000 times
simulated_values <- acceptance_rejection_simulation(1000)

# Plot histogram
hist(simulated_values, breaks = 0:10, main = "Histogram of Simulated Values", xlab = "Value", ylab = "Frequency", col = "skyblue", xlim = c(0.5, 10.5))
axis(side = 1, at = 1:10, labels = 1:10)
