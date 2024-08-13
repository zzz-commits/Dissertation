# Required libraries
library(stats)
library(dplyr)
library(tibble)  


p0 <- 1  # Base price used in the F_inv function, acts as a reference price

# Function to calculate the inverse relationship based on the provided parameters
F_inv <- function(p0, a, theta) {
  return((p0 - a) / p0) # Returns the normalized difference, essentially a scaling by p0
}



# Simulates demand with uncertainty where demand decreases with increasing price p, adjusted by random noise
d_theta <- function(p, sigma) {
  epsilon <- rnorm(1, mean = 0, sd = sigma) # Random noise representing demand fluctuations
  return((theta0 / (theta1 + p)) + epsilon)  # Demand function considering price sensitivity and noise
}

# Returns the ratio of two parameters, theta0 and theta1
H_theta <- function(theta0, theta1) {
  return(theta0 / theta1) # Used to determine a component in price setting
}
# Computes the optimal action (a) by maximizing an objective function under competition settings
optimal_a <- function(Ct, p0, theta0, theta1) {
  a_values <- seq(0, p0, by = 0.01) # Candidate a values from 0 to p0
  scores <- sapply(a_values, function(a) { # Evaluate each candidate
    val <- (2 * a) / (H_theta(theta0, theta1) + a)
    score <- if (val < 1) { # Calculate part of the score function
      F_inv(p0, a, theta0) * log(1 - val) # Use logarithmic scoring if conditions are met
    } else {
    } else { 
      -Inf  # Set score to negative infinity if val exceeds 1
    }
    print(paste("a:", a, "score:", score))  
    return(score)
  })
  best_a <- a_values[which.max(scores)]  # Select the a value that maximizes the score
  return(best_a) # Return the optimal a
}



T <- 100  
# Create a tibble to store results
results <- tibble(a = numeric(T), p = numeric(T), b = numeric(T), G_A = numeric(T), G_B = numeric(T))

for (t in 1:T) {
  Ct <- runif(1, min = 0, max = 1)# Randomly generate a cost parameter
  theta0 <- 1  
  theta1 <- 2  
  sigma <- 0.5  
  
  a <- optimal_a(Ct, p0, theta0, theta1)
  p <- (H_theta(theta0, theta1) + a) / 2
  b <- F_inv(p0, a, theta0)  
  
  d_t <- d_theta(p, sigma)  # Compute demand with noise
  G_A <- a * b  # Compute gain for agent A
  G_B <- p * min(b, d_t) - a * b # Compute gain for agent B based on the min demand
  
  results[t, ] <- list(a, p, b, G_A, G_B)
}





# alternative scenario


d <- function(p, sigma) {
  epsilon <- rnorm(1, mean = 0, sd = sigma)
  theta0 / (theta1 + p) + epsilon
}


optimize_pricing <- function(theta0, theta1, sigma, T) {
  results <- tibble(iteration = integer(T), f = numeric(T), c = numeric(T), b = numeric(T), p = numeric(T), G_A = numeric(T), G_B = numeric(T))
  
  for (t in 1:T) {
    
    f_vals <- seq(0, 100, by = 10)
    c_vals <- seq(0.1, 10, by = 0.1)
    
    best_profit <- -Inf
    best_f <- 0
    best_c <- 0
    
    for (f in f_vals) {
      for (c in c_vals) {

        p <- optimize_retail_price(c, theta0, theta1)  # Function to determine optimal p
        b <- optimize_order_quantity(p, c, sigma, theta0, theta1)  # Function to determine optimal b
        

        d_t <- d(p, sigma)
        G_A <- f + c * b
        G_B <- p * min(d_t, b) - c * b - f
        
        if (G_A > best_profit) {
          best_profit <- G_A
          best_f <- f
          best_c <- c
          results[t, ] <- list(t, f, c, b, p, G_A, G_B)
        }
      }
    }
  }
  
  return(results)
}
