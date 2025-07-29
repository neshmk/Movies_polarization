### MAIN POWER ANALYSES ###
install.packages("pwr")
library(pwr)
## REGRESSION POWER ANALYSIS (3 predictors & 1 continuous outcome) ##
# Define the parameters for the power analysis
effect_size <- 0.150   # Medium effect size equivalent to cohen's d = 0.3
alpha <- 0.05 #/ 4    # Bonferroni correction for 3 tests
power <- 0.80
num_predictors <- 3

# Power analysis
result <- pwr.f2.test(u = num_predictors, f2 = effect_size,
                      sig.level = alpha, power = power)

# Total sample size needed
total_n <- result$v + num_predictors + 1
total_n

################################################################################
# minimum number of ratings per movie
library(tidyverse)

# Define a true rating distribution (e.g., polarized)
true_dist <- c(rep(1, 30), rep(10, 30), rep(5, 40))  # 100 ratings: polarized

# Function to compute entropy
entropy <- function(ratings) {
  p <- table(ratings) / length(ratings)
  -sum(p * log(p), na.rm = TRUE)
}

# Define sample sizes to test
sample_sizes <- c(10, 20, 50, 100, 200, 500)

# Simulate entropy estimates at different sample sizes
set.seed(123)
sim_results <- map_dfr(sample_sizes, function(n) {
  entropies <- replicate(100, entropy(sample(true_dist, n, replace = TRUE)))
  tibble(
    sample_size = n,
    mean_entropy = mean(entropies),
    sd_entropy = sd(entropies)
  )
})

# Plot standard deviation of entropy vs. sample size
ggplot(sim_results, aes(x = sample_size, y = sd_entropy)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Entropy Stability by Sample Size",
    x = "Number of Ratings",
    y = "Standard Deviation of Entropy (100 runs)"
  ) +
  theme_minimal()
