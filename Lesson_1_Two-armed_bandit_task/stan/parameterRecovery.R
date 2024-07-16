# Load necessary libraries
library(rstan)

# Load the simulated data
load("./data/multiArmedSimulationData.Rdata")

# Prepare data for Stan
stan_data <- list(
  Nblocks = max(df$block),
  Ntrials = nrow(df),
  Narms = 2,
  choice = df$choice,
  reward = df$reward
)

# Fit the model
fit <- stan(
  file = "multiArmed.stan",
  data = stan_data,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 123
)

# Print the results
print(fit)
plot(fit)
