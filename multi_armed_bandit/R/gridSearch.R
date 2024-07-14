rm(list = ls())

library(ggplot2)
library(reshape2)

# Set initial variables
Narms <- 2
Nblocks <- 4
Ntrials <- 100
data <- read.csv('./data/multiArmedSimulationData.csv')
choices <- data$choice
rewards <- data$reward
alphas <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
betas <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# For each parameter combination, get the probability of choosing the action that was chosen
all_likelihoods <- numeric(length(alphas) * length(betas))

index <- 1
for (alpha in alphas) {
  for (beta in betas) {
    log_likelihood <- numeric(Nblocks * Ntrials)
    
    for (block in 1:Nblocks) {
      Qval <- rep(0.5, Narms)
      
      for (trial in 1:Ntrials) {
        # Player's choice probabilities
        p <- exp(beta * Qval) / sum(exp(beta * Qval))
        choice <- choices[trial]
        log_likelihood[(block - 1) * Ntrials + trial] <- log(p[choice])
        
        # Updating Q-values
        reward <- rewards[trial]
        Qval[choice] <- Qval[choice] + alpha * (reward - Qval[choice])
      }
    }
    all_likelihoods[index] <- sum(log_likelihood)
    index <- index + 1
  }
}

# Reshape results into a matrix
prob_matrix <- matrix(all_likelihoods, nrow = length(alphas), ncol = length(betas), byrow = TRUE)

# Convert the matrix to a data frame for plotting
df <- melt(prob_matrix)
colnames(df) <- c("alpha", "beta", "log_likelihood")
df$alpha <- factor(df$alpha, labels = alphas)
df$beta <- factor(df$beta, labels = betas)

# Plot results as a heatmap
ggplot(df, aes(x = alpha, y = beta, fill = log_likelihood)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Log likelihood of alpha and beta parameter combinations",
       x = "alpha", y = "beta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
