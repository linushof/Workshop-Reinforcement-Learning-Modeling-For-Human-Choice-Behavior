# Set initial variables
Narms <- 2
Nstates <- 2
Nblocks <- 4
Ntrials <- 1000

# Read data
data <- read.csv('data/stanmodel_two_step_task/artificial_data.csv')
choices1 <- as.numeric(data$choice1)
choices2 <- as.numeric(data$choice2)
states <- as.numeric(data$state)
rewards <- as.numeric(data$reward)

# Define state transition matrix
state_transition <- matrix(c(0.7, 0.3, 0.3, 0.7), nrow = 2, byrow = TRUE)

# Set other parameters
alpha <- 0.3
lambda_ <- 0.5
beta <- 3
omegas <- seq(0, 1, by = 0.1)

# For each parameter combination, get probability of choosing the action that was chosen
all_likelihoods <- numeric(length(omegas))

for (omega in omegas) {
  log_likelihood <- numeric()
  
  for (block in 1:Nblocks) {
    # Initialize Qmf values on each block
    Qmf1 <- rep(0.5, Narms) # stage 1 - only one state
    Qmf2 <- matrix(0.5, nrow = Nstates, ncol = Narms) # stage 2 - two states
    # Initialize Qmb values - only for first stage - second stage is identical to MF
    Qmb1 <- rep(0.5, Narms)
    
    for (trial in 1:Ntrials) {
      Qmb1[1] <- state_transition[1, 1] * max(Qmf2[1, ]) + state_transition[2, 1] * max(Qmf2[2, ])
      Qmb1[2] <- state_transition[1, 2] * max(Qmf2[1, ]) + state_transition[2, 2] * max(Qmf2[2, ])
      # Integrate MF and MB values modulated by omega parameter
      Qnet1 <- omega * Qmb1 + (1 - omega) * Qmf1
      # Player's choice probabilities
      p1 <- exp(beta * Qnet1) / sum(exp(beta * Qnet1))
      choice1 <- choices1[trial]
      log_likelihood <- c(log_likelihood, log(p1[choice1]))
      
      # Updating Q values
      reward <- rewards[trial]
      # Updating Qmf values
      state <- states[trial]
      choice2 <- choices2[trial]
      p2 <- exp(beta * Qmf2[state, ]) / sum(exp(beta * Qmf2[state, ]))
      # Log-likelihood for second choice (commented out in original)
      #log_likelihood <- c(log_likelihood, log(p2[choice2]))
      # Prediction errors
      PE2 <- reward - Qmf2[state, choice2] # Second stage reward-based prediction error
      PE1 <- Qmf2[state, choice2] - Qmf1[choice1] # First stage prediction error
      
      # Second stage update
      Qmf2[state, choice2] <- Qmf2[state, choice2] + alpha * PE2
      # First stage update with an eligibility trace for the second-stage prediction error
      Qmf1[choice1] <- Qmf1[choice1] + alpha * PE1 + lambda_ * alpha * PE2
    }
  }
  
  all_likelihoods[which(omegas == omega)] <- sum(log_likelihood)
}

#plot
library(ggplot2)

# Create a data frame for plotting
data <- data.frame(omegas = omegas, all_likelihoods = all_likelihoods)

# Create the scatter plot
ggplot(data, aes(x = omegas, y = all_likelihoods)) +
  geom_point() +
  labs(x = "Omegas", y = "Sum of Log Likelihoods") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal()
