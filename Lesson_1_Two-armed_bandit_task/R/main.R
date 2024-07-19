#Shahar-Lab, MathPsych 2024

rm(list = ls())  


#### simulate agent ----

#set task variables 
source('./functions/generate_excpected_values.r')
Narms              = 2
Ntrials            = 1000
expvalues          = generate_excpected_values(Narms, Ntrials, range = c(0.2,0.8), start_values = c(0.4, 0.7), noise = 0.01)
matplot(t(expvalues), type = "l", lty = 1, col = 1:Ntrials, xlab = "trial", ylab = "excpected value")

#set parameters
alpha = 0.6
beta  = 3

#simulate behavior
source('./functions/simulate_multiarmed.r')
df = simulate_data(Narms, Ntrials, expvalues, alpha, beta)

#practice
#df = simulate_data_broken(Narms, Ntrials, expvalues)

save(df,file="./data/simulated_data.Rdata")



#### grid search ----
load("./data/simulated_data.Rdata")

source('./functions/like_multiarmed.r')
results <- data.frame()

for (alpha in seq(0.1, 0.9, by = 0.1)) {
      
      loglike = likelihood(Narms, Ntrials, df, alpha = alpha, beta = 3)
      
      #practice
      #loglike = likelihood_broken(Narms, Ntrials, df, alpha = alpha, beta = 5)
      
      results = rbind(results,
                      data.frame(
                          alpha = alpha,
                          beta   = 5,
                          loglike = loglike
                          )
      )
}
plot(results$alpha,results$loglike)


#### Bayesian estimation ----
library(cmdstanr)
library(ggplot2)
library(ggdist)
load("./data/simulated_data.Rdata")


# Prepare data for Stan
stan_data <- list(
  Ntrials = nrow(df),
  Narms = 2,
  choice = df$choice,
  reward = df$reward
)

#compile stan
model <- cmdstan_model('./functions/twoarmed.stan')
model <- cmdstan_model('./functions/twoarmed_broken.stan')

#sample posterior
fit <- model$sample(
          data            = stan_data, 
          iter_sampling   = 2000,
          iter_warmup     = 1000,
          chains          = 2,
          parallel_chains = 2
          )  

# Print the results
print(fit)
samples = fit$draws(format = "df")

ggplot(samples, aes(x = alpha)) +
  stat_halfeye() + 
  theme_minimal() + xlim(0,1)

ggplot(samples, aes(x = beta)) +
  stat_halfeye() + 
  theme_minimal() + xlim(0,10)
