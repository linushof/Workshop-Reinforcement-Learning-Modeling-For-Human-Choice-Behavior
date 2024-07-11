rm(list = ls())  

generate_random_walk <- function(Narms = 4, Ntrials = 100, step_size = 0.01) {
  # Initialize the expected values to random values between 0 and 1
  expvalues <- matrix(runif(Narms * Ntrials), nrow = Narms, ncol = Ntrials)
  
  for (trial in 2:Ntrials) {
    # Generate random steps for each arm
    steps <- runif(Narms, min = -step_size, max = step_size)
    # Update the expected values
    expvalues[, trial] <- expvalues[, trial - 1] + steps
    # Ensure values remain between 0 and 1
    expvalues[, trial] <- pmin(pmax(expvalues[, trial], 0), 1)
    rownames(expvalues)=c('ev1','ev2')
  }
  
  return(expvalues)
}

#set parameters
alpha = 0.3
beta  = 1.5

#set initial var
Narms              = 2
Nblocks            = 4
Ntrials            = 100
expvalues          = generate_random_walk(Narms, Ntrials)
Qval               = as.matrix(t(rep(0.5,Narms)))
colnames(Qval)     =sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
df                 =data.frame()

for (block in 1:Nblocks){
  
  Qval      = as.matrix(t(rep(0.5,Narms)))
  
  for (trial in 1:Ntrials){
    
    #players choice
    p         = exp(beta*Qval) / sum(exp(beta*Qval))
    choice    = sample(1:Narms,1,prob=p)
    
    #outcome 
    reward = sample(0:1,1,prob=c(1-expvalues[choice,trial],expvalues[choice,trial]))
    
    #save trial's data
    
    #create data for current trials
    dfnew=data.frame(
      block                = block,
      trial                = trial,
      choice               = choice,
      expval_ch            = expvalues[choice,trial],
      reward               = reward
    )
    
    dfnew=cbind(dfnew,Qval)
    dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
    
    #bind to the overall df
    df=rbind(df,dfnew)
    #updating Qvalues
    Qval[choice] = Qval[choice] + alpha*(reward - Qval[choice])
  }
}

save(df,file="./data/multiArmedSimulationData.Rdata")

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
