data {
  // General fixed parameters for the experiment/models
  int<lower=1> Ntrials;
  int<lower=2> Narms;

  // Behavioral data
  array[Ntrials] int<lower=1, upper=Narms> choice;
  array[Ntrials] int<lower=0, upper=1> reward;
}

parameters {
  real<lower=0, upper=1> alpha;
  real beta;
}

model {
  
  // Internal variables
  array[Narms] real Qval = rep_array(0.5, Narms);
  real Qdiff;
  real PE;
  
  // Priors for individual level parameters
  alpha ~ beta(1, 1);
  beta  ~ normal(0, 5);
  
  // Likelihood estimation
  for (trial in 1:Ntrials) {
    
    // Calculate Q-value difference and probability of choosing the action
    Qdiff = Qval[2] - Qval[1];
      target += bernoulli_logit_lpmf(choice[trial]-1 | beta * Qdiff);    
    // Update Q-values based on the chosen action
    PE = reward[trial] - Qval[choice[trial]];
    Qval[choice[trial]] += alpha * PE;
  }
}
