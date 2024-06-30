#simulate beta distribution with mean, sd to alpha,beta and kappa
# Number of individuals
N <- 1000  

# Number of observations per individual
trials <- 200
# Desired mean and standard deviation
mean = 0.5
sd = 0.2
kappa = (mean*(1-mean)/sd^2)-1

# Calculate alpha and beta from mean and sd
alpha = mean * ((mean * (1 - mean) / sd^2) - 1)
beta = (1 - mean) * ((mean * (1 - mean) / sd^2) - 1)
sd=sqrt(mus*(1 - mus)/(kappas+1))
# Generate random numbers from the beta distribution
random_numbers = rbeta(N, alpha, beta)
