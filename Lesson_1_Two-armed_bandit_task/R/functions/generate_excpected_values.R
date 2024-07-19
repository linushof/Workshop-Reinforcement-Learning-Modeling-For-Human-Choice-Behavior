
generate_excpected_values <- function(Narms , Ntrials , range = c(0.1, 0.9) , start_values = c(0.2, 0.9) , noise = 0.1) {
  
  #initiate matrix
  expvalues <- matrix(runif(Narms * Ntrials), nrow = Narms, ncol = Ntrials)
  
  #assign first values 
  expvalues[, 1] = start_values
  
  #simulate random walk values
  for (trial in 2:Ntrials) {
    
    #random walk
    expvalues[, trial] <- expvalues[, trial - 1] + rnorm(Narms,0,noise)
    expvalues[, trial] <- pmin(pmax(expvalues[, trial], range[1]), range[2])
  
  }
  
  return(expvalues)
}
