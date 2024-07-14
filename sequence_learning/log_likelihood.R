# Simulation code for sequence learning
rm(list=ls())

#pre-allocation
subject = 1
rndwlk = read.csv('sequence_learning/sequence_learning_simulations/rndwlk_depth3_100trials.csv',header=F)

load('sequence_learning/data/simulated_data.Rdata')

Narms              = 2 
Nstages            = 3 
Nblocks            = 1 
Nstates            = 8 
Ntrials_perblock   = 100 
expvalues          = rndwlk
rownames(expvalues)= c('ev1','ev2','ev3','ev4','ev5','ev6','ev7','ev8')
Qval               = as.matrix(t(rep(0.5,Narms)))
colnames(Qval)     = sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})


alpha = 0.5 
beta = 3 
lambda = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

log_likelihood_1 = array(0, 9)
log_likelihood_2 = array(0, 9)
log_likelihood_3 = array(0, 9)


for (l in lambda) {
  
  Qval = array(0.5, dim = c(Narms, Nstates, Nstages))
  
  log_p1 = array(0, dim = c(Ntrials_perblock))
  log_p2 = array(0, dim = c(Ntrials_perblock))
  log_p3 = array(0, dim = c(Ntrials_perblock))
  

  for (t in 1:Ntrials_perblock){
    state1  = 1
    p_1     = exp(beta*Qval[, state1, 1]) / sum(exp(beta*Qval[, state1, 1]))
    choice1 = as.numeric(df[t,"choice1"])
    p_choice1 = p_1[choice1]
    log_p1[t][1] = log(p_choice1)
    
    state2  = choice1
    p_2     = exp(beta*Qval[, state2, 2]) / sum(exp(beta*Qval[, state2, 2]))
    choice2 = as.numeric(df[t,"choice2"])
    p_choice2 = p_2[choice2]
    log_p2[t][1] = log(p_choice2)
    
    
    state3  = -2 + 2*choice1 + choice2
    p_3       = exp(beta*Qval[, state3, 3]) / sum(exp(beta*Qval[, state3, 3]))
    choice3 = as.numeric(df[t,"choice3"])
    p_choice3 = p_3[choice3]
    log_p3[t][1] = log(p_choice3)
    
    
    #simulate outcome
    reward = as.numeric(df[t,"reward"])
    
    #update Qvalues
    PE_1 = Qval[choice2, state2, 2] - Qval[choice1, state1, 1]
    PE_2 = Qval[choice3, state3, 3] - Qval[choice2, state2, 2]
    PE_3 = reward - Qval[choice3, state3, 3]
    
    Qval[choice1, state1, 1] = Qval[choice1, state1, 1] + 
      alpha*PE_1 +
      alpha*l*PE_2 +
      alpha*(l**2)*PE_3
    
    Qval[choice2, state2, 2] = Qval[choice2, state2, 2] + 
      alpha*PE_2 + 
      alpha*l*PE_3
    
    Qval[choice3, state3, 3] = Qval[choice3, state3, 3] + alpha*PE_3
    
    
  }

  
  log_likelihood_1[l*10] = sum(log_p1)
  log_likelihood_2[l*10] = sum(log_p2)
  log_likelihood_3[l*10] = sum(log_p3)

}

log_likelihood = log_likelihood_1 + log_likelihood_2 + log_likelihood_3
which(log_likelihood == max(log_likelihood), arr.ind = TRUE)
plot(log_likelihood)

