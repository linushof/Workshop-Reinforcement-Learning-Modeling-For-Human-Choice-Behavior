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
Qval               = as.matrix(t(rep(0.5,____)))
colnames(Qval)     =sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
df                 =data.frame()

for (block in 1:Nblocks){
  
  Qval      = as.matrix(t(rep(0.5,Narms)))
  
  for (trial in 1:Ntrials){
    
    #players choice
    p         = exp(____*Qval) / sum(exp(____*Qval))
    choice    = sample(1:____,1,prob=p)
    
    #outcome 
    reward = sample(0:1,1,prob=c(1-expvalues[____,trial],expvalues[____,trial]))
    
    #save trial's data
    
    #create data for current trials
    dfnew=data.frame(
      block                = block,
      trial                = trial,
      choice               = choice,
      expval_ch            = expvalues[____,trial],
      reward               = reward
    )
    
    dfnew=cbind(dfnew,Qval)
    dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
    
    #bind to the overall df
    df=rbind(df,dfnew)
    #updating Qvalues
    Qval[choice] = ____ + ____*(reward - ____)
  }
}

save(df,file="./data/multiArmedSimulationData.Rdata")