#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  #browser()
#pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  beta  = parameters['beta']
  c     = parameters['c']

  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  rownames(expvalues)= c('ev1','ev2','ev3','ev4')
  df                 = data.frame()
  
for (block in 1:Nblocks){
  
  Qval      = as.matrix(t(rep(0,Narms)))
  Nvisit    = as.matrix(t(rep(0,Narms)))
  colnames(Qval)     = sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
  colnames(Nvisit)   = sapply(1:Narms, function(n) {paste('Nvisit',n,sep="")})
  
  
  for (trial in 1:Ntrials_perblock){

    #computer offer
    raffle    = sample(1:Narms,Nraffle,prob=rep(1/Narms,Narms)) 
    raffle    = sort(raffle)
    
    #integrate ucb
    ucb_values = sqrt(log(trial) / (Nvisit + 1))
    Qnet       = Qval + c * ucb_values 
    #players choice
    p          = exp( beta * Qnet[raffle]) / sum( exp( beta * Qnet[raffle]))
    choice     = sample(raffle,1,prob=p)
    unchosen   = raffle[choice!=raffle]
    
    #outcome 
    reward = sample(0:1,1,prob=c(1-expvalues[choice,trial],expvalues[choice,trial]))
    
    
    #save trial's data
    
      #create data for current trials
      dfnew=data.frame(
            subject              = subject,
            block                = block,
            trial                = trial,
            first_trial_in_block = (trial==1)*1,
            choice               = choice,
            selected_offer       = (choice==raffle[2])*1,
            unchosen             = unchosen,
            offer1               = raffle[1],
            offer2               = raffle[2],
            Nvisit_offer1        = Nvisit[raffle[1]],
            Nvisit_offer2        = Nvisit[raffle[2]],
            ucb_offer1           = ucb_values[raffle[1]],
            ucb_offer2           = ucb_values[raffle[2]],
            expval_ch            = expvalues[choice,trial],
            expval_unch          = expvalues[raffle[choice!=raffle],trial],
            reward               = reward
            )
      
      dfnew=cbind(dfnew,Qval)
      dfnew=cbind(dfnew,Nvisit)
      dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
      
      #bind to the overall df
      df=rbind(df,dfnew)
       
    
    
    #updating Qvalues
    Qval[choice] = Qval[choice] + alpha*(reward - Qval[choice])
    
    #update visits
    Nvisit[choice] <- Nvisit[choice] + 1
  }
}     
  return (df)
}