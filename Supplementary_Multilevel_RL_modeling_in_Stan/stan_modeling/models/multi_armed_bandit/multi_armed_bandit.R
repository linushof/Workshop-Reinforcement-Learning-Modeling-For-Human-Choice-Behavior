#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  beta  = parameters['beta']

  
  #set initial var
  Narms              = 2 #cfg$Narms
  Nraffle            = 2 #cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials            = cfg$Ntrials
  expvalues          = cfg$rndwlk
  Qval               = as.matrix(t(rep(0.5,Narms)))
  colnames(Qval)     =sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
  df                 =data.frame()
  
for (block in 1:Nblocks){
  
  Qval      = as.matrix(t(rep(0.5,Narms)))
  
  for (trial in 1:Ntrials){

    #computer offer
    raffle    = sample(1:Narms,Nraffle,prob=rep(1/Narms,Narms)) 
    raffle    = sort(raffle)
    
    #players choice
    p         = exp(beta*Qval[raffle]) / sum(exp(beta*Qval[raffle]))
    choice    = sample(raffle,1,prob=p)
    unchosen  = raffle[choice!=raffle]
    
    #outcome 
    reward = sample(0:1,1,prob=c(1-expvalues[trial,choice],expvalues[trial,choice]))
    
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
            expval_ch            = expvalues[trial,choice],
            expval_unch          = expvalues[trial,raffle[choice!=raffle]],
            reward               = reward
            )
      
      dfnew=cbind(dfnew,Qval)
      dfnew=cbind(dfnew,expvalues[trial,])
      
      #bind to the overall df
      df=rbind(df,dfnew)
       
    
    
    #updating Qvalues
    Qval[choice] = Qval[choice] + alpha*(reward - Qval[choice])
  }
}     
  return (df)
}
