#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  lambda = parameters['lambda']
  beta  = parameters['beta']
  omega  = parameters['omega']

  
  #set initial var
  Nstates            = cfg$Nstates #2 states in second stage
  Narms              = cfg$Narms #2 offered options in each state
  Nblocks            = cfg$Nblocks 
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  state_transition   = matrix(c(0.7,0.3,0.3,0.7),ncol=2)
  df                 = data.frame()
  
for (block in 1:Nblocks){
  
  #initialize Qmf values on each block
  Qmf1               =  #stage 1 - only one state
  Qmf2               =  #stage 2 - two states
  
  #initialize Qmb values - only for first stage - second stage is identical to MF
  Qmb1               =  ###
  
  for (trial in 1:Ntrials_perblock){

    #define first stage Qmb values based on MF values of second stage and state transitions.
    Qmb1[1] = state_transition[]*max(Qmf2[])+state_transition[]*max(Qmf2[])
    Qmb1[2] = state_transition[]*max(Qmf2[])+state_transition[]*max(Qmf2[])
    
    #integrate MF and MB values modulated by omega parameter
    Qnet1     = omega*###+(1-omega)*###
    
    #make first-stage choice
    p1        = exp(beta1*Qnet1) / ###fill this
    ###### add line
    unchosen1 = c(1:Narms)[-choice1]
    
    #sample state transition based on first stage choice
    state=sample(1:Nstates,1,prob=)
    state_prob = state_transition[]
    
    #second stage choice
    p2        = ### / sum(exp(beta2*Qmf2[state,]))
    choice2   = sample(1:Narms,1,prob=p2)
    unchosen2 = c(1:Narms)[-choice2]
    
    #sample outcome
    if(state==1){
      expval_ch = ###
      expval_unch = ###
    }
    else{
      expval_ch = ### #for state B, it is 3rd and 4th row of expvalues csv 
      expval_unch = ###
    
    }
    reward = sample(0:1,1,prob=###)
    
    #save trial's data
    
      #create data for current trials
      dfnew=data.frame(
            subject              = subject,
            block                = block,
            trial                = trial,
            first_trial_in_block = (trial==1)*1,
            choice1              = choice1,
            state                = state,
            state_prob           = state_prob,
            choice2              = choice2,
            expval_ch            = expval_ch,
            expval_unch          = expval_unch,
            reward               = reward
            )

      
       #bind to the overall df
       df=rbind(df,dfnew)
       
    #updating Qmf values
    
    #Prediction erros
    PE2 = reward - ### #second stage reward-based prediction error
    PE1 = ### -Qmf1[choice1] #first stage prediction error
    
    #second stage update
    Qmf2[state,choice2]=Qmf2[state,choice2]+###
    #first stage update with an eligibility trace for the second-stage prediction error
    Qmf1[choice1] = Qmf1[choice1] +###

  }
}     
  return (df)
}