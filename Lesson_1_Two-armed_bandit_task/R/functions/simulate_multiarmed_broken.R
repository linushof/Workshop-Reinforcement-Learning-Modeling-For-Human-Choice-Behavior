simulate_data <- function(Narms, Ntrials, expvalues, alpha, beta){

  df                 = data.frame()
  
  #starting values for Qvalues 
  Qvalues            = as.matrix(t(rep(0.5,Narms)))
  colnames(Qvalues)  = sapply(1:Narms, function(n) {paste('Qvalue',n,sep="")})

    for (trial in 1:Ntrials){
      
      #players choice
      p         = ____________
      choice    = ____________
      
      #outcome 
      reward = sample(0:1, 1 , prob = c( 1 - expvalues[choice,trial], expvalues[choice,trial]))
      
      #save trial's data
      df = rbind(df, 
          data.frame(
              trial       = trial,
              choice      = choice,
              reward      = reward,
              Qvalues     = Qvalues,
              expval      = t(t(expvalues)[trial,]),
              acc         = as.numeric((expvalues[choice,trial] - expvalues[3 - choice,trial]) > 0))
          )
      
      
      #updating Qvalues
      Qvalues[choice] = _______________________
  
  }

return(df)
  
}