simulate_data <- function(Narms, Ntrials, expvalues, alpha, beta){

  df                 = data.frame()
  
  #starting values for Qvalues 
  Qvalues            = as.matrix(t(rep(0.5,Narms)))
  colnames(Qvalues)  = sapply(1:Narms, function(n) {paste('Qvalue',n,sep="")})

    for (trial in 1:Ntrials){
      
      #players choice
      p         = exp( beta * Qvalues ) / sum( exp( beta * Qvalues))
      choice    = sample( 1:Narms , 1 , prob = p)
      
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
      Qvalues[choice] = Qvalues[choice] + alpha*(reward - Qvalues[choice])
  
  }

return(df)
  
}