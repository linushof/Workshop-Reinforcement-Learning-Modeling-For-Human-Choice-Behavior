likelihood <- function(Narms, Ntrials, df, alpha , beta ){

  #initiate 
  Qvalues  = rep(0.5,Narms)
  like     = rep(NA,Ntrials)
  
  for (trial in 1:Ntrials){
    
      #empirical data    
      choice   = df$choice[trial]
      reward   = df$reward[trial]
      
      #players choice
      p           = exp( beta * Qvalues ) / sum( exp( beta * Qvalues))
      
      #likelihood of choice
      like[trial] = ________
      
      #updating Qvalues
      Qvalues[choice] = Qvalues[choice] + alpha*(reward - Qvalues[choice])
    
  }
  
return(sum(log(____)))  

}
